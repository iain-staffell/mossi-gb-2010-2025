#####
## ##  PERSISTENT PLANTS AND MERIT ORDER DISPATCH
#####

### taken from fGarch -- random sqew normal
rsnorm = function(n, mean = 0, sd = 1, xi = 1) 
{
	weight = xi / (xi + 1/xi)
	z = runif(n, -weight, 1-weight)
	Xi = xi^sign(z)
	Random = -abs(rnorm(n))/Xi * sign(z)
	m1 = 2/sqrt(2 * pi)
	mu = m1 * (xi - 1/xi)
	sigma = sqrt((1 - m1^2) * (xi^2 + 1/xi^2) + 2 * m1^2 - 1)
	Random = (Random - mu)/sigma
	Random * sd + mean
}

qsnorm = function(p, mean = 0, sd = 1, xi = 1)
{
	m1 = 2/sqrt(2 * pi)
	mu = m1 * (xi - 1/xi)
	sigma = sqrt((1 - m1^2) * (xi^2 + 1/xi^2) + 2 * m1^2 - 1)
	g = 2/(xi + 1/xi)
	sig = sign(p - 1/2)
	Xi = xi^sig
	p = ((sign(p - 1/2) + 1) / 2 - sig * p)/(g * Xi)
	Quantile = (-sig * qnorm(p = p, sd = Xi) - mu)/sigma
	Quantile * sd + mean
}

### my variant on this -- random sqew normal constrained to P5-P95
rsnormc = function(n, mean=0, sd=1, xi = 1, quantile=c(0.05,0.95))
{
	if (length(n) != 1 || !is.finite(n) || n < 0 || n != floor(n) ||
		!is.finite(mean) || !is.finite(sd) || sd < 0 || !is.finite(xi) || xi <= 0 ||
		length(quantile) != 2 || any(!is.finite(quantile)) ||
		quantile[1] <= 0 || quantile[2] >= 1 || quantile[1] >= quantile[2])
		stop('Invalid sample size, spread, skew or quantiles for rsnormc.')
	if (n == 0) return(numeric())
	if (sd == 0)
		return(rep(mean, n))
		
	lower = qsnorm(quantile[1], mean, sd, xi)
	upper = qsnorm(quantile[2], mean, sd, xi)

	N = 1 + 1.1 * n / diff(quantile)
	x = rsnorm(N, mean, sd, xi)
	x = x[x>lower & x<upper]

	if (length(x) < n)
	{
		N = n - length(x)
		x = c(x, rsnormc(N, mean, sd, xi, quantile))
	}

	x[seq_len(n)]
}


# Create the fleet once. Reuse this table for every period and scenario.
# Every unit within a technology has an equal, fixed share of its capacity.
build_plants = function(tech, mossi, capacity, seed=23)
{
	if (!is.null(seed)) set.seed(seed)
	plants = vector('list', length(tech))
	for (i in seq_along(tech))
	{
		n = names(tech)[i]
		t = tech[[i]]
		cap = capacity[[t$capacity_column]]
		if (!length(cap) || any(!is.finite(cap)) || any(cap < 0) ||
			!is.finite(t$unit_size) || t$unit_size <= 0)
			stop('Invalid capacity or unit size for ', n)
		num_units = max(1, ceiling(max(cap) / t$unit_size))

		# keep the existing constrained efficiency and bid distributions
		# OCGT retains its own spread/skew, as in the original model
		spread = if (n == 'OCGT') t$varom_sd else mossi$bid_spread
		skew = if (n == 'OCGT') t$varom_xi else mossi$bid_skew
		if (!is.finite(spread) || spread < 0 || !is.finite(skew) || skew <= 0)
			stop('Invalid bid spread or skew for ', n)
		eff = rsnormc(num_units, t$eff_mu, t$eff_sd, t$eff_xi)
		if (any(!is.finite(eff)) || any(eff <= 0 | eff > 1))
			stop('Sampled efficiencies must be between zero and one for ', n)
		bid = rsnormc(num_units, 0, spread, skew)
		plants[[i]] = data.frame(
			id=sprintf('%s_%03d', n, seq_len(num_units)), tech=n,
			capacity_share=1 / num_units, eff=eff, carbon=t$carbon / eff,
			varom=t$varom_mu, bid_adjustment=bid, cap=0, mc=0,
			stringsAsFactors=FALSE
		)
	}
	do.call(rbind, plants)
}


# Take prepared plants, whose capacity and bid already reflect this period.
# Fractional unit capacities are expected when we scale the fleet.
build_stack = function(plants, precision=10)
{
	if (length(precision) != 1 || !is.finite(precision) || precision <= 0)
		stop('Stack precision must be a positive number of MW.')
	if (!nrow(plants) || any(!is.finite(as.matrix(plants[c('cap', 'mc', 'carbon')])) ) || any(plants$cap < 0))
		stop('Plants must have finite bids, emissions and non-negative capacities.')
	technologies = unique(plants$tech)
	plants = plants[plants$cap > 0, ]
	if (!nrow(plants)) stop('There is no available dispatchable capacity in this period.')
	plants = plants[order(plants$mc), ]
	upper = cumsum(plants$cap)
	total = tail(upper, 1)

	# include zero dispatch and the exact capacity ceiling, even off the MW grid
	grid = unique(c(seq(0, total, by=precision), total))
	unit = pmin(nrow(plants), 1 + findInterval(grid, upper, left.open=TRUE))
	stack = data.frame(cap=grid, mc=plants$mc[unit])

	# cumulative sums at plant boundaries, interpolated to our regular MW grid
	# this also handles units smaller than the grid spacing without losing them
	stack$carbon = approx(c(0, upper), c(0, cumsum(plants$cap * plants$carbon / 1000)),
		xout=grid, ties='ordered')$y
	for (n in technologies)
	{
		generation = plants$cap * (plants$tech == n)
		stack[[n]] = approx(c(0, upper), c(0, cumsum(generation)), xout=grid, ties='ordered')$y
	}
	stack
}


run_stack = function(inputs, stack, tech_groups=list())
{
	if (!all(c('demand', 'demand_net') %in% names(inputs)) ||
		any(!is.finite(inputs$demand)) || any(!is.finite(inputs$demand_net)))
		stop('run_stack needs finite demand and demand_net columns.')
	results = inputs
	technologies = setdiff(names(stack), c('cap', 'mc', 'carbon'))

	# round positive net demand up to the next grid point, capped by supply
	# zero/negative net demand uses zero dispatch, with the lowest stack bid
	d = pmax(1, pmin(nrow(stack), 1 + findInterval(inputs$demand_net, stack$cap, left.open=TRUE)))
	results$price = stack$mc[d]
	results$carbon = stack$carbon[d]
	results$ci = ifelse(results$demand > 0, results$carbon * 1000 / results$demand, NA_real_)
	for (n in technologies) results[[n]] = stack[[n]][d]
	for (n in names(tech_groups))
	{
		if (!all(tech_groups[[n]] %in% technologies)) stop('Unknown technology in group ', n)
		results[[n]] = rowSums(results[tech_groups[[n]]])
	}

	# retain first/last stack bid prices at the limits; expose the clipped MW
	# ponytail: no scarcity-price or curtailment model; add one if those cases matter
	results$unserved = pmax(0, inputs$demand_net - tail(stack$cap, 1))
	results$surplus = pmax(0, -inputs$demand_net)
	results
}
