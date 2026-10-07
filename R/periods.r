#####
## ##  SELECT DATES, RUN A PERIOD AND SUMMARISE RESULTS
#####

period_start = function(dates, period)
{
	period = match.arg(period, c('day', 'month', 'quarter', 'year'))
	dates = as.Date(dates, tz='UTC')
	if (period == 'day') return(dates)
	month = as.integer(format(dates, '%m'))
	if (period == 'quarter') month = 1 + 3 * ((month - 1) %/% 3)
	if (period == 'year') month = 1
	as.Date(sprintf('%s-%02d-01', format(dates, '%Y'), month))
}


# The user's final date is inclusive. Internally, each period ends just before
# the next period starts, so no half-hour belongs to two periods.
make_periods = function(start_date, end_date, period='quarter')
{
	period = match.arg(period, c('day', 'month', 'quarter', 'year'))
	start_date = as.Date(start_date)
	end_date = as.Date(end_date)
	if (length(start_date) != 1 || length(end_date) != 1 ||
		anyNA(c(start_date, end_date)) || start_date > end_date)
		stop('Please give a valid start date and an end date on or after it.')
	step = switch(period, day='day', month='month', quarter='3 months', year='year')
	last = seq(period_start(end_date, period), by=step, length.out=2)[2]
	boundaries = seq(period_start(start_date, period), last, by=step)
	data.frame(start=pmax(head(boundaries, -1), start_date),
		end=pmin(tail(boundaries, -1), end_date + 1))
}


setup_mossi_period = function(mossi, tech, plants, data, availability, start_date, end_date)
{
	# end_date is exclusive here (use make_periods for an inclusive user range)
	start_date = as.Date(start_date)
	end_date = as.Date(end_date)
	if (start_date >= end_date) stop('A model period must contain at least one day.')
	days = seq(start_date, end_date - 1, by='day')
	f = match(days, data$capacity$date)
	if (anyNA(f)) stop('Missing daily capacity in period starting ', start_date)
	cc = data$capacity[f, , drop=FALSE]
	qq = data$prices[match(days, data$prices$date), , drop=FALSE]
	if (any(!is.finite(as.matrix(qq[setdiff(names(qq), 'date')]))))
		stop('Missing fuel/carbon prices in period starting ', start_date)

	# apply availability day by day, then average available MW over the period
	q = 1 + (as.integer(format(days, '%m')) - 1) %/% 3
	for (n in names(tech))
	{
		t = tech[[n]]
		installed = cc[[t$capacity_column]]
		available = availability[[t$availability_column]][match(q, availability$quarter)]
		if (length(installed) != length(days) || any(!is.finite(installed)) || any(installed < 0) ||
			length(available) != length(days) || any(!is.finite(available)) || any(available < 0 | available > 1))
			stop('Invalid capacity or availability for ', n, ' in period starting ', start_date)
		tech[[n]]$cap = mean(installed * available)
	}

	# the reference fuel mean is calculated from the full price input history
	# it does not move when we change the simulation dates or period length
	prices = colMeans(qq[setdiff(names(qq), 'date')])
	coal_var = if (is.null(mossi$coal_var)) mossi$fuel_var else mossi$coal_var
	gas_var = if (is.null(mossi$gas_var)) mossi$fuel_var else mossi$gas_var
	prices['coal'] = prices['coal'] + coal_var * (prices['coal'] - data$fuel_mean['coal'])
	prices['gas'] = prices['gas'] + gas_var * (prices['gas'] - data$fuel_mean['gas'])
	for (n in names(tech)) tech[[n]]$fuel = unname(prices[tech[[n]]$fuel_column])
	mossi$carbon_price = unname(prices['carbon'])
	mossi$carbon_price_ets = unname(prices['carbon_ets'])
	mossi$carbon_price_cpf = unname(prices['carbon_cpf'])

	# keep the fixed plant characteristics, changing only MW and current costs
	for (n in names(tech))
	{
		p = plants$tech == n
		plants$cap[p] = plants$capacity_share[p] * tech[[n]]$cap
		plants$mc[p] = tech[[n]]$fuel / plants$eff[p] +
			plants$carbon[p] * mossi$carbon_price / 1000 + plants$varom[p] + plants$bid_adjustment[p]
	}

	# use each original observation once, including repeated autumn timestamps
	rows = data$rows_by_day[as.character(days)]
	counts = lengths(rows)
	if (any(!counts %in% c(46, 48, 50)))
		stop('Missing or incomplete half-hourly data in period starting ', start_date)
	f = unlist(rows, use.names=FALSE)
	eiq = data$actual[f, ]
	inputs = data.frame(
		date=eiq$date, input_row=f, demand=eiq$demand,
		WindOnshore=eiq$onshore.wind, WindOffshore=eiq$offshore.wind,
		Solar=eiq$solar, Nuclear=eiq$nuclear,
		Imports=eiq$other_supply # precomputed imports, pumped storage and hydro
	)
	if (any(!is.finite(as.matrix(inputs[setdiff(names(inputs), 'date')]))))
		stop('Missing dispatch inputs in period starting ', start_date)
	if (any(!is.finite(as.matrix(eiq[c('wind', 'coal', 'gas', 'carbon.domestic', 'price.day')]))))
		stop('Missing market outturn in period starting ', start_date)
	inputs$demand_net = with(inputs, demand - WindOnshore - WindOffshore - Solar - Nuclear - Imports)

	# record the capacities and fuel prices used, for inspection and plotting
	capacity = c(vapply(tech, function(t) t$cap, numeric(1)),
		wind_onshore=mean(cc$wind_onshore), wind_offshore=mean(cc$wind_offshore),
		solar=mean(cc$solar), nuclear=mean(cc$nuclear), demand=median(inputs$demand))
	fuelprice = c(vapply(tech, function(t) t$fuel, numeric(1)), carbon=mossi$carbon_price)
	list(tech=tech, mossi=mossi, plants=plants, inputs=inputs, capacity=capacity, fuelprice=fuelprice)
}


run_one_period = function(mossi, tech, plants, data, availability, start_date, end_date, tech_groups)
{
	prepared = setup_mossi_period(mossi, tech, plants, data, availability, start_date, end_date)
	stack = build_stack(prepared$plants, mossi$precision)
	results = run_stack(prepared$inputs, stack, tech_groups)
	financials = calculate_financials(results, prepared$tech, prepared$mossi)
	list(results=results, financials=financials, capacity=prepared$capacity, fuelprice=prepared$fuelprice)
}


# Financial estimates use the existing technology-average cost assumptions.
# Bidding adjustments affect the clearing price, not the actual operating cost.
calculate_financials = function(results, tech, mossi)
{
	tech$WindOnshore = list(varom_mu=4, eff_mu=1, fuel=0, carbon=0)
	tech$WindOffshore = list(varom_mu=6, eff_mu=1, fuel=0, carbon=0)
	tech$Solar = list(varom_mu=3, eff_mu=1, fuel=0, carbon=0)
	tech$Nuclear = list(varom_mu=8, eff_mu=0.33, fuel=0, carbon=0)
	financials = setNames(lapply(1:6, function(i) setNames(numeric(length(tech)), names(tech))),
		c('revenue', 'profit', 'expenses', 'opex', 'fuel', 'carbon'))
	for (n in names(tech))
	{
		t = tech[[n]]
		# each observation is half an hour; all returned amounts are million GBP
		mwh = results[[n]] * 0.5
		financials$revenue[n] = sum(mwh * results$price) / 1e6
		financials$opex[n] = sum(mwh) * t$varom_mu / 1e6
		financials$fuel[n] = sum(mwh) * t$fuel / t$eff_mu / 1e6
		financials$carbon[n] = sum(mwh) * t$carbon / t$eff_mu / 1000 * mossi$carbon_price / 1e6
	}
	financials$expenses = financials$opex + financials$fuel + financials$carbon
	financials$profit = financials$revenue - financials$expenses
	financials
}


# Reporting is independent of stack-update frequency. Calculate price quantiles
# from the original half-hours, never from averages of daily/monthly quantiles.
summarise_results = function(results, actual, period='quarter')
{
	if (!nrow(results)) stop('There are no model results to summarise.')
	actual = actual[results$input_row, ]
	if (!identical(as.numeric(actual$date), as.numeric(results$date)))
		stop('Model results and actual observations do not line up.')
	groups = split(seq_len(nrow(results)), period_start(results$date, period))
	act = sim = vector('list', length(groups))
	for (i in seq_along(groups))
	{
		f = groups[[i]]
		act[[i]] = c(carbon=sum(actual$carbon.domestic[f]) / 2e6,
			coal=sum(actual$coal[f]) / 2e6, gas=sum(actual$gas[f]) / 2e6,
			setNames(quantile(actual$price.day[f], c(0.1, 0.5, 0.9)), c('P10', 'P50', 'P90')))
		sim[[i]] = c(carbon=sum(results$carbon[f]) / 2e6,
			coal=sum(results$Coal[f]) / 2e6, gas=sum(results$CCGT[f]) / 2e6,
			setNames(quantile(results$price[f], c(0.1, 0.5, 0.9)), c('P10', 'P50', 'P90')))
	}
	act = as.data.frame(do.call(rbind, act))
	sim = as.data.frame(do.call(rbind, sim))
	errors = vapply(names(act), function(n) rmse(act[[n]], sim[[n]]), numeric(1))
	list(dates=as.Date(names(groups)), actual=act, model=sim, errors=errors,
		half_hours=lengths(groups))
}


# The original analysis uses boxplot hinges, grouped in 2 GW net-demand bins.
summarise_net_demand = function(price_difference, net_demand)
{
	if (!length(price_difference))
		return(data.frame(net=numeric(), p25=numeric(), p50=numeric(), p75=numeric()))
	x = boxplot(price_difference ~ round_n(net_demand, 2000), plot=FALSE, outline=FALSE)
	data.frame(net=as.numeric(x$names)/1000, p25=x$stats[2, ],
		p50=x$stats[3, ], p75=x$stats[4, ])
}
