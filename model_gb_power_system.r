#####
## ##  CHOOSE WHAT TO RUN
#####

	# open MOSSI.Rproj, then source this whole script.
	project_dir = local({
		paths = unlist(lapply(sys.frames(), function(frame) frame$ofile))
		args = commandArgs(trailingOnly=FALSE)
		paths = c(paths, sub('^--file=', '', args[grepl('^--file=', args)]))
		paths = paths[basename(paths) == 'model_gb_power_system.r' & file.exists(paths)]
		root = if (length(paths)) dirname(normalizePath(tail(paths, 1))) else getwd()
		if (!file.exists(file.path(root, 'R', 'dispatch.r')))
			stop('Open MOSSI.Rproj or source the full path to model_gb_power_system.r.')
		root
	})

	# how often should we update capacity and prices, and rebuild the stack?
	# 'quarter' / 'month' / 'day' --- but dispatch is always half-hourly
	run_period = 'day'
	start_date = as.Date('2010-01-01')
	end_date = as.Date('2025-12-31')

	# report results independently of how often the stack changes
	# 'year' / 'quarter' / 'month' / 'day'
	report_period = 'month'
	plot = TRUE

	# quickly check this works?  
	# TRUE runs January 2010 as a short demonstration
	quick_run = FALSE 

	# optional command-line shortcuts if running from RScript
	args = commandArgs(trailingOnly=TRUE)
	if ('--quick' %in% args) quick_run = TRUE
	if ('--no-plots' %in% args) plot = FALSE
	if (quick_run) {
		start_date = as.Date('2010-01-01')
		end_date = as.Date('2010-01-31')
		output_dir = file.path(output_dir, 'quick-run')
	}

	# both price files have daily rows; the monthly file repeats each month's prices
	# run_period controls averaging of prices and capacity when building the stack
	price_file    = 'inputs/monthly_fuel_carbon_prices.csv'
	capacity_file = 'inputs/daily_plant_capacity.csv'
	genmix_file   = 'inputs/electric_insights_data.rds'
	output_dir    = 'outputs' 



#####
## ##  SET OUR CORE OPTIONS
#####

	# do we want to fix a random seed
	# NULL if not
	seed = 23

	# initialise an object to hold the MOSSI model parameters
	# and set its precision (in MW)	
	mossi = list()
	mossi$precision = 10


	# in previous work (https://doi.org/10.1016/j.joule.2021.09.011)
	# we used 'bid spreading' - allowing generators to bid power prices
	# that differ from their short-run marginal cost (SRMC), which better
	# reflects their actual behaviour, the generation mix, and wholesale prices

	# we don't use that here though, as we instead want to calculate the SRMC
	# of a perfectly competitive market, rather than what we think prices should be

	# how much do power stations spread their bids away from SRMC (GBP/MWh)
	mossi$bid_spread = 0 		# (previously 17.50)

	# the skewness of the bid distributions
	# (1 = normal, 1.5 = long upwards tail, 0.666 = long downwards tail)
	mossi$bid_skew = 1			# (previously 0.925)

	# should fuel prices vary differently from their long-run mean
	# e.g. to have greater seasonal cyclical movements?
	# (0.5 means they vary at 1.5x, -0.2 means they vary at 0.8x)
	mossi$fuel_var = 0			# (previously 0.25)


	# define our fleet-average power station availability
	# Ofgem, 2012: Electricity capacity assessment, report to government, Reference: 126/12 https://www.ofgem.gov.uk/ofgem-publications/40203/electricity-capacity-assessment-2012.pdf
	# Maintenance is concentrated in the summer months. Ofgem, 2012 (page 27, Fig. 3.1) reports that coal fired power plants are 26% more available in winter than in summer - respectively gas CCGT (+17%), OCGT (+14%) and nuclear (+12%). Gas prices are also seasonal.
	# TODO (it would be 'more pretty' to specify this as a daily smooth sine)
	#      (but that wouldn't materially affect results)

	availability = data.frame(
		quarter = c( 1,  2,  3,  4),
		nuclear = c(75, 70, 65, 70) / 100,
		coal    = c(85, 65, 60, 80) / 100,
		ccgt    = c(80, 70, 65, 80) / 100,
		ocgt    = c(80, 70, 70, 80) / 100
	)



#####
## ##  LOAD OUR INPUTS
#####

	# code 
	for (script in c('blam', 'plants', 'inputs', 'dispatch', 'periods', 'plots'))
		source(file.path(project_dir, 'R', paste0(script, '.r')), local=TRUE, encoding='UTF-8')

	# find the data
	input_files = file.path(project_dir, c(capacity_file, price_file, genmix_file))
	if (any(!file.exists(input_files)))
		stop('Missing input file(s): ', paste(input_files[!file.exists(input_files)], collapse=', '))
	output_path = file.path(project_dir, output_dir)
	if (!dir.exists(output_path) && !dir.create(output_path, recursive=TRUE))
		stop('Cannot create output folder: ', output_path)

	# load the data
	data = load_mossi_inputs(input_files[1], input_files[2], input_files[3])
	plants = build_plants(tech, mossi, data$capacity, seed)

	# include partial first/last periods if requested
	run_period = match.arg(run_period, c('quarter', 'month', 'day'))
	report_period = match.arg(report_period, c('year', 'quarter', 'month', 'day'))
	periods = make_periods(start_date, end_date, run_period)
	requested_days = as.character(seq(start_date, end_date, by='day'))
	if (any(!requested_days %in% names(data$rows_by_day)))
		stop('The requested dates extend beyond the half-hourly input data.')
	period_results = vector('list', nrow(periods))



#####
## ##  RUN THE MODEL
#####

	# progress ux
	PB = blam.progress(nrow(periods))
	flush( paste('Dispatching MOSSI for', nrow(periods), 'periods (', run_period, ')...\n') )

	# build the merit order stack and dispatch it
	for (i in seq_len(nrow(periods)))
	{
		period_results[[i]] = run_one_period(
			mossi, tech, plants, data, availability,
			periods$start[i], periods$end[i], tech_groups
		)

		# progress ux
		if (interactive() && i %% 25 == 0) PB$total(i)
		if (!interactive() && (i %% 500 == 0 || i == nrow(periods)))
			flush(sprintf('  %d / %d periods complete\n', i, nrow(periods)))
	}
	if (interactive()) PB$total(nrow(periods), flush=TRUE)



#####
## ##  ASSEMBLE THE RESULTS
#####

	flush('\nAssembling results...\n')

	# assemble all results
	results = do.call(rbind, lapply(period_results, function(x) x$results))
	capacity = as.data.frame(do.call(rbind, lapply(period_results, function(x) x$capacity)))
	fuelprice = as.data.frame(do.call(rbind, lapply(period_results, function(x) x$fuelprice)))
	run_dates = periods$start

	financials = list()
	for (n in names(period_results[[1]]$financials))
	{
		values = do.call(rbind, lapply(period_results, function(x) x$financials[[n]]))
		financials[[n]] = data.frame(date=run_dates, values)
	}
	rm(period_results)


	# generate annual financial results for each technology (million GBP)
	ann_financials = lapply(financials, function(x) {
		aggregate(x[setdiff(names(x), 'date')],
			by=list(date=period_start(x$date, 'year')), FUN=sum)
	})

	# compare model results with actual outturn
	comparison = summarise_results(results, data$actual, report_period)



#####
## ##  MAKE SUMMARY PLOTS & SAVE
#####

	# align actual inputs to modelled short-run marginal cost
	actual = data$actual[results$input_row, ]
	stopifnot(identical(as.numeric(actual$date), as.numeric(results$date)))
	out = data.frame(
		date = actual$date,
		price = actual$price.day,
		cost = results$price,
		demand = actual$demand,
		wind = actual$wind,
		solar = actual$solar
	)

	# look at monthly aggregate differences
	dc = out$demand * out$cost / 2
	dp = out$demand * out$price / 2

	# .. in the median difference between price & cost
	agg1 = aggregate_monthly(out$date, out$price-out$cost, median)
	# .. in the mean difference between price & cost
	agg2 = aggregate_monthly(out$date, out$price-out$cost, mean)
	# .. in the sum of difference between price & cost (in billions)
	agg3 = aggregate_monthly(out$date, (dp-dc)/1e9, sum)

	# split the period into the 18 months of crisis... and everything else
	pdiff = out$price - out$cost
	ndema = out$demand - out$wind - out$solar
	crisis = out$date >= as.POSIXct('2021-07-01 00:00:00', tz='UTC') & out$date < as.POSIXct('2023-01-01 00:00:00', tz='UTC')

	# summarise differences as a function of net demand
	a = summarise_net_demand(pdiff[!crisis], ndema[!crisis])
	b = summarise_net_demand(pdiff[crisis], ndema[crisis])
	monthly_summary = data.frame(date=agg1$date,
		median_price_minus_srmc_gbp_mwh=agg1$value,
		mean_price_minus_srmc_gbp_mwh=agg2$value,
		income_minus_marginal_cost_gbp_bn=agg3$value)

	# save results
	settings = list(start_date=start_date, end_date=end_date, run_period=run_period,
		report_period=report_period, seed=seed, mossi=mossi, tech=tech,
		availability=availability, plot=plot, quick_run=quick_run,
		input_md5=setNames(unname(tools::md5sum(input_files)),
			c(capacity_file, price_file, genmix_file)))

	model_output = list(settings=settings, plants=plants, results=results,
		run_dates=run_dates, capacity=capacity, fuelprice=fuelprice,
		comparison=comparison, financials=financials, ann_financials=ann_financials,
		out=out, monthly_summary=monthly_summary,
		net_demand=list(other=a, crisis=b), session_info=sessionInfo())

	write.csv(model_output, file.path(output_path, 'results.csv'), row.names=FALSE)
	write.csv(monthly_summary, file.path(output_path, 'monthly_summary.csv'), row.names=FALSE)
	writeLines(capture.output(sessionInfo()), file.path(output_path, 'session_info.txt'))

	if (plot)
		write_mossi_plots(file.path(output_path, 'figures.pdf'), run_dates,
			capacity, fuelprice, comparison, stack_colour, monthly_summary, a, b)

	flush('\nFinished. Results saved in: ', normalizePath(output_path), '\n')
