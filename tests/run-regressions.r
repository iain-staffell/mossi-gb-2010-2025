# Run from the repository root: Rscript --vanilla tests/run-regressions.r
# Base R only. All model runs use temporary copies, leaving outputs/ untouched.
local({
	repo = normalizePath('.')
	work = tempfile('MOSSI regression ')
	dir.create(work)
	on.exit({ setwd(repo); unlink(work, recursive=TRUE) }, add=TRUE)
	stopifnot(all(file.copy(file.path(repo, c('model_gb_power_system.r', 'R', 'inputs')),
		work, recursive=TRUE)))
	setwd(work)

	# Check the full-run defaults without dispatching sixteen years of data.
	# Evaluate the actual configuration section in a fresh environment.
	script = readLines('model_gb_power_system.r')
	config_end = grep('^## ##  SET OUR CORE OPTIONS$', script)
	stopifnot(length(config_end) == 1)
	config = parse(text=script[seq_len(config_end - 1)])
	check_config = function(args) {
		env = new.env(parent=baseenv())
		env$commandArgs = function(trailingOnly=FALSE) if (trailingOnly) args else character()
		eval(config, env)
		env
	}
	full = check_config(character())
	stopifnot(full$output_dir == 'outputs', !full$quick_run,
		full$start_date == as.Date('2010-01-01'), full$end_date == as.Date('2025-12-31'))
	quick = check_config('--quick')
	stopifnot(quick$output_dir == file.path('outputs', 'quick-run'), quick$quick_run,
		quick$start_date == as.Date('2010-01-01'), quick$end_date == as.Date('2010-01-31'))
	# The documented manual switch must select the same output directory.
	manual = check_config(character())
	eval(parse(text=sub('quick_run = FALSE', 'quick_run = TRUE',
		script[seq_len(config_end - 1)], fixed=TRUE)), manual)
	stopifnot(manual$quick_run, manual$output_dir == quick$output_dir,
		manual$start_date == quick$start_date, manual$end_date == quick$end_date)

	rscript = file.path(R.home('bin'), 'Rscript')
	run_quick = function(no_plots=FALSE) {
		args = c('--vanilla', 'model_gb_power_system.r', '--quick')
		if (no_plots) args = c(args, '--no-plots')
		status = system2(rscript, args, stdout='run.log', stderr='run.log')
		if (status != 0) stop(paste(readLines('run.log'), collapse='\n'))
	}
	verify_outputs = function(plot) {
		path = file.path('outputs', 'quick-run')
		files = c('results.rds', 'results.csv', 'monthly_summary.csv', 'session_info.txt')
		if (plot) files = c(files, 'figures.pdf')
		stopifnot(setequal(list.files(path), files),
			all(file.info(file.path(path, files))$size > 0))
		saved = readRDS(file.path(path, 'results.rds'))
		stopifnot(is.list(saved$settings), is.data.frame(saved$plants),
			is.list(saved$financials), is.list(saved$ann_financials),
			inherits(saved$session_info, 'sessionInfo'),
			inherits(saved$results$date, 'POSIXct'), inherits(saved$run_dates, 'Date'),
			saved$settings$quick_run, identical(saved$settings$plot, plot),
			length(saved$settings$input_md5) == 3, !anyNA(saved$settings$input_md5),
			length(saved$run_dates) == 31, nrow(saved$results) == 31 * 48,
			all(is.finite(saved$results$price)),
			nrow(saved$monthly_summary) == 1,
			identical(as.numeric(saved$results$date), as.numeric(saved$out$date)))
		expected_dates = seq(as.POSIXct('2010-01-01 00:00:00', tz='UTC'),
			as.POSIXct('2010-01-31 23:30:00', tz='UTC'), by='30 min')
		stopifnot(identical(as.numeric(saved$results$date), as.numeric(expected_dates)))
		for (name in c('results', 'monthly_summary')) {
			csv = read.csv(file.path(path, paste0(name, '.csv')), stringsAsFactors=FALSE)
			expected = saved[[name]]
			expected$date = as.character(expected$date)
			rownames(expected) = NULL
			stopifnot(isTRUE(all.equal(csv, expected, tolerance=1e-12)))
		}
		stopifnot(any(grepl('R version', readLines(file.path(path, 'session_info.txt')))))
		if (plot) stopifnot(rawToChar(readBin(file.path(path, 'figures.pdf'), 'raw', 5)) == '%PDF-')
		saved
	}

	# The documented command must work in a clean R session and create its folders.
	run_quick()
	with_plots = verify_outputs(TRUE)
	stopifnot(identical(list.files('outputs'), 'quick-run'))

	# With plots disabled, a fresh quick run must leave full-run results untouched.
	unlink(file.path('outputs', 'quick-run'), recursive=TRUE)
	writeLines('full-run sentinel', file.path('outputs', 'results.csv'))
	run_quick(no_plots=TRUE)
	without_plots = verify_outputs(FALSE)
	stopifnot(identical(readLines(file.path('outputs', 'results.csv')), 'full-run sentinel'))
	for (name in setdiff(names(with_plots), c('settings', 'session_info')))
		stopifnot(identical(with_plots[[name]], without_plots[[name]]))

	cat('All regression tests passed.\n')
})
