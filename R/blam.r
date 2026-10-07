#############################################################
#####       ____    _                       _____       #####
#####      |  _ \  | |                     |  __ \      #####
#####      | |_) | | |   __ _   _ __ ___   | |__) |     #####
#####      |  _ <  | |  / _' | | '_ ` _ \  |  _  /      #####
#####      | |_) | | | | (_| | | | | | | | | | \ \      #####
#####      |____/  |_|  \__,_| |_| |_| |_| |_|  \_\     #####
#####                                                   #####
#############################################################
#####      07-10-2026       #####     IAIN STAFFELL     #####
#############################################################



###################################################################################################################
##################      GENERAL STUFF      ########################################################################
###################################################################################################################

	# concatenate strings
	`%&%` = function(a, b) paste0(a, b)

	# console - flush line and clear line
	flush = function(...)
	{
		cat(...)
		flush.console()
	}

	clear = function(...)
	{
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
		cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")

		if (length(list(...)) > 0)
		{
			cat(...)
			flush.console()
		}
	}



	#######################################################################
	#
	#   A CLASS FOR PRODUCING SIMPLE PROGRESS BARS
	#
	#	PB = blam.progress(500)
	#   for (i in 1:500) {
	#		# do stuff...
	#		PB$tick()
	#		# or PB$tick(msg='some text..')
	#	}
	#
	#   if you want to run an expensive progress update, it's nice to have that
	#   often to begin with, then becoming more spaced out as time goes on...
	#
	#   PB = blam.progress(13000, milestones=30)
	#   for (i in 1:13000) {
	#		# do stuff...
	#		PB$tick()
	#		if (PB$milestone()) {
	#			# expensive update...
	#		}
	#	}
	#
	#
	#   see progress_bar.r for more examples

	blam.progress = function(N, dp=NULL, i_dp=NULL, N_dp=NULL, milestones=round(sqrt(100*sqrt(N))), update=0.25)
	{

		#####
		## ##  MEMBERS
		#####

		PB = list(

			time_start = NA,
			time_now = NA,

			i = 0,
			skipped = 0,
			N = NA,
			milestones = NA,

			time_since_print = NA,
			time_elapsed = NA,
			time_remain = NA,
			rate_avg = NA,

			pct_dp = NA, 
			i_dp = 0, 
			N_dp = 0,
			update = 0.25

		)


		#####
		## ##  METHODS
		#####

		PB$constructor = function(N, dp=NULL, i_dp=NULL, N_dp=NULL, milestones=100, update=0.25)
		{
			# initialise our time
			PB$time_start = Sys.time()
			PB$time_since_print = PB$time_start - 999

			# initialise the size of the calculation
			PB$N = N

			# choose a suitable number of d.p. for the percentage
			if (is.null(dp))
			{
				dp = log10(N / 100)
				dp = max(0, floor(dp-1))
			}
			PB$pct_dp = dp

			# assign the number of d.p. for the i of N part, if wanted
			if (!is.null(i_dp))
			{
				PB$i_dp = i_dp
			}

			if (!is.null(N_dp))
			{
				PB$N_dp = N_dp
			}

			# assign milestone values (points that are log spaced when you might want an update)
			m = 1 + (N-1) * (seq(0, 1, length.out=milestones) ^ 2)
			PB$milestones = unique(round(m))

			# assign the update frequency (in seconds)
			PB$update = update

			# save
			assign('PB', PB, envir=PB)
		}


		PB$tick = function(i=1, msg=NULL, msg_front=NULL, flush=FALSE)
		{
			PB$i = PB$i + i
			PB$time_now = Sys.time()
			if (flush | (PB$time_now - PB$time_since_print) > PB$update)
				PB$print(msg=msg, msg_front=msg_front)

			# save
			assign('PB', PB, envir=PB)
		}

		PB$skip = function(i=1, msg=NULL, msg_front=NULL, flush=FALSE)
		{
			PB$i = PB$i + i
			PB$skipped = PB$skipped + i
			PB$time_now = Sys.time()
			if (flush | (PB$time_now - PB$time_since_print) > PB$update)
				PB$print(msg=msg, msg_front=msg_front)

			# save
			assign('PB', PB, envir=PB)
		}

		PB$total = function(i, msg=NULL, msg_front=NULL, flush=FALSE)
		{
			PB$i = i
			PB$time_now = Sys.time()
			if (flush | (PB$time_now - PB$time_since_print) > PB$update)
				PB$print(msg=msg, msg_front=msg_front)

			# save
			assign('PB', PB, envir=PB)
		}

		PB$milestone = function()
		{
			return(PB$i %in% PB$milestones)
		}


		#####
		## ##  PRIVATE METHODS
		#####


		# function to print everything out how i want it
		PB$print = function(msg=NULL, msg_front=NULL)
		{
			PB$time_since_print = PB$time_now

			PB$time_elapsed = as.numeric( difftime(PB$time_now, PB$time_start, unit='secs') )
			PB$time_elapsed = max(1e-6, PB$time_elapsed)

			share_done = PB$i / PB$N
			graft_done = (PB$i - PB$skipped) / (PB$N - PB$skipped)
			graft_left = (1 - graft_done) / graft_done

			PB$time_remain  = PB$time_elapsed * graft_left
			PB$rate_avg = (PB$i - PB$skipped) / PB$time_elapsed

			elapsed_txt = PB$print_time(PB$time_elapsed)
			rate_txt = PB$rate(PB$rate_avg)
			eta_txt = PB$print_time(PB$time_remain)

			clear()

			if (!is.null(msg_front))
				flush(msg_front)

			flush(percent(share_done, PB$pct_dp), 'in', elapsed_txt, '-', round(PB$i, PB$i_dp), '/', round(PB$N, PB$N_dp), '-', rate_txt, '- ETA:', eta_txt)

			if (!is.null(msg))
				flush(' -', msg)
		}

		# function to print the time how i want it
		PB$print_time = function(seconds)
		{
			if (is.infinite(seconds) | is.nan(seconds) | is.na(seconds))
			{
				return('The end of time...')
			}

			if (seconds < 3600) {

				mins = floor(seconds / 60)
				seconds = floor(seconds %% 60)
				txt = paste0(mins, 'm', seconds, 's')

			} else if (seconds < 86400) { 

				mins = seconds / 60
				hours = floor(mins / 60)
				mins = floor(mins %% 60)
				txt = paste0(hours, 'h', mins, 'm')

			} else {

				hours = seconds / 3600
				days = floor(hours / 24)
				hours = floor(hours %% 24)
				txt = paste0(days, 'd', hours, 'h')

			}

			return(txt)
		}

		# function to print the speed how i want it
		PB$rate = function(rate)
		{
			if (rate < 1/60) {

				txt = sigfig(rate*3600, 3) %&% ' / hour'

			} else if (rate < 1) { 

				txt = sigfig(rate*60, 3) %&% ' / min'

			} else {

				txt = sigfig(rate, 3) %&% ' / sec'

			}

			return(txt)
		}


		#####
		## ##  BUILD CLASS
		#####

		# define this as an environment
		PB = list2env(PB)

		# set the class type
		class(PB) = 'PBClass'

		# run the constructor
		PB$constructor(N, dp, i_dp, N_dp, milestones, update)

		# return
		return(PB)
	}



###################################################################################################################
##################      GRAPHICS      #############################################################################
###################################################################################################################

	# write a y-axis legend at the top of the chart
	ylab_top = function(text, at=0.1, cex=1, adj=0, font=2)
	{
		if (max(par('mfrow')) == 2) cex = cex * 0.83
		if (max(par('mfrow')) > 2) cex = cex * 0.66

		mtext(side=3, text, line=at, cex=cex, font=font, adj=adj)
	}


	# plot the polygon for an area chart
	plot_area = function(x, y1, y2, ...)
	{
		if (length(y1) == 1)
			y1 = rep(y1, length(x))
		if (length(y2) == 1)
			y2 = rep(y2, length(x))

		# if necessary, split into seperate groups based on NA
		# to avoid it drawing wrong
		if (any(is.na(c(x,y1,y2))))
		{
			pos = which(is.na(x) | is.na(y1) | is.na(y2))
			pos = findInterval(seq_along(x), pos+1)

			s = split(data.frame(x=x,y1=y1,y2=y2), pos)
			s = lapply(s, na.everyone.remove)

			for (ss in s)
				plot_area(ss$x, ss$y1, ss$y2, ...)

		} else {

			# now draw it
			x = c(x, rev(x))
			y = c(y1, rev(y2))
			polygon(x, y, ...)
		}
	}


	# convert one/some numbers into percentage format
	percent = function(val, dp=0, sf=NULL, na.rm=FALSE, plus=FALSE)
	{
		# fixed number of significant figures, if specified
		if (!is.null(sf))
			txt = paste0(sigfig(val*100, sf), '%')

		# or default to a fixed number of decimal places
		if (is.null(sf))
			txt = paste0(formatC(100*val, digits=dp, format='f'), '%')

		# clean up NA values
		if (na.rm == TRUE)
			txt[ is.na(val) ] = ''

		# add plus signs
		if (plus == TRUE){
			x = (val > 0)
			x[ is.na(x) ] = FALSE
			txt[ x ] = '+' %&% txt[ x ]
		}
		txt
	}


	# convert one/some numbers to sf significant figures
	sigfig = function(val, sf)
	{
		val = formatC(signif(val, digits=sf), digits=sf, format="fg", flag="#")
		gsub("\\.$", "", val)
	}


###################################################################################################################
##################      ARRAY TOOLS      ##########################################################################
###################################################################################################################

	# remove rows containing NA in any selected column
	na.everyone.remove = function(dataframe, cols=colnames(dataframe))
	{
		# find NA in all rows/columns
		filter = is.na(dataframe[ , cols])

		# find rows where any column is NA
		filter = apply(filter, 1, any)
		filter = as.vector(filter)

		# remove those rows
		dataframe[!filter, ]
	}



###################################################################################################################
##################      TIME-SERIES      ##########################################################################
###################################################################################################################

	# aggregate our data as desired

	aggregate_monthly = function(dates, values, FUN='mean', ...)
	{
		results = aggregate(values, by=list(format(dates, '%Y-%m-01')), FUN=FUN, ...)
		colnames(results) = c('date', 'value')
		results$date = as.Date(results$date)
		results
	}


###################################################################################################################
##################      STATISTICS      ###########################################################################
###################################################################################################################

	# you have a bunch of numbers...
	# you want them rounded to the nearest from a set of other numbers
	# or the nearest of a multiple of numbers
	# 
	round_to_nearest = function(numbers, breaks, plot=FALSE)
	{
		if (length(breaks) == 1) {

			# round our numbers to the nearest multiple
			rounded = round(numbers/breaks) * breaks

		} else {

			# filter out NA first
			nmbrs = numbers[ !is.na(numbers) ]

			# round our numbers to custom breaks
			breaks = c(-Inf, sort(breaks))
			nearest = findInterval(nmbrs, breaks)

			# get the upper/lower limits for each number
			lower = breaks[nearest]
			upper = c(breaks, Inf)[nearest+1]

			# pick the closest
			use_upper = (upper - nmbrs) < (nmbrs - lower)
			use_upper[ is.na(use_upper) ] = TRUE
			rndd = lower
			rndd[use_upper] = upper[use_upper]

			# splice them back in
			rounded = numbers
			rounded[ !is.na(rounded) ] = rndd

		}

		if (plot)
		{
			# show folks what's going on
			plot(numbers, lower, pch=16, cex=0.5, col='grey')
			points(numbers, upper, pch=16, cex=0.5, col='grey')
			for (r in breaks) abline(v=r, col='red')
			points(numbers, rounded, pch=16)
		}

		rounded
	}
	round_n = round_to_nearest


	# calculate root mean squared
	rms = function(x)
	{
		sqrt( mean( x^2, na.rm=TRUE) )
	}
	rmse = function(x, y)
	{
		rms(x - y)
	}

