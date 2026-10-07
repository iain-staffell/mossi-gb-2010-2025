#####
## ##  PLOTS FOR INSPECTING THE STACK AND RESULTS
#####

	# Use a single PDF on every platform, including machines without a display.
	write_mossi_plots = function(file, run_dates, capacity, fuelprice, comparison,
		stack_colour, monthly_summary, other, crisis)
	{
		pdf(file, width=12, height=8, onefile=TRUE)
		on.exit(dev.off(), add=TRUE)
		plot_comparison(run_dates, capacity, fuelprice, comparison, stack_colour)

		par(cex=1.1, mar=c(5, 5, 4, 2) + 0.1)
		labels = c('Median difference between day-ahead price and SRMC (\u00a3/MWh)',
			'Mean difference between day-ahead price and SRMC (\u00a3/MWh)',
			'Market-wide income minus marginal cost (\u00a3bn)')
		for (i in seq_along(labels)) {
			plot(monthly_summary$date, monthly_summary[[i+1]],
				type=if (nrow(monthly_summary) == 1) 'p' else 'l', xlab='', ylab='')
			ylab_top(labels[i], cex=1.1)
			abline(h=0)
		}

		plot(NA, xlim=c(0, 60), ylim=c(-100, 350),
			xlab='Demand net of wind and solar (GW)',
			ylab='Difference between day-ahead price and SRMC (\u00a3/MWh)')
		abline(h=0)
		groups = list(crisis, other)
		colours = c('#FF8529', '#182CC4')
		labels = c('July 2021 to December 2022', 'Other months in the selected date range')
		present = vapply(groups, nrow, integer(1)) > 0
		for (i in which(present)) {
			x = groups[[i]]
			plot_area(x$net, x$p25, x$p75, border=NA, col=paste0(colours[i], '44'))
			lines(x$net, x$p50, lwd=3, col=colours[i])
		}
		legend('topleft', legend=labels[present], lwd=3, col=colours[present], bty='n')
	}

	plot_comparison = function(run_dates, capacity, fuelprice, comparison, stack_colour)
	{
		layout(matrix(1:6, nrow=3, byrow=TRUE))
		plot_capacity(run_dates, capacity, stack_colour)
		plot_fuel_prices(run_dates, fuelprice, stack_colour)
		plot_coal_accuracy(comparison$dates, comparison$actual, comparison$model)
		plot_gas_accuracy(comparison$dates, comparison$actual, comparison$model)
		plot_carbon_accuracy(comparison$dates, comparison$actual, comparison$model)
		plot_price_accuracy(comparison$dates, comparison$actual, comparison$model)
		layout(1)
	}

	plot_capacity = function(dat, kap, stack_colour)
	{
		kap = kap / 1000
		kap$CCGT = kap$CCGTE + kap$CCGTF + kap$CCGTG + kap$CCGTH
		kap$Coal = kap$CoalOptOut + kap$CoalConver + kap$CoalRemain
		kap$Wind = kap$wind_onshore + kap$wind_offshore
		plot(dat, kap$demand, type='l', col=stack_colour$demand, lwd=2,
			ylim=c(0, max(1, max(kap) * 1.02)), xlab='', ylab='Operating capacity (GW)')
		lines(dat, kap$Biomass, col=stack_colour$Biomass)
		lines(dat, kap$Wind, col=stack_colour$WindOffshore)
		lines(dat, kap$solar, col=stack_colour$Solar)
		lines(dat, kap$CCGT, col=stack_colour$CCGT)
		lines(dat, kap$OCGT, col=stack_colour$OCGT)
		lines(dat, kap$Coal, col=stack_colour$CoalOptOut)
		axis(4, labels=FALSE)
	}

	plot_fuel_prices = function(dat, kst, stack_colour)
	{
		plot(dat, kst$Biomass, type='l', col=stack_colour$Biomass,
			ylim=c(min(0, min(kst)), max(1, max(kst) * 1.02)), xlab='', ylab='Fuel / carbon price')
		lines(dat, kst$CoalOptOut, col=stack_colour$CoalOptOut)
		lines(dat, kst$CCGTF, col=stack_colour$CCGT)
		lines(dat, kst$OCGT, col=stack_colour$OCGT)
		lines(dat, kst$carbon, col=stack_colour$Nuclear)
		axis(4, labels=FALSE)
	}


#####
## ##  6-PANEL DIAGNOSTIC PLOT
#####

	plot_coal_accuracy = function(dat, act, sim)
	{
		plot(dat, act$coal, type='l', ylim=c(0, max(act$coal, sim$coal)*1.02), xlab='', ylab='Coal TWh')
		lines(dat, sim$coal, lwd=2, col='#00880088')
		e = round(rmse(act$coal, sim$coal), 2)
		legend('bottomleft', 'RMSE = '%&%e, inset=c(-0.02, 0.02), bty='n', col=NULL)
		axis(4, labels=FALSE)
	}

	plot_gas_accuracy = function(dat, act, sim)
	{
		plot(dat, act$gas, type='l', ylim=c(0, max(act$gas, sim$gas)*1.02), xlab='', ylab='Gas TWh')
		lines(dat, sim$gas, lwd=2, col='#00880088')
		e = round(rmse(act$gas, sim$gas), 2)
		legend('bottomleft', 'RMSE = '%&%e, inset=c(-0.02, 0.02), bty='n', col=NULL)
		axis(4, labels=FALSE)
	}

	plot_carbon_accuracy = function(dat, act, sim)
	{
		plot(dat, act$carbon, type='l', ylim=c(0, max(act$carbon, sim$carbon)*1.02), xlab='', ylab='Carbon MT')
		lines(dat, sim$carbon, lwd=2, col='#00880088')
		e = round(rmse(act$carbon, sim$carbon), 2)
		legend('bottomleft', 'RMSE = '%&%e, inset=c(-0.02, 0.02), bty='n', col=NULL)
		axis(4, labels=FALSE)
	}

	plot_price_accuracy = function(dat, act, sim)
	{
		plot(dat, act$P50, type='l', ylim=range(0, act$P10, act$P90, sim$P10, sim$P90) + c(-1, 1), xlab='', ylab='Price (per MWh)')
		lines(dat, sim$P50, lwd=2, col='#00880088')

		plot_area(dat, act$P10, act$P90, border=NA, col='#0000002E')
		plot_area(dat, sim$P10, sim$P90, border=NA, col='#00880020')

		e = round(rmse(act$P50, sim$P50), 2)
		legend('bottomleft', 'RMSE = '%&%e, inset=c(-0.02, 0.02), bty='n', col=NULL)
		axis(4, labels=FALSE)
	}
