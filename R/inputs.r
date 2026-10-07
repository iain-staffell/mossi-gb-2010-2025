#####
## ##  LOAD AND PREPARE OUR INPUT DATA
#####
# All dispatch observations remain half-hourly. Day keys follow the calendar
# labels in the Electric Insights archive (which are stored with a UTC label).
# Keep the original row order: autumn clock changes can repeat timestamps.

load_mossi_inputs = function(capacity_file, price_file, ei_file)
{
	# read installed capacity
	cap = read.csv(capacity_file, stringsAsFactors=FALSE)
	names(cap) = tolower(names(cap))
	if (!all(c('date', 'ccgt_e', 'ccgt_f', 'ccgt_g', 'wind_onshore', 'wind_offshore', 'solar', 'nuclear') %in% names(cap)))
		stop('The capacity file is missing required columns.')
	cap$date = as.Date(cap$date, '%d/%m/%Y')
	if (anyNA(cap$date) || anyDuplicated(cap$date))
		stop('Capacity dates must be valid and unique.')
	cap = cap[order(cap$date), ]

	# read daily fuel and carbon prices (monthly values are repeated for each day)
	qep = read.csv(price_file, stringsAsFactors=FALSE)
	names(qep) = tolower(names(qep))
	price_columns = c('wood', 'coal', 'gas', 'oil', 'carbon_ets', 'carbon_cpf')
	if (!all(c('date', price_columns) %in% names(qep)) ||
		!all(vapply(qep[price_columns], is.numeric, logical(1))))
		stop('The price file must contain Date, Wood, Coal, Gas, Oil, Carbon_ETS and Carbon_CPF.')
	qep$date = as.Date(qep$date, '%d/%m/%Y')
	if (anyNA(qep$date) || anyDuplicated(qep$date))
		stop('Price dates must be valid and unique, with one row per day.')
	qep = qep[qep$date >= as.Date('2009-01-01'), ]
	qep$carbon = qep$carbon_ets + qep$carbon_cpf
	price_columns = c('wood', 'coal', 'gas', 'oil', 'carbon', 'carbon_ets', 'carbon_cpf')
	if (!all(price_columns %in% names(qep)) || !nrow(qep) ||
		any(!is.finite(as.matrix(qep[price_columns]))))
		stop('Fuel and carbon prices must have complete numeric columns.')

	# match daily prices to capacity dates; setup_mossi_period averages over each run period
	idx = match(cap$date, qep$date)
	prices = data.frame(date=cap$date, qep[idx, price_columns], row.names=NULL)

	# compact half-hourly data, with other supply and domestic emissions precomputed
	eiq = readRDS(ei_file)
	required = c('date', 'demand', 'onshore.wind', 'offshore.wind', 'wind',
		'solar', 'nuclear', 'other_supply', 'coal', 'gas', 'carbon.domestic', 'price.day')
	if (!is.data.frame(eiq) || !all(required %in% names(eiq)))
		stop('The half-hourly input must be a data frame containing: ', paste(required, collapse=', '))
	if (!inherits(eiq$date, 'POSIXt') || anyNA(eiq$date))
		stop('Input dates must be valid half-hourly timestamps.')
	value_columns = setdiff(required, 'date')
	if (!all(vapply(eiq[value_columns], is.numeric, logical(1))))
		stop('Half-hourly demand, generation, emissions and price columns must be numeric.')

	# group row numbers once, rather than scanning the full archive every day
	rows_by_day = split(seq_len(nrow(eiq)), as.Date(eiq$date, tz='UTC'))
	list(capacity=cap, prices=prices, actual=eiq, rows_by_day=rows_by_day,
		fuel_mean=c(coal=mean(qep$coal), gas=mean(qep$gas)))
}
