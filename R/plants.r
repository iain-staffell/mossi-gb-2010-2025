#####
## ##  POWER STATION TYPES
#####
  #
  # tech holds the assumptions for each technology.
  # capacity_column, fuel_column and availability_column link these to our inputs.
  # unit_size determines the number of representative units at the maximum
  # installed capacity in the full input history. Their MW then scale together.

tech = list()

tech_groups = list(
	Coal = c('CoalOptOut', 'CoalConver', 'CoalRemain'),
	CCGT = c('CCGTE', 'CCGTF', 'CCGTG', 'CCGTH')
)


tech$Biomass = list(

	capacity_column = 'biomass',
	fuel_column = 'wood',
	availability_column = 'ocgt',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.40,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 7,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 120 * 0.40
)

tech$CoalOptOut = list(

	capacity_column = 'coal_to_lcpd',
	fuel_column = 'coal',
	availability_column = 'coal',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.36,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 3.00,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 344
)

tech$CoalConver = list(

	capacity_column = 'coal_to_biomass',
	fuel_column = 'coal',
	availability_column = 'coal',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.37,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 3.00,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 344
)

tech$CoalRemain = list(

	capacity_column = 'coal_other',
	fuel_column = 'coal',
	availability_column = 'coal',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.38,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 3.00,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 344
)

tech$CCGTE = list(

	capacity_column = 'ccgt_e',
	fuel_column = 'gas',
	availability_column = 'ccgt',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.48,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 5.00,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 205
)

tech$CCGTF = list(

	capacity_column = 'ccgt_f',
	fuel_column = 'gas',
	availability_column = 'ccgt',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.506,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 5.00,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 205
)

tech$CCGTG = list(

	capacity_column = 'ccgt_g',
	fuel_column = 'gas',
	availability_column = 'ccgt',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.529,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 5.00,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 205
)

tech$CCGTH = list(

	capacity_column = 'ccgt_h',
	fuel_column = 'gas',
	availability_column = 'ccgt',

	cap = NA,
	unit_size = 300,
	msg = 0.0,

	eff_mu = 0.530,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 5.00,
	varom_sd = 0.01,
	varom_xi = 1,

	fuel = NA,

	carbon = 205
)

tech$OCGT = list(

	capacity_column = 'peaking',
	fuel_column = 'oil',
	availability_column = 'ocgt',

	cap = NA,
	unit_size = 60,
	msg = 0.00,

	eff_mu = 0.35,
	eff_sd = 0.04,
	eff_xi = 1,

	varom_mu = 5.00,
	varom_sd = 0.00,
	varom_xi = 1,

	fuel = NA,

	carbon = 274
)


# when we go about plotting these,
# what colours should we use?
# plots look up colours by technology name
stack_colour = list(
	Nuclear      = '#4D9D57',
	Imports      = '#FF6180',
	Biomass      = '#FF912E',
	CoalConver   = '#4C4C4C',
	CoalRemain   = '#5C5C5C',
	CoalOptOut   = '#3C3C3C',
	CCGTE        = '#5F4A70',
	CCGTF        = '#705784',
	CCGTG        = '#826599',
	CCGTH        = '#9675B1',
	OCGT         = '#BE3A53',
	WindOffshore = '#0FB3D8',
	WindOnshore  = '#51C1DB',
	Solar        = '#FFD627',
	CCGT         = '#705784',
	demand       = 'black'
)


