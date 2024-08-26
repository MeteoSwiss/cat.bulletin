# Make bulletin plots and text
source('period.to.analyse.R')
source('set.stations.R')
source('bulletin.maps.R')
source('bulletin.desc.map.R')
source('bulletin.desc.ts.R')

# Period to analyse
dates <- anaperiod()
stations <- set.stations()

params <- c("T")
types  <- c("abs","anom")

for (p in params) {
	# Plot and describe time series (Swiss mean)
        bulletin.desc.ts(par=p,ref_period=dates$ref_period,
			 enddate=dates$t.end.climtab,timespan=dates$timespan)

	# Make station data table
	# based on clim.table
	bulletin.table(stations=stations,begdate=dates$begdate,enddate=dates$enddate,
		       refabbr=dates$refabbr)

        # Describe station data

	for (t in types) {
		# Plot maps
		map <- bulletin.maps(param=p,type=t,
				      begdate=dates$begdate,enddate=dates$enddate,
				      status=dates$status,timespan=dates$timespan,
				      refabbr=dates$refabbr)

		# Describe maps
		if (p == "T" & t == "anom") bulletin.desc.map(p,map,dates$periodname)
	}
}
