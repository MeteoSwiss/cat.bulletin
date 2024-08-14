# Make bulletin plots and text
source('period.to.analyse.R')
source('bulletin.maps.R')
source('bulletin.desc.map.R')
source('bulletin.desc.ts.R')

# Period to analyse
dates <- anaperiod()

params <- c("T")
types  <- c("abs","anom")

for (p in params) {
	# Plot and describe time series (Swiss mean)
        if (p == "T") bulletin.desc.ts(param=p,ref_period=dates$ref_period,
				       enddate=dates$t.end.climtab,timespan=dates$timespan)

	# Plot station data

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
