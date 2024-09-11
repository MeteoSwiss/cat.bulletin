# Define period to be analysed
anaperiod <- function(timespan="m",year=NULL,period=NULL,ref_period=c(1991,2020),day_thres=25) {
library(lubridate)

seasons <- array(NA, c(4,3))
seasons[1,] <- c(12,1,2)
seasons[2,] <- c(3,4,5)
seasons[3,] <- c(6,7,8)
seasons[4,] <- c(9,10,11)

status <- NULL

monthname <- c("Januar","Februar","März","April","Mai","Juni",
           "Juli","August","September","Oktober","November","Dezember")
seasonname <- c("Winter","Frühling","Sommer","Herbst")

# period = NULL --> automatic mode; for manual mode: provide month or season in function argument
if (is.null(period)) {
	mode <- "automatic"
} else {
	mode <- "manual"
	if (timespan == "m") {
	       if (!(period %in% c(1:12)) | !(year >= 1991)) {
	                stop("Wrong input: year must be a numeric >= 2000, 
			     period must be between 1 and 12 for month.")
	       }
	} else if (timespan == "s") {
                if (!(period %in% c(1:4)) | !(year >= 1991)) {
                        stop("Wrong input: year must be a numeric >= 2000,
                             period must be between 1 (winter), 2 (spring), 3 (summer) and 4 (fall) for season.")
               }
	} else {
		stop("Wrong input: timespan must be a character m for month or s for season.")
	}
	status <- "past"
}

current_date <- Sys.Date()
if (mode == "automatic") {
# Current date
day_of_month <- as.numeric(format(current_date,"%d"))
current_month<- as.numeric(format(current_date,"%m"))
current_year <- as.numeric(format(current_date,"%Y"))
prev_year    <- current_year - 1

if (day_of_month >= day_thres) {
	if (timespan == "m") {
	        # Analyse the current month
	        # Begin date: first day of current month
	        begdate <- format(floor_date(current_date,"month"),"%Y.%m.%d")
	        # End date: yesterday = current date - 1
	        enddate <- format(current_date - 1,"%Y.%m.%d")
		periodname <- monthname[as.numeric(substr(begdate,6,7))]
	}
	if (timespan == "s" & (current_month %in% seasons[,3])) {
                # Analyse the current season if we are in the last month of a season
                # Begin date: first day of current season
                begdate <- format(floor_date(current_date %m-% months(2),"month"),"%Y.%m.%d")
                # End date: yesterday = current date - 1
                enddate <- format(current_date - 1,"%Y.%m.%d")
		for (i in 1:4) {
			mm <- as.numeric(substr(begdate,6,7))
			if (mm %in% seasons[i,]) {
				periodname <- seasonname[i]
			}
		}
	}
	status <- "current"
} else {
	status <- "past"
	if (timespan == "m") {
	        # Analyse the previous month
	        # Begin date: first day of previous month
	        begdate <- format(floor_date(current_date %m-% months(1),"month"),"%Y.%m.%d")
	        # End date: last day of previous month
	        enddate <- format(ceiling_date(current_date %m-% months(1),"month") - 1,"%Y.%m.%d")
		periodname <- monthname[as.numeric(substr(begdate,6,7))]
	}
        if (timespan == "s") {
		# Analyse the previous season
		# Begin date: first day of previous season
		for (i in 1:4) {
			if (current_month %in% seasons[i,]) {
				current_season <- i
				if (current_season > 1) {
					prev_season <- i - 1
					begmon <- seasons[prev_season,1]
					endmon <- seasons[prev_season,3]
					if (begmon < 10) {
						begmon <- paste0("0",begmon)
					}
					if (prev_season == 1) {
						ybeg <- prev_year
						yend <- current_year
					} else {
						ybeg <- current_year
						yend <- current_year
					}
					begdate <- format(as.Date(paste0(ybeg,"-",begmon,"-01")),"%Y.%m.%d")
					enddate <- format(ceiling_date(as.Date(paste0(yend,"-",endmon,"-01")),"month") - 1,"%Y.%m.%d")
                                	periodname <- seasonname[prev_season]
				} else {
					prev_season <- 4
					if (current_month == 12) {
						ybeg <- current_year
						yend <- current_year
					} else {
						ybeg <- prev_year
						yend <- prev_year
					}
					begdate <- format(as.Date(paste0(ybeg,"-09-01")),"%Y.%m.%d")
					enddate <- format(as.Date(paste0(yend,"-11-30")),"%Y.%m.%d")
					periodname <- seasonname[4]
				}
			}
		}

	}
}
} else {
	#mode == manual
	#settings for month
	if (timespan == "m") {
		y1 <- year
		if (period < 10) {
			m1 <- paste0("0",period)
		} else {
		  m1 <- period
		}
	  periodname <- monthname[as.numeric(m1)]	
		begdate <- paste0(y1,".",m1,".01")
		enddate <- format(ceiling_date(as.Date(gsub("\\.","-",begdate)),"month") - 1,"%Y.%m.%d")
	}
	#settings for season
        if (timespan == "s") {
		if (period == 1) {
			begdate <- format(as.Date(paste0(year - 1,"-12-01")),"%Y.%m.%d")
			enddate <- format(ceiling_date(as.Date(paste0(year,"-02-01")),"month") - 1,"%Y.%m.%d")
		} else {
			m1 <- seasons[period,1]
			m2 <- seasons[period,3]
			if (m1 < 10) m1 <- paste0("0",m1)
			if (m2 < 10) m2 <- paste0("0",m2)
			begdate <- format(as.Date(paste0(year,"-",m1,"-01")),"%Y.%m.%d")
			enddate <- format(ceiling_date(as.Date(paste0(year,"-",m2,"-01")),"month") - 1,"%Y.%m.%d")
		}
		periodname <- seasonname[period]
	}
}

# Begin and end date for climate table
t.beg.climtab <- gsub(pattern="\\.",replace="",as.character(begdate))
t.end.climtab <- gsub(pattern="\\.",replace="",as.character(enddate))

refabbr <- paste0(substr(ref_period[1],3,4),substr(ref_period[2],3,4))

return(list(current_date=current_date, begdate=begdate, enddate=enddate, t.beg.climtab=t.beg.climtab, t.end.climtab=t.end.climtab, status=status, timespan=timespan, ref_period=ref_period, refabbr=refabbr, periodname=periodname))

}
