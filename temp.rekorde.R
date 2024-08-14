source("rekorde.R")
stats <- c("DIS","WFJ","DAV","ARO","CHU","RAG","VAD","STG","GUT","HAI","SHA",
	   "HLL","RUE","DEM","BAS","EBK","SAE","TAE","HOE","REH","KLO","ELM",
	   "GLA","WAE","SMA","EIN","GUE","ANT","ALT","ENG","LUZ","PIL","GRH",
	   "MER","JUN","INT","ABO","BER","GST","CHD","MLS","GRA","PLF","PAY",
	   "FRE","BRL","NEU","CHM","CHA","NAP","LAG","KOP","WYN","BUS","ULR",
	   "ZER","GRC","VIS","BLA","MVE","EVO","SIO","GSB","AIG","PUY","DOL",
	   "CGI","GVE","CDF","FAH","PIO","COM","SBE","GRO","MAG","CIM","OTL",
	   "LUG","SBO","BEH","ROB","SIA","COV","SAM","BUF","SCU","SMM")

rcy <- array(NA,c(length(stats),13))
for (i in 1:length(stats)) {
	stat <- stats[i]
	print(stat)
	data <- rekorde(station=stat,year=2024,month=1)
	if (!(is.null(data$ranks_curryear))) {
		rcy[i,1] <- data$ranks_curryear
	} else {
		rcy[i,1] <- NA
	}
	rcy[i,2] <- data$firstmeas
	rcy[i,4] <- data$value[1]
	rcy[i,5] <- as.numeric(substr(data$datetime[1],1,4))
	rcy[i,6] <- data$value[2]
        rcy[i,7] <- as.numeric(substr(data$datetime[2],1,4))
        rcy[i,8] <- data$value[3]
        rcy[i,9] <- as.numeric(substr(data$datetime[3],1,4))
        rcy[i,10] <- data$value[4]
        rcy[i,11] <- as.numeric(substr(data$datetime[4],1,4))
        rcy[i,12] <- data$value[5]
        rcy[i,13] <- as.numeric(substr(data$datetime[5],1,4))
}
rcy[,3] <- 2024-rcy[,2]+1

breaks_rk <- array(NA, c(7,2))
breaks_rk[1,] <- c(0.5,1.4)
breaks_rk[2,] <- c(1.5,2.4)
breaks_rk[3,] <- c(2.5,3.4)
breaks_rk[4,] <- c(3.5,5.4)
breaks_rk[5,] <- c(5.5,10.4)
breaks_rk[6,] <- c(10.5,20.4)
breaks_rk[7,] <- c(20.5,1000000)

breaks_lm <- array(NA, c(3,2))
breaks_lm[1,] <- c(-0.5,60.4)
breaks_lm[2,] <- c(60.5,100.4)
breaks_lm[3,] <- c(100.5,400)

countrec <- array(NA,c(7,3))
for (i in 1:7) {
	for (j in 1:3) {
		countrec[i,j]  <- 
			length(which(rcy[,1]>breaks_rk[i,1] & rcy[,1]<breaks_rk[i,2] &
			rcy[,3]>breaks_lm[j,1] & rcy[,3]<breaks_lm[j,2]))
	}
}

if (sum(countrec[1,]) > 0) {
	plural1 <- ""
	if (sum(countrec[1,]) > 1) {
		plural1 <- "en"
	}
	text01.1 <- paste0("An ",sum(countrec[1,])," Messstandort",plural1," wurde der 1. Rang seit Messbeginn registriert.")
	if (sum(countrec[1,]) <= 3) {
		# Up to 3 stations:
		# List stations with the respective values, 
		# the length of its series and former records
		ist <- which(rcy[,1]==1)
		st1 <- stats[ist]
		text01.2 <- rep("",length(ist))
		for (i in 1:length(ist)) {
			stname <- mchdwh::station_info(nat_abbr=st1[i])$station_name
			text01.2[i] <- paste0("Die Messstation ",stname," erreichte einen Wert von ",rcy[ist[i],4]," °C.")
			irec1 <- which(rcy[ist[i],c(4,6,8,10,12)]==rcy[ist[i],4])
			if (length(irec1) == 2) {
				text01.2[i] <- paste(text01.2[i]," Ein solcher Wert kam im Jahr ",rcy[ist[i],7]," bereits einmal vor.",sep="")
			}
			if (length(irec1) > 2) {
				text01.2[i] <- paste(text01.2[i]," Ein solcher Wert kam in der Vergangenheit bereits mehrmals vor.",sep="")
			}
			if (length(irec1) == 1) {
				text01.2[i] <- paste(text01.2[i]," Der bisherige Rekord am Standort ",stname," stammte aus dem Jahr ",rcy[ist[i],7]," und betrug ",rcy[ist[i],6]," °C.",sep="")
			}
		}
		text01.2 <- paste(text01.2,collapse=" ")
	} else {
		# More than 3 stations:
		# If the number of stations is equal to or smaller than 5
		# list them all in a sentence without adding values or measurement start. 
		# Give the number of stations in total that had a rank 1 and 
		# mention how many of these have more than 100 years of measurements
		# If the number of stations with > 100 years of measurements and rank 1
		# is 3 or lower, mention them all (again with the current value and the previous record)
		# of the number is > 3, then just mention the one station with the largest
		# deviation from its previous record
		if (sum(countrec[1,]) <= 5) {
			ist <- which(rcy[,1]==1)
			st1 <- stats[ist]
			stnames <- rep("",length(ist))
			for (i in 1:length(ist)) {
				stnames[i] <- paste0(mchdwh::station_info(nat_abbr=st1[i])$station_name," (Messbeginn ",rcy[ist[i],2],")")
			}
			tstats5 <- paste(stnames,collapse=", ")
			commapos <- as.numeric(gregexpr(pattern=", ",text=tstats5,fixed=T)[[1]])
			commapos <- commapos[length(commapos)]
			substring(tstats5,commapos,commapos+1) <- "--"
			tstats5 <- gsub("--"," und ",tstats5)
			text01.2 <- paste0("Die betroffenen Messstationen sind: ",tstats5)
		} else {
			ist1 <- which(rcy[,1]==1 & rcy[,3]>100)
			st11 <- stats[ist1]
			stnames1 <- mchdwh::station_info(nat_abbr=st11)$station_name
			if (length(ist1) == 1) {
				text01.2 <- paste0("Darunter war auch die Station ",stnames1," mit einer ",rcy[ist1,3],"-jährigen Messreihe.")
			}
			if (length(ist1) > 1) {
                        	text01.2 <- paste0("Darunter waren auch ",countrec[1,3]," Stationen mit über 100-jährigen Messungen.")
				if (length(ist1) <= 5) {
					tstats5 <- paste(stnames1,collapse=", ")
                        		commapos <- as.numeric(gregexpr(pattern=", ",text=tstats5,fixed=T)[[1]])
                        		commapos <- commapos[length(commapos)]
                        		substring(tstats5,commapos,commapos+1) <- "--"
                        		tstats5 <- gsub("--"," und ",tstats5)
                        		text01.2 <- paste0(text01.2," Die betroffenen Messstationen sind: ",tstats5)
				}
			}
			
		}
	}
}

text02 <- paste0("Ein 2. Rang wurde an insgesamt ",sum(countrec[2,])," Messstationen verzeichnet, ",countrec[2,3]," davon messen seit über 100 Jahren.")
text03 <- paste0("Den 3. Rang erreichten ",sum(countrec[3,])," Standorte.")
text04 <- paste0("Insgesamt wurde an ",sum(countrec[1:4,])," Messstandorten ein Rang unter den ersten 5 und an ",sum(countrec[1:5,])," Standorten ein Rang unter den ersten 10 erreicht.")

alltext <- paste(text01.1,text01.2,text02,text03,text04,collapse=" ")
print(alltext)

#w_rank <- 1/rcy[,1]
#w_measdur <- (2024-rcy[,2]+1)/max(2024-rcy[,2]+1)
#record_weight <- round(w_measdur*w_rank,2)
