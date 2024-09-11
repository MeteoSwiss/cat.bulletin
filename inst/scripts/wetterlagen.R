library(mchdwh)

month <- "Februar"

data <- dwhget_surface(nat_abbr = "RHW",
                   param_short = "wkwtg3d0",
                   year = c(2024),month=2,meas_cat=1)

# 1-8 zyklonal
# 9-16 antizyklonal
# 17-24 indifferent
# 25 Tief
# 26 Hoch

# Dataframe "wetterlagen" erstellen mit Angaben zu 
# Richtung der Höhenströmung, Position Hoch/Tief zur Schweiz und allgemeine Witterung angeben pro Wetterlage angeben
bestimmt <- c("tiefdruck","hochdruck")
hoehenstroemung <- c("west","südwest","nordwest","nord","nordost","ost","südost","süd","west","südwest","nordwest","nord","nordost","ost","südost","süd","west","südwest","nordwest","nord","nordost","ost","südost","süd","tief","hoch")
position_ht <- c("Tief mit Zentrum nordöstlich der Schweiz","Tief mit Zentrum nördlich der Schweiz","Tief mit Zentrum östlich der Schweiz","Tief mit Zentrum östlich der Schweiz","Tief mit Zentrum südöstlich der Schweiz","Tief mit Zentrum südlich der Schweiz","Tief mit Zentrum südwestlich der Schweiz","Tief mit Zentrum westlich der Schweiz","Flaches Hoch südlich der Schweiz","Hoch östlich der Schweiz","Hoch südwestlich der Schweiz","Hoch westlich der Schweiz","Hoch nördlich der Schweiz","Hoch nordöstlich der Schweiz","Hoch östlich der Schweiz","Tief über Skandinavien","","","","","","Schweiz eingekesselt zwischen einem Hoch im Norden und einem Tief im Süden","Tief südwestlich der Schweiz","Tief westlich der Schweiz","Tief mit Zentrum über der Schweiz","Hoch mit Zentrum über der Schweiz")

wl <- data$value

wl[wl %in% c(1:8,25)] <- 1
wl[wl %in% c(9:16,26)] <- 2
wl[wl %in% 17:24] <- 3

wetterlagen <- cbind(data$value,wl)

rles <- rle(wl)

n <- 0
for (i in 1:3) {
	n[i] <- length(which(wl==i))
}

# Mehrheitlich tief- oder hochdruckbestimmt
if (n[1] > sum(n)/2) {
	text01.00 <- paste0("Der Monat war mehrheitlich ",bestimmt[1],"bestimmt.")
}
if (n[2] > sum(n)/2) {
        text01.00 <- paste0("Der Monat war mehrheitlich ",bestimmt[2],"bestimmt.")
}

# Im Einfluss des Tiefs

text01.01 <- paste0("Während ",length(which(wl==1))," Tagen des Monats lag die Schweiz im Einflussbereich eines Tiefs.")

i1 <- which(rles$lengths>4 & rles$values==1)
if (length(i1)>1) {
	plur01 <- "n"
} else {
	plur01 <- ""
	bd <- paste0(sum(rles$lengths[1:(i1-1)])+1,".")
	ed <- paste0(sum(rles$lengths[1:i1]),". ",month)
	phase01 <- paste0("Sie dauerte vom ",bd," bis am ",ed,".")
}
text02.01 <- paste0("Es gab ",length(i1)," mehrtägige Phase",plur01," mit Tiefdruckeinfluss.")
text02.01 <- paste(text02.01,phase01,sep=" ")

# Im Einfluss des Hochs

text01.02 <- paste0("An insgesamt ",length(which(wl==2))," Tagen dominierte Hochdruckwetter.")

i2 <- which(rles$lengths>4 & rles$values==2)
if (length(i2)>1) {
        plur02 <- "n"
	phase02 <- ""
	for (t2 in 1:length(i2)) {
		bd <- paste0(sum(rles$lengths[1:(i2[t2]-1)])+1,".")
        	ed <- paste0(sum(rles$lengths[1:i2[t2]]),". ",month)
		phase02 <- paste(phase02,paste0("Die ",t2,". Phase dauerte vom ",bd," bis am ",ed,"."),sep=" ")
	}
} else {
        plur02 <- ""
        bd <- paste0(sum(rles$lengths[1:(i2-1)])+1,".")
        ed <- paste0(sum(rles$lengths[1:i2]),". ",month)
        phase02 <- paste0("Sie dauerte vom ",bd," bis am ",ed,".")
}
text02.02 <- paste0("Es gab ",length(i2)," mehrtägige Phase",plur02," mit Hochdruckeinfluss.")
text02.02 <- paste(text02.02,phase02,sep=" ")

text01.03 <- paste0("Die Wetterlage über dem Alpenraum liess sich an ",length(which(wl==3))," Tagen des Monats nicht eindeutig zuordnen.")

if (n[1]==max(n)) {
	alltext <- paste(text01.00,text01.02,text02.02,text01.01,text02.01,text01.03)
}
if (n[2]==max(n)) {
        alltext <- paste(text01.00,text01.02,text02.02,text01.01,text02.01,text01.03)
}
print(alltext)
