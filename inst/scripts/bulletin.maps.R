bulletin.maps <- function(param,type,begdate,enddate,status,timespan,refabbr) {
library(gridmch)
library(geocors)

products <- list()
products$T$abs$pname1 <- "TabsD"
products$T$abs$pname2 <- "TabsM"
products$T$abs$breaks <- c(seq(-18,0,2),seq(3,27,3))
products$T$anom$pname1 <- paste0("TanomD",refabbr)
products$T$anom$pname2 <- paste0("TanomM",refabbr)
products$T$anom$breaks <- c(seq(-7,-2,1),-1.5,-1,-0.5,0.5,1,1.5,seq(2,7,1))
## ADD LIST ELEMENTS FOR ALL OTHER VARIABLES (PREC, SUNSHINE)

fig.fname <- paste0(param,"_",type,"_",timespan,"_",refabbr)

# MAKE SURE PLOTS BELOW WORK FOR ALL PARAMETERS (AVERAGEING, ETC.)
# OUTPUT IS FIELD OF ONLY ONE PARAM/TYPE
# PROGRAM CALCULATIONS FOR SEASONS AS WELL

if (timespan == "m") {
	if (status == "current") {
		# current month: compute and plot average over all past days 
		# of the current month (until yesterday)
		product <- products[[param]][[type]]$pname1
		t.beg <- begdate
		t.end <- enddate
		t.format <- "yyyy.mm.dd"

		grd <- gridmch(pname=product,
		            t.beg=t.beg,t.end=t.end,t.format=t.format,
		            grid.name="ch01r.swiss.lv95",do.plot=FALSE,return.what="grid")
		agg <- apply(grd,FUN=mean,MARGIN=c(1,2))
		agg <- copy.grid.atts(from=grd,to=agg)
		attr(agg,"time") <- attr(grd,"time")[1]
		attr(agg,"grid.name") <- attr(grd,"grid.name")

		plot.spec <- plot.specs(appear="Intranet", pname=product,
		                        user.opts=list(plot.stats=FALSE,breaks=products[[param]][[type]]$breaks,
						       plot.title=FALSE,main.mid="",
						       fname.plot=fig.fname,plot.source=FALSE,
						       method.label="",plot.date=FALSE))
		do.call(what = "gridmch.plot",
			args = c(list(grid=agg,dat=NULL,pname=product,gr.format="pdf"),plot.spec))

		vdata <- agg
	} else {  
		#status == "past"
		m.beg <- substr(begdate,1,7)
		m.end <- m.beg
		t.format <- "yyyy.mm"

		product <- products[[param]][[type]]$pname2
		grd <- gridmch(pname=product,
			       t.beg=m.beg,t.end=m.end,t.format=t.format,
			       grid.name="ch01r.swiss.lv95",do.plot=FALSE,
			       return.what="grid")

		plot.spec <- plot.specs(appear="Intranet", pname=product,
                		        user.opts=list(plot.stats=FALSE,breaks=products[[param]][[type]]$breaks,
						       plot.title=FALSE,main.mid="",
						       fname.plot=fig.fname,plot.source=FALSE,
						       method.label="",plot.date=FALSE))
		do.call(what = "gridmch.plot",
			args = c(list(grid=grd,dat=NULL,pname=product,gr.format="pdf"),plot.spec))

		vdata <- grd
	}
} else { 
	# timespan == "s"
	if (status == "current") {
	} else {   
		# status == "past"
	}
}

return(vdata)

}
