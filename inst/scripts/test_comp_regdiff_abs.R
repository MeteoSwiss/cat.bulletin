comp_regdiff_abs <- function(parameter, vals) {
  
  if (parameter == "T") {
    # Fixed input for temperature
    absval <- "TT"
  }
  if (parameter == "P") {
    # Fixed input for precipitation
    absval <- "RR"
  }
  if (parameter == "S") {
    # Fixed input for sunshine duration
    absval <- "SS"
  }
  
  vcurr_all <- vals[[absval]][!is.na(vals[[absval]])]

  regs <- unique(vals$Region)
  vhighest <- 1
  vlowest <- 1
  for (r in 1:length(regs)) {
    indr <- which(vals$Region == regs[r])
    vhighest[r] <- quantile(abs(vals[[absval]][indr]),0.8,na.rm=TRUE)
    vlowest[r] <- quantile(abs(vals[[absval]][indr]),0.2,na.rm=TRUE)
  }
  member_h <- stats::cutree(hclust(dist(vhighest)),3)
  member_l <- stats::cutree(hclust(dist(vlowest)),3)
  mr_h <- 0
  mr_l <- 0
  for (i in 1:3) {
    indm_h <- which(member_h == i)
    mr_h[i] <- mean(vhighest[indm_h])
    indm_l <- which(member_l == i)
    mr_l[i] <- mean(vlowest[indm_l])
  }
  mhigh <- which(mr_h==max(mr_h))
  mhigh <- which(member_h == mhigh)
  regshigh <- collapse_sentence(regs[mhigh])
  # range of highest values: values of <valshigh> have locally been exceeded
  valshigh <- vhighest[mhigh]
  if (parameter != "T") {
    valshigh <- trunc(max(valshigh) / 10) * 10
  }
  mlow <- which(mr_l==min(mr_l))
  mlow <- which(member_l == mlow)
  regslow <- collapse_sentence(regs[mlow])
  # range of lowest values: values have locally been below <valslow>
  valslow <- vlowest[mlow]
  if (parameter != "T") {
    valslow <- ceiling(min(valslow / 10)) * 10  
  }
  
  # Output
  return(list(regshigh = regshigh, regslow = regslow, 
              valshigh = valshigh, valslow = valslow))
  
}
