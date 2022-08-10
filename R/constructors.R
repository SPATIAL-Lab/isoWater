#####
#--- isotope value data frame ----
#creates object of 'iso' used to pass values to functions
#####

iso = function(H, O, Hsd, Osd, HOc = 0){
  v = data.frame(H=H, O=O, Hsd=Hsd, Osd=Osd, HOc=HOc)
  if(any(v$HOc > v$Hsd * v$Osd)){
    stop("Inconsistent SD and COV values")
  }
  if(any(rowSums(is.na(v)) > 0)){
    v = v[rowSums((is.na(v))) == 0,]
    warning("One or more samples include misisng values and were dropped")
  }
  class(v)[2] = "iso"
  return(v)
}

#####
#--- meteoric water line object ----
#creates statistical summary of meteoric water line based on sample values
#####

mwl = function(HO, plot = TRUE){
  if(!inherits(HO, "data.frame")){
    stop("HO must be a data.frame")
  }
  if(!is.numeric(HO[,1]) | !is.numeric(HO[,2])){
    stop("Non-numeric values in HO")
  }
  ns = nrow(HO)
  HO = HO[!(is.na(HO[,1]) | is.na(HO[,2])),]
  if(nrow(HO) != ns){
    warning("Missing values removed from HO")
  }
  if(ns < 3){
    stop("At least 3 sample values required")
  }
  if(ns < 10){
    warning("Small number of sample values, MWL may be poorly constrained")
  }
  
  meanH = mean(HO[,1])
  meanO = mean(HO[,2])
  
  slope = sd(HO[,1]) / sd(HO[,2])
  intercept = meanH - slope * meanO
  res = HO[,1] - (slope * HO[,2] + intercept)
  r2 = 1 - (var(res) / var(HO[,1]))
  if(r2 < 0.7){
    warning("Weakly correlated sample H and O values, MWL is poorly constrained")
  }
  
  sso = sum((HO[,2] - meanO)^2)
  rmse = sqrt(sum(res^2) / (ns - 2))
  
  if(plot){
    omar = par("mar")
    on.exit(par(mar = omar))
    par(mar = c(5,5,1,1))
    plot(HO[,2:1], xlab = expression(delta^{18}*"O"), 
         ylab = expression(delta^{2}*"H"))
    abline(intercept, slope)
    xs = seq(min(HO[,2] - 2), max(HO[,2] + 2), 
             by = (diff(range(HO[,2])) + 4) / 20)
    ys = sqrt(sum(res^2)/(ns - 2) * (1 + 1/ns + (xs - meanO)^2 / sso))
    lines(xs, xs * slope + intercept + ys, col = "grey")
    lines(xs, xs * slope + intercept - ys, col = "grey")
    points(HO[,2:1], pch = 21, bg = "white")
  }
  
  mwl = c(slope, intercept, meanO, sso, rmse, ns)
  class(mwl) = "mwl"
  return(mwl)
}
