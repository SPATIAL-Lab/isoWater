#####
#--- preliminaries ----
#####

source("watercomp.R")

#####
#--- illustrate MWL source implementation ----
#####

#sample values
obs = iso(-27, -3, 0.25, 0.05, 0)

#hypothesized slope value (slope, sd)
hslope = c(4.5, 0.3)

#demonstrate the prior distribution of sources
#ngens observed values
library(mvtnorm)
library(MASS)
ngens=5000

mwl=c(8.01, 9.57, 167217291.1, 2564532.2, -8.096, 80672)
HO_obs = rmvnorm(ngens, c(obs$H, obs$O), matrix(c(obs$Hsd^2, rep(obs$HOc*obs$Hsd*obs$Osd,2), obs$Osd^2),2,2))
H_obs = HO_obs[,1]
O_obs = HO_obs[,2]
obs_prob = vector(,ngens)
for(i in 1:ngens){  #bivariate normal probability density for each obs draw
  obs_prob[i] = dmvnorm(c(H_obs[i], O_obs[i]),c(obs$H, obs$O), sigma=matrix(c(obs$Hsd^2, rep(obs$HOc*obs$Hsd*obs$Osd,2), obs$Osd^2),2,2))
}

#ngens d18O from uniform distribution
o_min = -17
o_max = -2
O_h = o_min + runif(ngens) * (o_max - o_min)

#draw d2H using mwl variance
sr = sqrt((mwl[3] - (mwl[1]^2 * mwl[4]))/(mwl[6]-2))  ##sum of squares
sy = sr * sqrt(1 + 1 / mwl[6] + (O_h - mwl[5])^2 / mwl[4])  ##prediction standard error, e.g., http://science.widener.edu/svb/stats/regress.html and http://www.real-statistics.com/regression/confidence-and-prediction-intervals/
H_h = rnorm(ngens, O_h * mwl[1] + mwl[2], sy)
hypo_prob = dnorm(H_h, O_h * mwl[1] + mwl[2], sy)   #normal probability density for each hypo draw

#example data for plotting
examp = data.frame(H_h, O_h, hypo_prob, H_obs, O_obs, obs_prob)

#run the test case
results.list = mwlsource(obs, c(8.01, 9.57, 167217291.1, 2564532.2, -8.096, 80672), hslope, ngens = 1e6)
results = results.list$results

#some summary information
dens = kde2d(results$O_h, results$H_h, n=250)
mean(results$H_h)
mean(results$O_h)
quantile(results$H_h, c(0.05, 0.1, 0.5, 0.9, 0.95))
quantile(results$O_h, c(0.05, 0.1, 0.5, 0.9, 0.95))

#poor man's credible regions - show areas in H/O space containning highest X% of densities
dz = as.vector(dens$z)   #extract densities to vector
dzf = data.frame("indx"=seq(1:length(dz)), "val"=dz, "sm"=rep(0), "quant"=rep(0))  #bundle w/ other useful stuff
dzf = dzf[order(-dzf$val),]  #sort by gridcell density
rs=0
for(i in 1:nrow(dzf)){   #accumulate density
  rs = rs + dzf$val[i]
  dzf$sm[i] = rs
}
dzf$quant = ifelse(dzf$sm < 0.95*rs, 0.95, dzf$quant)   #classify using cum dens thresholds
dzf$quant = ifelse(dzf$sm < 0.90*rs, 0.90, dzf$quant)
dzf = dzf[order(dzf$indx),]   #reorder to original 
densq = dens
densq$z = matrix(dzf$quant, 250, 250)   #pop back into original structure

#set up plot space
jpeg("lmwl.jpg", units = "cm", res = 600, width = 7, height = 7)
#setEPS()
#postscript("Figure4.eps", width=8.4/2.54, height=8.4/2.54)
par(mai=c(0.5,0.5,0.1,0.1), cex=0.5)

#plot prior
nex = ngens
plot(examp$O_h[1:nex], examp$H_h[1:nex], xlab=expression(paste(delta^{18}, "O (\u2030)")), ylab=expression(paste(delta^{2}, "H (\u2030)")))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey", border = "black")
examp = examp[with(examp, order(hypo_prob*obs_prob)), ]
lcols = ( 0.9 - 0.9*(examp$obs_prob[1:nex] * examp$hypo_prob[1:nex])/max(examp$obs_prob[1:nex] * examp$hypo_prob[1:nex]))
lcols = mapply(max, lcols, 0)
abline(9.57, 8.01)
points(examp$O_h[1:nex],examp$H_h[1:nex], pch=21, col=rgb(lcols, lcols, lcols, max=1), bg="white")

#overlay posterior
points(results$O_h[1:nex],results$H_h[1:nex], pch=21, bg="white", col="blue")
points(quantile(results$O_h, 0.5), quantile(results$H_h, 0.5), pch=21, col="red", bg="red")   
xlim=c(par("usr")[1], par("usr")[2])
ylim=c(par("usr")[3], par("usr")[4])
contour(densq,  xlab="", ylab="", drawlabels=FALSE, levels=c(0.9, 0.95), col="red", add=TRUE)
points(obs$O, obs$H, pch=21, col="red", bg="black")    

dev.off()
