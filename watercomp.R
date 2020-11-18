#####
#--- preliminaries ----
#####

library(mvtnorm)
library(MCMCpack)
library(rjags)
library(R2jags)

#####functions here

#####
#--- isotope value data frame ----
#creates object of 'iso' data structure used to pass values to functions
#####

#takes values of H and O isotope composition, SD of each, and covariance
iso = function(H,O,Hsd,Osd,HOc){
  v = data.frame(H=H, O=O, Hsd=Hsd, Osd=Osd, HOc=HOc)
  return(v)
}

#####
#--- single source implementation ----
#####

#takes values of observed and hypothesized source water (each type 'iso'), hypothesized EL slope value
#prior probability of source, and number of parameter draws
sourceprob = function(obs, hsource, hslope, prior=1, ngens=10000){
  
  #ngens observed values
  HO_obs = rmvnorm(ngens, c(obs$H, obs$O), matrix(c(obs$Hsd^2, rep(obs$HOc*obs$Hsd*obs$Osd,2), obs$Osd^2),2,2))
  H_obs = HO_obs[,1]
  O_obs = HO_obs[,2]
  obs_prob = vector(,ngens)
  for(i in 1:ngens){
    obs_prob[i] = dmvnorm(c(H_obs[i], O_obs[i]),c(obs$H, obs$O), sigma=matrix(c(obs$Hsd^2, rep(obs$HOc*obs$Hsd*obs$Osd,2), obs$Osd^2),2,2))
  }
  
  #ngens hypothesized source values
  HO_h = rmvnorm(ngens, c(hsource$H, hsource$O), matrix(c(hsource$Hsd^2, rep(hsource$HOc*hsource$Hsd*hsource$Osd,2), hsource$Osd^2),2,2))
  H_h=HO_h[,1]
  O_h=HO_h[,2]
  hypo_prob = vector(,ngens)
  for(i in 1:ngens){
    hypo_prob[i] = dmvnorm(c(H_h[i], O_h[i]),c(hsource$H, hsource$O), sigma=matrix(c(hsource$Hsd^2, rep(hsource$HOc*hsource$Hsd*hsource$Osd,2), hsource$Osd^2),2,2))
  }
  
  #get slope and standardized source probability for each case
  S = (H_obs-H_h)/(O_obs-O_h)
  Sprob = dnorm(S, hslope[1], hslope[2])/dnorm(hslope[1], hslope[1], hslope[2])
  
  #if sample value lies below or left of source evaporation can't explain it
  msk = ifelse(H_h > H_obs | O_h > O_obs, 0, 1)
  Sprob = Sprob * msk
  goods = sum(msk)

  results = data.frame(H_h, O_h, hypo_prob, H_obs, O_obs, obs_prob, Sprob) #return data frame w/ all draws

  print(paste(goods, "out of", ngens))  #how many non-zero probabilities?

  return(results)
  
}

#####
#--- MWL source implementation ----
#####

#takes values of observed water (type 'iso'), MWL (see below), hypothesized EL slope value
#and number of parameter draws
mwlsource = function(obs, mwl=c(8.01, 9.57, 167217291.1, 2564532.2, -8.096, 80672), hslope, ngens=10000){
  #mwl contains parameters for the meteoric water line:
  #slope, intercept, sum of squares in d2H, sum of squares in d18O, average d18O, and number of samples.
  #Defaults were calculated from precipitation samples extracted from waterisotopes.org DB on 7/7/2017,
  #screened to include only samples with -10 < Dex < 30 

  #get number of observations
  nobs = nrow(obs)
  
  #stacked obs vcov matrix
  obs.vcov = matrix(nrow = nobs * 2, ncol = 2)
  for(i in 1:nobs){
    obs.vcov[1 + (i - 1) * 2, 1] = obs[i, 3] ^ 2
    obs.vcov[1 + (i - 1) * 2, 2] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 1] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 2] = obs[i, 4] ^ 2
  }
  
  #establish credible range for source water d18O
  o_cent = (mwl[2] - (obs$H - hslope[1] * obs$O) ) / (hslope[1]-mwl[1])
  var.x = mwl[4] / mwl[6]
  var.y = mwl[3] / mwl[6]
  var.xy = var.x * mwl[1]
  o_var = var.x - (var.xy / var.y) ^ 2 * var.y 
  o_var = o_var + (var.x - o_var) * hslope[1] / mwl[1]
  
  sr = sqrt((mwl[3] - (mwl[1]^2 * mwl[4]))/(mwl[6]-2))  ##sum of squares

  d = list(o_cent = o_cent, o_var = o_var, sr = sr,
           obs = obs, obs.vcov = obs.vcov, mwl = mwl,
           hslope = hslope, nobs = nobs)
  inits = list()
  for(i in 1:3){
    inits[[i]] = list("o_s" = rnorm(1, o_cent, abs(obs$O - o_cent) / 2))
  }  
  p = c("h_s", "o_s", "slp", "evap")

  n.iter = ngens * 1.1
  n.burnin = ngens * 0.1
  n.thin = floor((n.iter - n.burnin) / 2500)
  post = jags(d, NULL, p, lmwl, n.iter = n.iter, 
              n.burnin = n.burnin, n.thin = n.thin)

  sl = post$BUGSoutput$sims.list
  results = data.frame(sl$h_s, sl$o_s, sl$slp, sl$evap)
  #reassign names to results dataframe
  results@names = c("H_h", "O_h", "S", "E")

  return(list(mcmc_summary = post$BUGSoutput$summary, results = results))
}

#####
#--- Mixtures implementation ----
#####

#takes values of observed and hypothesized endmember source waters (each type 'iso'),hypothesized EL slope,
#prior (as relative contribution of each source to mixture), and number of parameter draws
mixprob = function(obs, hsource, hslope, prior=rep(1,nrow(hsource)), shp=2, ngens=100000){

  #get number of observations
  nobs = nrow(obs)
  
  #stacked obs vcov matrix
  obs.vcov = matrix(nrow = nobs * 2, ncol = 2)
  for(i in 1:nobs){
    obs.vcov[1 + (i - 1) * 2, 1] = obs[i, 3] ^ 2
    obs.vcov[1 + (i - 1) * 2, 2] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 1] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 2] = obs[i, 4] ^ 2
  }
  
  #get number of sources
  nsource = nrow(hsource)
  if(nsource < 2){
    stop("mixing model requires at least 2 sources")
  }
  
  #stacked source vcov matrix
  hsource.vcov = matrix(nrow = nsource * 2, ncol = 2)
  for(i in 1:nsource){
    hsource.vcov[1 + (i - 1) * 2, 1] = hsource[i, 3] ^ 2
    hsource.vcov[1 + (i - 1) * 2, 2] = hsource[i, 5]
    hsource.vcov[2 + (i - 1) * 2, 1] = hsource[i, 5]
    hsource.vcov[2 + (i - 1) * 2, 2] = hsource[i, 4] ^ 2
  }
  
  #dirchlet priors
  alphas = prior/min(prior) * shp
  
  #data
  d = list(nsource = nsource, nobs = nobs, 
           obs = obs, obs.vcov = obs.vcov,
           s = hsource, s.vcov = hsource.vcov,
           alphas = alphas, hslope = hslope)
  
  #parameters
  p = c("h_s", "fracs", "slp", "evap")

  #runit
  n.iter = ngens * 1.1
  n.burnin = ngens * 0.1
  n.thin = floor((n.iter - n.burnin) / 2500)
  post = jags(d, NULL, p, mixmod, n.iter = n.iter, 
              n.burnin = n.burnin, n.thin = n.thin)
  
  sl = post$BUGSoutput$sims.list
  results = data.frame(sl$fracs, sl$slp, sl$evap)
  #reassign names to results dataframe
  n = "s1_fraction"
  for(i in 2:nsource){
    nadd = paste0("s", i, "_fraction")
    n = c(n, nadd)
  }
  n = c(n, "S", "E")
  results@names = n
  
  return(list(mcmc_summary = post$BUGSoutput$summary, results = results))
}

mixmod = function(){
  #Data model
  for(i in 1:nobs){
    obs[i, 1:2] ~ dmnorm.vcov(c(h_pred, o_pred), obs.vcov[(1 + (i-1) * 2):
                                                        (2 + (i-1) * 2),])
  }
  
  #Process model
  h_pred = mix.h + evap * slp
  o_pred = mix.o + evap
  
  #evap prior
  evap ~ dunif(0, 15)
  
  #EL slope prior
  slp ~ dnorm(hslope[1], 1 / hslope[2] ^ 2)
  
  #mixture
  mix.o = sum(fracs * h_s[, 2])
  mix.h = sum(fracs * h_s[, 1])
  fracs ~ ddirch(alphas)
  
  #sources prior
  for(i in 1:nsource){
    h_s[i, 1:2] ~ dmnorm.vcov(s[i, 1:2], s.vcov[(1 + (i-1) * 2):
                                                  (2 + (i-1) * 2),])
  }
}

lmwl = function(){
  #Data model
  for(i in 1:nobs){
    obs[i, 1:2] ~ dmnorm.vcov(c(h_pred, o_pred), obs.vcov[(1 + (i-1) * 2):
                                                        (2 + (i-1) * 2),])
  }
  
  #Process model
  h_pred = h_s + evap * slp
  o_pred = o_s + evap
  
  #evap prior
  evap ~ dunif(0, 15)
  
  #EL slope prior
  slp ~ dnorm(hslope[1], 1 / hslope[2] ^ 2)
  
  #MWL source prior
  h_s ~ dnorm(o_s * mwl[1] + mwl[2], 1 / sy ^ 2) 
  sy = sr * sqrt(1 + 1 / mwl[6] + (o_s - mwl[5])^2 / mwl[4])
  o_s ~ dunif(o_min, o_max)
}