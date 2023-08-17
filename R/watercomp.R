#####
#--- MWL source implementation ----
#####

#takes values of observed water (type 'iso'), MWL (see below), hypothesized EL slope value
#and number of parameter draws
mwlSource = function(obs, MWL = NULL, slope, stype = 1, ngens=1e4, ncores = 1){

  if(is.null(MWL)){
    data("GMWL", envir = environment())
    GMWL = GMWL
    MWL = GMWL
  }
  
  if(MWL[6] == 80672 & stype == 2){
    warning("Using stype=2 and GMWL is inappropriate for most applications; see man")
  }
  if(!inherits(obs, "iso")){
    warning("Expecting iso object for obs, this argument may be formatted incorrectly")
  }
  if(!(stype %in% c(1, 2))){
    stop("stype must be 1 or 2, see ?mwlSource")
  }

  #get number of observations
  ndat = nrow(obs)
  
  #stacked obs vcov matrix
  obs.vcov = matrix(nrow = ndat * 2, ncol = 2)
  for(i in 1:ndat){
    obs.vcov[1 + (i - 1) * 2, 1] = obs[i, 3] ^ 2
    obs.vcov[1 + (i - 1) * 2, 2] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 1] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 2] = obs[i, 4] ^ 2
  }
  
  #establish credible range for source water d18O
  #center calculated as intersection of EL and MWL
  o_cent = (MWL[2] - (mean(obs$H) - slope[1] * mean(obs$O)) ) / (slope[1]-MWL[1])
  
  #large range of d18O values to evaluate
  o_eval = seq(o_cent - 25, o_cent + 25, by = 0.001)
  
  #sample or mean stats?
  if(stype == 1){
    MWL = c(MWL, 1 + 1 / MWL[6])
  } else{
    MWL = c(MWL, 1 / MWL[6])
  }
  
  #CI or PI bounds on MWL d2H
  h_MWL.bound = MWL[5] * sqrt(MWL[7] + (o_eval - MWL[3])^2 / MWL[4])
  h_MWL.min = MWL[1] * o_eval + MWL[2] - h_MWL.bound
  h_MWL.max = MWL[1] * o_eval + MWL[2] + h_MWL.bound
  
  #d2H values on mean EL
  h_el = mean(obs[,1]) + slope[1] * (o_eval - mean(obs[,2]))
  
  #Find se range for d18O
  o_keep = o_eval[h_el >= h_MWL.min & h_el <= h_MWL.max]
  o_var = diff(range(o_keep))^2

  d = list(o_cent = o_cent, o_var = o_var,
           obs = obs, obs.vcov = obs.vcov, MWL = MWL,
           slope = slope, ndat = ndat)
  
  p = c("source_d2H", "source_d18O", "S", "E")

  n.iter = ngens * 1.1
  n.burnin = ngens * 0.1
  n.thin = floor((n.iter - n.burnin) / 2500)
  n.thin = max(n.thin, 1)
  if(ncores > 1){
    tmf = tempfile(fileext = ".txt")
    tmfs = file(tmf, "w")
    cat(mwlModel, file = tmfs)
    close(tmfs)
    post = jags.parallel(data = d, parameters.to.save = p, 
                         model.file = tmf, 
                         n.iter = n.iter, n.chains = ncores, 
                         n.burnin = n.burnin, n.thin = n.thin)    
  } else{
    post = jags(data = d, parameters.to.save = p, 
                model.file = textConnection(mwlModel), n.iter = n.iter, 
                n.burnin = n.burnin, n.thin = n.thin)    
  }

  sl = post$BUGSoutput$sims.list
  results = data.frame(sl$source_d2H, sl$source_d18O, sl$S, sl$E)
  #reassign names to results dataframe
  results@names = c("source_d2H", "source_d18O", "S", "E")

  wcout = list(summary = post$BUGSoutput$summary, results = results)
  class(wcout) = "mwlSource"
  
  return(wcout)
}

#####
#--- Mixtures implementation ----
#####

#takes values of observed and hypothesized endmember source waters (each type 'iso'),hypothesized EL slope,
#prior (as relative contribution of each source to mixture), and number of parameter draws
mixSource = function(obs, sources, slope, prior=rep(1,nrow(sources)), 
                   shp=1, eprior = c(0, 15), ngens=1e5, ncores = 1){

  if(!inherits(obs, "iso")){
    warning("Expecting iso object for obs, this argument may be
            formatted incorrectly")
  }
  
  #get number of observations
  ndat = nrow(obs)
  
  #stacked obs vcov matrix
  obs.vcov = matrix(nrow = ndat * 2, ncol = 2)
  for(i in 1:ndat){
    obs.vcov[1 + (i - 1) * 2, 1] = obs[i, 3] ^ 2
    obs.vcov[1 + (i - 1) * 2, 2] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 1] = obs[i, 5]
    obs.vcov[2 + (i - 1) * 2, 2] = obs[i, 4] ^ 2
  }
  
  if(!inherits(sources, "iso")){
    warning("Expecting iso object for sources, this argument may be
            formatted incorrectly")
  }
  
  #get number of sources
  nsource = nrow(sources)
  if(nsource < 2){
    stop("mixing model requires at least 2 sources")
  }
  
  #stacked source vcov matrix
  sources.vcov = matrix(nrow = nsource * 2, ncol = 2)
  for(i in 1:nsource){
    sources.vcov[1 + (i - 1) * 2, 1] = sources[i, 3] ^ 2
    sources.vcov[1 + (i - 1) * 2, 2] = sources[i, 5]
    sources.vcov[2 + (i - 1) * 2, 1] = sources[i, 5]
    sources.vcov[2 + (i - 1) * 2, 2] = sources[i, 4] ^ 2
  }
  
  #dirchlet priors
  alphas = prior/min(prior) * shp
  
  #evap priors
  if(inherits(eprior, "numeric")){
    if(length(eprior) != 2){
      stop("eprior must be length 2")
    }
    if(any(eprior < 0)){
      stop("eprior values must be equal to or greater than zero")
    }
    if(any(eprior > 15)){
      message("eprior values greater than 15 are very unlikely in most systems")
    }
    eprior = sort(eprior)
    if(eprior[2] <= eprior[1]){
      eprior[2] = eprior[1] + 1e-3
    }
  } else{
    stop("eprior must be numeric")
  }
  
  #data
  d = list(nsource = nsource, ndat = ndat, 
           obs = obs, obs.vcov = obs.vcov,
           sources = sources, sources.vcov = sources.vcov,
           alphas = alphas, eprior = eprior, slope = slope)
  
  #parameters
  p = c("mixture_d2H", "mixture_d18O", "fracs", "S", "E")
  
  #run it
  n.iter = ngens * 1.1
  n.burnin = ngens * 0.1
  n.thin = floor((n.iter - n.burnin) / 2500)
  n.thin = max(n.thin, 1)
  if(ncores > 1){
    tmf = tempfile(fileext = ".txt")
    tmfs = file(tmf, "w")
    cat(mixModel, file = tmfs)
    close(tmfs)
    post = jags.parallel(data = d, parameters.to.save = p, 
                         model.file = tmf, 
                         n.iter = n.iter, n.chains = ncores, 
                         n.burnin = n.burnin, n.thin = n.thin)
  } else{
    post = jags(data = d, parameters.to.save = p, 
                model.file = textConnection(mixModel), 
                n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin)
  }

  sl = post$BUGSoutput$sims.list
  results = data.frame(sl$mixture_d2H, sl$mixture_d18O, sl$fracs, 
                       sl$S, sl$E)
  #reassign names to results dataframe
  n = c("mixture_d2H", "mixture_d18O")
  for(i in 1:nsource){
    nadd = paste0("s", i, "_fraction")
    n = c(n, nadd)
  }
  n = c(n, "S", "E")
  results@names = n
  
  wcout = list(summary = post$BUGSoutput$summary, results = results)
  class(wcout) = "mixSource"
  
  return(wcout)
}
