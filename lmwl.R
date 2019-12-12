model {
  #Data model
  HO ~ dmnorm(pred.mu, HO.tau)
  pred.mu = c(h_pred, o_pred)
  
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