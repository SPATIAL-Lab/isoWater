model {
  #Data model
  for(i in 1:nobs){
    o[i, 1:2] ~ dmnorm.vcov(c(h_pred, o_pred), o.vcov[(1 + (i-1) * 2):
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