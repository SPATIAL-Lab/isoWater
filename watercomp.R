library(MASS)

sourceprob = function(vars){
  #vars = Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h, Hsig_h, Obar_h, Osig_h, HOcorr_h, Sbar_h, Ssig_h, graph
  
  ngens=5000
  
  H_o = rnorm(ngens, vars[1], vars[2])
  O_o = rnorm(ngens, vars[3], vars[4])
  HO_h = mvrnorm(ngens, c(vars[5], vars[7]), 
                 matrix(c(vars[6]^2, vars[9]*vars[6]*vars[8], vars[9]*vars[6]*vars[8], vars[8]^2),2,2))
  S_o = (H_o-HO_h[,1])/(O_o-HO_h[,2])
  
  if(graph){
    par(mai=c(1.3,1.3,0.5,0.5))
    plot(HO_h[,2],HO_h[,1],xlim=c(vars[7]-1, vars[7]+3), ylim=c(vars[5]-8, vars[5]+20), 
        xlab=expression(paste(delta^{18}, "O (\u2030)")),
        ylab=expression(paste(delta^{2}, "H (\u2030)")))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey")
    lines(c(vars[7]-1, vars[7]+2.5),c(vars[5]-8, vars[5]+20), lw=2)
    mapply(abline, a=H_o-S_o*O_o, b=S_o, col="red")
    points(HO_h[,2],HO_h[,1])
    points(O_o, H_o, pch=21, col="red", bg="white")
    lines(c(vars[7]-1, vars[7]+3),c(vars[5]-1*vars[10], vars[5]+3*vars[10]), col="black", lw=2)
    points(vars[7], vars[5], pch=21, bg="light grey")
    points(vars[3], vars[1], pch=21, col="red", bg="light grey")
  }
  
  Sprob_o = mean(dnorm(S_o, vars[10], vars[11]))
  return(Sprob_o)
}

mixprob = function(vars){
  #vars = 1:Hbar_o, 2:Hsig_o, 3:Obar_o, 4:Osig_o, 5:Hbar_h1, 6:Hsig_h1, 7:Obar_h1, 8:Osig_h1, 9:HOcorr_h1, 
  #10:Hbar_h2, 11:Hsig_h2, 12:Obar_h2, 13:Osig_h2, 14:HOcorr_h2, 15:Sbar_h, 16:Ssig_h, 17:graph
  
  ngens=5000

  H_o = rnorm(ngens, vars[1], vars[2])
  O_o = rnorm(ngens, vars[3], vars[4])
  HO_h1 = mvrnorm(ngens, c(vars[5], vars[7]), 
                matrix(c(vars[6]^2, vars[9]*vars[6]*vars[8], vars[9]*vars[6]*vars[8], vars[8]^2),2,2))
  HO_h2 = mvrnorm(ngens, c(vars[10], vars[12]), 
                matrix(c(vars[11]^2, vars[14]*vars[11]*vars[13], vars[14]*vars[11]*vars[13], vars[13]^2),2,2))

  mix_max = c(1:ngens)
  Hmax_h = c(1:ngens)
  Omax_h = c(1:ngens)
  for (i in 1:ngens){
    mix = seq(0,1,0.005)
    H_h = HO_h1[i,1]*mix + HO_h2[i,1]*(1-mix)
    O_h = HO_h1[i,2]*mix + HO_h2[i,2]*(1-mix)
    S_o = (H_o[i]-H_h)/(O_o[i]-O_h)
    Sprob_o = dnorm(S_o, vars[15], vars[16])
    topprob = which.max(Sprob_o)
    mix_max[i] = mix[topprob]
    Hmax_h[i] = HO_h1[i,1]*mix[topprob] + HO_h2[i,1]*(1-mix[topprob])
    Omax_h[i] = HO_h1[i,2]*mix[topprob] + HO_h2[i,2]*(1-mix[topprob])
  }

  if(graph){
    d = density(mix_max)
    plot(d)
    points(mean(mix_max), 0, col="blue")
    lines(c(mean(mix_max)-sd(mix_max), mean(mix_max)+sd(mix_max)), c(0,0), col="blue")
    m1 = (vars[10]-vars[5])/(vars[12]-vars[7])
    b1 = vars[5] - m1*vars[7]
    m2 = vars[15]
    b2 = vars[1] - m2*vars[3]
    x1 = (b2-b1)/(m1-m2)
    anymix = (vars[12] - x1)/(vars[12] - vars[7])
    points(anymix, 0, col="red")
    
    par(mai=c(1.3,1.3,0.5,0.5))
    plot(HO_h1[,2],HO_h1[,1],xlim=c(min(c(vars[7], vars[12]))-1,max(c(vars[7], vars[12]))+1), 
         ylim=c(min(c(vars[5], vars[10]))-8,max(c(vars[5], vars[10]))+8), 
         xlab=expression(paste(delta^{18}, "O (\u2030)")),
         ylab=expression(paste(delta^{2}, "H (\u2030)")))
    points(HO_h2[,2], HO_h2[,1])
    points(O_o, H_o, col="red")
    lines(c(min(c(vars[7], vars[12])), max(c(vars[7], vars[12]))),c(min(c(vars[5], vars[10])), max(c(vars[5], vars[10]))))
    lines(c(vars[3]-4, vars[3]+1),c(vars[1] - 4*vars[15], vars[1] + 1*vars[15]), col="red")
    points(Omax_h, Hmax_h, col="blue")
  }
  
  reslt = c(mean(mix_max), sd(mix_max), mix_max)
  return(reslt)
}

