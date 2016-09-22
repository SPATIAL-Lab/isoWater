source("watercomp.R")

#--- sensitivity test hypothesized sources for surface-water evaporation case ----

#illustrate case
Obar_h = -7.0
Hbar_h = Obar_h*8+10
Hbar_o = -36.0
Obar_o = -5.0
Hsig_o = 0.25
Osig_o = 0.05
Hsig_h = 1.6
Osig_h = 0.2
HOcorr_h = 0.8
Sbar_h = 5.0
Ssig_h = 1.0
graph = 1

sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h, Hsig_h, Obar_h, Osig_h, HOcorr_h, Sbar_h, Ssig_h, graph))

#base case
Obar_h = seq(-13, -5, 0.05)
Hbar_h = Obar_h*8+10
prob = c(1:length(Obar_h))
graph = 0

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h, graph))
}

plot(Obar_h, prob, type ="l", ylim=c(0,0.65), 
     xlab=expression(paste("Hypothesized source water ",delta^{18}, "O (\u2030)")), ylab="Probability")

#higher measurement uncertainty
Hsig_o = 1.0
Osig_o = 0.1

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h))
}

lines(Obar_h, prob, col="red")

#better defined slope
Hsig_o = 0.25
Osig_o = 0.05
Ssig_h = 0.5

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h))
}

lines(Obar_h, prob, col="blue")

#measured source
HOcorr_h = 0.0
Hsig_h = Hsig_o
Osig_h = Osig_o
Ssig_h = 1.0

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h))
}

lines(Obar_h, prob, col="dark green")

lines(c(min(Obar_h),max(Obar_h)),c(0.05,0.05), lty=3)
legend(-13, 0.65, c("Base case", "Imprecise measurement", "Precise EL slope", "Measured source"), 
       col=c("black", "red", "blue", "dark green"), lty=1, bty="n")

#--- sensitivity test hypothesized sources for soil-water evaporation case ----

#illustrate scenario
Obar_h = -7.0
Hbar_h = Obar_h*8+10
Hbar_o = -40.0
Obar_o = -5.0
Hsig_o = 0.25
Osig_o = 0.05
Hsig_h = 1.6
Osig_h = 0.2
HOcorr_h = 0.8
Sbar_h = 3.0
Ssig_h = 1.0
graph = 1

sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h, Hsig_h, Obar_h, Osig_h, HOcorr_h, Sbar_h, Ssig_h, graph))

#base case
Obar_h = seq(-13, -5, 0.05)
Hbar_h = Obar_h*8+10
prob = c(1:length(Obar_h))
graph = 0

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h, graph))
}

plot(Obar_h, prob, type ="l", ylim=c(0,0.65),
     xlab=expression(paste("Hypothesized source water ",delta^{18}, "O (\u2030)")), ylab="Probability")

#higher measurement uncertainty
Hsig_o = 1.0
Osig_o = 0.1

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h))
}

lines(Obar_h, prob, col="red")

#better defined evap slope
Hsig_o = 0.25
Osig_o = 0.05
Ssig_h = 0.5

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h))
}

lines(Obar_h, prob, col="blue")

#measured source
HOcorr_h = 0.0
Hsig_h = Hsig_o
Osig_h = Osig_o
Ssig_h = 1.0

for(i in 1:length(Obar_h)){
  prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h))
}

lines(Obar_h, prob, col="dark green")

lines(c(min(Obar_h),max(Obar_h)),c(0.05,0.05), lty=3)
legend(-13, 0.65, c("Base case", "Imprecise measurement", "Precise EL slope", "Measured source"), 
       col=c("black", "red", "blue", "dark green"), lty=1, bty="n")

#--- sensitivity test evaporation offset for surface-water case ----

Obar_h = seq(-10, -5, 0.05)
Hbar_h = Obar_h*8+10
prob = c(1:length(Obar_h))
Hbar_o = -40.0
Obar_o = -5.0
Hsig_o = 0.25
Osig_o = 0.05
Hsig_h = 1.6
Osig_h = 0.2
HOcorr_h = 0.8
Sbar_h = 3.0
Ssig_h = 1.0
graph = 0

plot(c(min(Obar_h),max(Obar_h)), c(0,0.4), col="white",
     xlab=expression(paste("Hypothesized source water ",delta^{18}, "O (\u2030)")), ylab="Probability")

for(j in 1:12){
  Obar_o = -7+0.5*j
  Hbar_o = -46+0.5*j*3
  
  for(i in 1:length(Obar_h)){
    prob[i] = sourceprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h[i], Hsig_h, Obar_h[i], Osig_h, HOcorr_h, Sbar_h, Ssig_h, graph))
  }
  
  lines(Obar_h, prob)
}

lines(c(min(Obar_h),max(Obar_h)),c(0.05,0.05), lty=3)

#--- test source mixtures implementation ----

Hbar_o = -36.0
Hsig_o = 0.25
Obar_o = -5.0
Osig_o = 0.05
Obar_h1 = -10.0
Hbar_h1 = Obar_h1*8 + 10
Osig_h1 = 0.25
Hsig_h1 = 2.0
HOcorr_h1 = 0.8
Obar_h2 = -3.0
Hbar_h2 = Obar_h2*8 + 10
Osig_h2 = 0.25
Hsig_h2 = 2.0
HOcorr_h2 = 0.8
Sbar_h = 5.0
Ssig_h = 0.5
graph = 1

mix = mixprob(c(Hbar_o, Hsig_o, Obar_o, Osig_o, Hbar_h1, Hsig_h1, Obar_h1, Osig_h1, HOcorr_h1, Hbar_h2, Hsig_h2, 
                Obar_h2, Osig_h2, HOcorr_h2, Sbar_h, Ssig_h, graph))
mix[1]
mix[2]
