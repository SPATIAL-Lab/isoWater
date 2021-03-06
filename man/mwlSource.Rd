\name{mwlSource}

\alias{mwlSource}

\title{
Water Source Using Meteoric Water Line
}

\description{
Given parameters describing a meteoric water line in H-O isotope space, generate a posterior sample of unevaporated source water values conditioned on one or more sample values.
}

\usage{
mwlSource(obs, MWL=c(8.01, 9.57, -8.096, 2564532.2, 5.76, 80672), 
                     slope, stype = 1, ngens=1e4, ncores = 1)
}

\arguments{
  \item{obs}{
  \code{\link{iso}} object containing isotope values for one or more samples.
  }
  \item{MWL}{
  numeric. Vector of length 6 containing parameters describing a meteoric water line (see Details).
  }
  \item{slope}{
  numeric. Vector of length two specifying prior parameters for the evaporation line slope (mean, standard deviation).
  }
  \item{stype}{
  integer. Line statistic used to constrain the source prior: 1 = confidence interval, 2 = prediction interval (see Details).
  }
  \item{ngens}{
  integer. Number of posterior samples to obtain (per chain).
  }
  \item{ncores}{
  integer. Number of cores to use for parallel processing.
  }
}

\details{
The prior distribution of source values is constrained by \code{MWL}, which contains the parameters: slope, intercept, average d18O, sum of squares in d18O, root mean square error, and number of samples for an emperically-determined meteoric water line. This object can be created from a H and O isotope dataset using the fuction \code{\link{mwl}}. The default values reflect the Global Meteoric Water Line estimated from a global precipitation compilation in Bowen, et al. (2019). \code{stype} determines how the source uncertainty about the MWL is calculated; the default (1, confidence interval) is appropriate if the source is best represented as an integrated mixture of the samples defining the MWL, whereas option 2 (prediction interval) is appropriate if the source is best represented as a single sample.

If \code{ncores} = 1, three chains will be run on a single core. If \code{ncores} > 1, \code{ncores} chains will be run in parallel on \code{ncores} cores. 
}

\value{
list.
\item{summary}{
matrix. Summary table of JAGS MCMC results, including parameter posterior distributions and convergence statistics.
}
\item{results}{
data.frame. Posterior samples of model parameters.
\describe{
\item{source_d2H}{Hydrogen isotopic composition of unevaporated source.}
\item{source_d18O}{Oxygen isotopic composition of unevaporated source.}
\item{S}{Evaporation line slope.}
\item{E}{Evaporation index, equal to the difference between the sample and unevaporated source mixture oxygen isotope values.}
}
}
}

\references{
Bowen et al. (2019) Isotopes in the water cycle: Regional- to global-Scale patterns and applications. \emph{Annual Review of Earth and Planetary Sciences} \strong{47} 453--479. \doi{10.1146/annurev-earth-053018-060220}.
}

\examples{
#Prep MWL
O = runif(10, -15, -2)
H = O * 8 + 10 + rnorm(10, 0, 6)
MWL = mwl(data.frame(H, O), plot = FALSE)

#Sample data
obs = iso(-60, -6, 0.5, 0.1, 0)

#Evaporation slope
slope = c(5, 0.3)

#Run and report...likely not converged!
ws = mwlSource(obs, MWL, slope, ngens = 1e3)
ws$summary

#A traceplot 
plot(ws$results$source_d18O[1:1000], type = "l")
lines(ws$results$source_d18O[1001:2000], col = 2)
lines(ws$results$source_d18O[2001:3000], col = 3)
}
