\name{mixSource}

\alias{mixSource}

\title{
Water Source as a Mixture
}

\description{
Given isotopic compositions of two or more potential sources, generate a posterior sample of source mixtures conditioned on one or more sample values.
}

\usage{
mixSource(obs, sources, slope, prior=rep(1,nrow(sources)), 
  shp = 1, edist = "unif", eprior = NULL, ngens = 1e5, ncores = 1)
}

\arguments{
  \item{obs}{
  \code{\link{iso}} object containing isotope values for one or more samples.
  }
  \item{sources}{
  \code{\link{iso}} object containing isotope values for two or more sources which may have contributed to the observed samples.
  }
  \item{slope}{
  numeric. Vector of length two specifying prior parameters for the evaporation line slope (mean, standard deviation).
  }
  \item{prior}{
  numeric. Vector of length equal to the number of sources, giving prior estimates of relative contributions of different sources.
  }
  \item{shp}{
  numeric. Shape parameter constant used in specifying prior estimates of source contributions (see Details). 
  }
  \item{edist}{
  character. One of \code{"unif"} or \code{"gamma"}, specifying whether the evaporation prior is modeled using a uniform or gamma distribution. 
  }
  \item{eprior}{
  numeric. Vector of length 2 giving prior parameter estimates for the oxygen isotope evaporation effect. For \code{edist = "unif"} these are maximum and minimum values and the default values are \code{c(0, 15)}. For \code{edist = "gamma"} these are shape and rate parameters, and the defaults are \code{c(1, 1)}.
  }
  \item{ngens}{
  integer. Number of posterior samples to obtain (per chain).
  }
  \item{ncores}{
  integer. Number of cores to use for parallel processing.
  }
}

\details{
The Dirichlet distribution is used to represent the fractional contribution of each source. The prior estimate is a Dirchlet where the shape parameter for source \code{i} is given by \code{prior[i] / min(prior) * shp}.

If \code{ncores} = 1, three chains will be run on a single core. If \code{ncores} > 1, \code{ncores} chains will be run in parallel on \code{ncores} cores. 
}

\value{
Returns an object of class \dQuote{mixSource}, a list containing:
\item{summary}{
matrix. Summary table of JAGS MCMC results, including parameter posterior distributions and convergence statistics.
}
\item{results}{
data.frame. Posterior samples of model parameters.
\describe{
\item{mixture_d2H}{Hydrogen isotopic composition of unevaporated source mixture.}
\item{mixture_d18O}{Oxygen isotopic composition of unevaporated source mixture.}
\item{sX_fraction}{Fractional contribution of each source.}
\item{S}{Evaporation line slope.}
\item{E}{Evaporation index, equal to the difference between the sample and unevaporated source mixture oxygen isotope values.}
}
}
}

\examples{
#Prep sources
O = runif(3, -15, -2)
H = O * 8 + 10 + rnorm(3, 0, 6)
sources = iso(H, O, 1, 0.2, 0.17)

#Sample data
obs = iso(-60, -6, 0.5, 0.1, 0)

#Evaporation slope
slope = c(5, 0.3)

#Run and report...likely not converged!
ws = mixSource(obs, sources, slope, ngens = 1e3)
ws$summary

#A traceplot
plot(ws$results$mixture_d18O[1:1000], type = "l")
lines(ws$results$mixture_d18O[1001:2000], col = 2)
lines(ws$results$mixture_d18O[2001:3000], col = 3)
}
