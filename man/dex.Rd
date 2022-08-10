\name{dex}

\alias{dex}

\title{
Deuterium excess
}

\description{
Calculates deuterium excess or line-conditioned excess.
}

\usage{
dex(HO, form = "dex", MWL = NULL)
}

\arguments{
  \item{HO}{
  data.frame. Hydrogen (column 1) and oxygen (column 2) isotope values for 1 or more water samples.
  }
  \item{form}{character. Calculate deuterium excess (\dQuote{dex}), line-conditioned excess (\dQuote{lcex}), or \dQuote{both}.}
  \item{MWL}{
  numeric. Vector the first two elements of which contain the meteoric water line slope and intercept (e.g., as created by \code{\link{mwl}}). The default value (if \code{MWL = NULL}) reflects the Global Meteoric Water Line estimated from a global precipitation compilation in Bowen, et al. (2019). Ignored for \code{form = "dex"}.
  }
}

\value{
Returns a copy of \code{HO} with an added field(s) \dQuote{dex} and/or \dQuote{lcex} containing the calculated values. Deuterium excess is calculated following Dansgaard (1964) as: \emph{dex = \eqn{\delta}2H - 8 * \eqn{\delta}18O}, and lc-excess following Landwehr & Coplen (2006) as \emph{lcex = \eqn{\delta}2H - m * \eqn{\delta}18O - b}, where \emph{m} and \emph{b} are the slope and intercept of \code{MWL}, respectively.
}

\references{
Bowen et al. (2019) Isotopes in the water cycle: Regional- to global-Scale patterns and applications. \emph{Annual Review of Earth and Planetary Sciences} \strong{47} 453--479. \doi{10.1146/annurev-earth-053018-060220}.

Dansgaard (1964) Stable isotopes in precipitation. \emph{Tellus} \strong{16} 436--468. \doi{10.1111/j.2153-3490.1964.tb00181.x}.

Landwehr & Coplen (2006) Line-conditioned excess: A new method for characterizing stable hydrogen and oxygen isotope ratios in hydrologic systems. In \emph{Isotopes in Environmental Studies}, International Atomic Energy Agency, 132--135. \url{http://www-pub.iaea.org/MTCD/publications/PDF/CSP_26_web.pdf}.
}

\examples{
O = runif(10, -15, -2)
H = O * 8 + 10 + rnorm(10, 0, 6)
d = dex(data.frame(H, O), form = "both")
print(d)
}
