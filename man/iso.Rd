\name{iso}

\alias{iso}

\title{
Iso Object
}

\description{
Creates objects of type "iso" 
}

\usage{
iso(H, O, Hsd, Osd, HOc = 0)
}

\arguments{
  \item{H}{
  numeric. Hydrogen isotope value or vector of hydrogen isotope values.
  }
  \item{O}{
  numeric. Oxygen isotope value or vector of oxygen isotope values.
  }
  \item{Hsd}{
  numeric. 1 standard devation uncertainty of \code{H} (value or vector of values).
  }
  \item{Osd}{
  numeric. 1 standard devation uncertainty of \code{O} (value or vector of values).
  }
  \item{HOc}{
  numeric. Covariance of \code{H} and \code{O} uncertainties.
  }
}

\value{
Returns an object of class \dQuote{iso}, a data.frame containing the provided values.
}

\examples{
obs = iso(-30, -5, 2, 0.2, 0.3)
str(obs)
}
