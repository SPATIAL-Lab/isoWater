\name{mwl}

\alias{mwl}

\title{
Mwl Object
}

\description{
Creates objects of type "mwl" containing statistics for a meteoric water line in H and O isotope space.
}

\usage{
mwl(HO, plot = TRUE)
}

\arguments{
  \item{HO}{
  data.frame. Hydrogen (column 1) and oxygen (column 2) isotope values for 3 or more water samples.
  }
  \item{plot}{
  logical. Plot the data, MWL, and standard error of prediction?
  }
}

\details{
\code{mwl} will return an error if fewer than 3 sample values are provided and a warning if fewer than 10 samples are provided or if the correlation coefficient betwen H and O values is less than 0.7. Sample values should span a broad enough range of isotope values to strongly constrain the MWL.

Model II (reduced major axis) regression is used to accomodate errors on both isotope values.
}

\value{
Returns an object of class \dQuote{mwl}, a numeric vector containing meteoric water line statistics. See \code{\link{mwlSource}}.
}

\examples{
O = runif(10, -15, -2)
H = O * 8 + 10 + rnorm(10, 0, 6)
MWL = mwl(data.frame(H, O))
str(MWL)
}
