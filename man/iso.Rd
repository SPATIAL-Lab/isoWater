\name{iso}

\alias{iso}

\title{
Iso Object
}

\description{
Creates objects of type "iso" 
}

\usage{
iso(H, O = 0, Hsd = 0, Osd = 0, HOc = 0)
}

\arguments{
  \item{H}{
  numeric or data.frame. If numeric, hydrogen isotope value or vector of hydrogen isotope values. If data.frame, data frame including five numeric fields that contain the H, O, Hsd, Osd, and HOc values (respectively). 
  }
  \item{O}{
  numeric. Oxygen isotope value or vector of oxygen isotope values.
  }
  \item{Hsd}{
  numeric. 1 standard deviation uncertainty of \code{H} (value or vector of values).
  }
  \item{Osd}{
  numeric. 1 standard deviation uncertainty of \code{O} (value or vector of values).
  }
  \item{HOc}{
  numeric. Covariance of \code{H} and \code{O} uncertainties (value or vector of values).
  }
}

\details{
  For numeric vector arguments, values in \code{Hsd}, \code{Osd}, and \code{HOc} are recycled if their length is shorter than that of \code{H} and \code{O}.
}

\value{
Returns an object of class \dQuote{iso}, a data.frame containing the provided values.
}

\examples{
obs = iso(-30, -5, 2, 0.2, 0.3)
str(obs)
}
