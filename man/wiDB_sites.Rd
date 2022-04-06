\name{wiDB_sites}

\alias{wiDB_sites}

\title{
Obtain information on wiDB sites using a query 
}

\description{
Obtain information on wiDB sites using a query 
}

\usage{
wiDB_sites(minLat = NULL, maxLat = NULL, minLong = NULL, maxLong = NULL,
  minElev = NULL, maxElev = NULL, minDate = NULL, maxDate = NULL, 
  countries = NULL, states = NULL, types = NULL, projects = NULL)
}

\arguments{
  \item{minLat}{
    numeric. Minimum latitude for query region, in decimal degrees. South negative.
  }
  \item{maxLat}{
    numeric. Maximum latitude for query region, in decimal degrees. South negative.
  }
  \item{minLong}{
    numeric. Minimum longitude for query region, in decimal degrees. West negative.
  }
  \item{maxLong}{
    numeric. Maximum longitude for query region, in decimal degrees. West negative.
  }
  \item{minElev}{
    numeric. Minimum elevation for query. Meters.
  }
  \item{maxElev}{
    numeric. Maximum elevation for query. Meters.
  }
  \item{minDate}{
    character. Minimum date for query. Format: "YYYY-MM-DD"
  }  
  \item{maxDate}{
    character. Maximum date for query. Format: "YYYY-MM-DD"
  }
  \item{countries}{
    character. Vector of one or more two-letter country codes for query.
  }
  \item{states}{
    character. Vector of one or more two-letter state or province codes for query.
  }
  \item{types}{
    character. Vector of one or more sample types for query. See vocabulary in the \href{https://wateriso.utah.edu/waterisotopes/pages/spatial_db/contribute.html}{wiDB template}.
  }
  \item{projects}{
    character. Vector of one or more project codes for query.
  }
}

\value{
dataframe. Contains location and summary information for all wiDB sites returned by query. See \href{https://github.com/SPATIAL-Lab/isoWater/blob/master/Protocol.md}{here} for details.
}

\details{
One or more arguments must be provided.
}

\examples{
#Find all sites with tap water data since September, 2019
sites = wiDB_sites(minDate = "2019-09-01", types = "Tap")
}
