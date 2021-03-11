\name{wiDB_data}

\alias{wiDB_data}

\title{
Obtain data from wiDB using a query 
}

\description{
Obtain data from wiDB using a query 
}

\usage{
wiDB_data(minLat = NULL, maxLat = NULL, minLong = NULL, 
  maxLong = NULL, minElev = NULL, maxElev = NULL, minDate = NULL, 
  maxDate = NULL, countries = NULL, states = NULL, types = NULL, 
  projects = NULL, fields = NULL, tmpdir = tempdir(), clean = TRUE)
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
    character. Vector of one or more sample types for query. See vocabulary in the \href{http://wateriso.utah.edu/waterisotopes/pages/spatial_db/contribute.html}{wiDB tempate}.
  }
  \item{projects}{
    character. Vector of one or more project codes for query.
  }
  \item{fields}{
    character. Vector of one or more data fields to return from database. If omitted, returns all default fields. See \href{https://github.com/SPATIAL-Lab/wiDButil/blob/master/Protocol.md}{here} for details.
  }
  \item{tmpdir}{
    character. Directory path to use for unpacking data object.
  }
  \item{clean}{
    logical. Remove working files after data object is unpacked?
  }
}

\value{
named list. See \href{https://github.com/SPATIAL-Lab/wiDButil/blob/master/Protocol.md}{here} for details.
\item{data}{dataframe. Data records for isotope samples returned by query.}
\item{projects}{dataframe. Provenance information associated with samples returned by query.}
Note that some data are embargoed or have been shared under a license that prohibits redistribution. In dataframe \emph{data} values of 9999 indicate that a measurement is available but can't be obtained directly from the wiDB. Project information in dataframe \emph{projects} can be used to contact or visit the primary data source to learn about access to these data. Values of -9999 in \emph{data} indicate no measurement.
}

\details{
One or more arguments must be provided. 
}

\examples{
#Download data for US precipitation in the 1990s
vals = wiDB_data(minDate = "1990-01-01", maxDate = "2000-01-01", 
  countries = "US", types = "Precipitation")

#Download data for US Rivers and streams, requesting a subset of data fields
vals = wiDB_data(minDate = "1980-01-01", maxDate = "2000-01-01", 
  countries = "US", types = "River_or_stream", 
  fields = "Site_Name,Latitude,Longitude,d2H")
}
