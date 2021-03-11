\name{wiDB_values}

\alias{wiDB_values}

\title{
Obtain value lists for categorical fields in wiDB
}

\description{
Obtain value lists for categorical fields in wiDB
}

\usage{
wiDB_values(fields)
}

\arguments{
  \item{fields}{
    character. One or more field names for which to obtain value lists. Limited to: "countries", "states", "types", and "projects".
  }
}

\value{
named list. Each element is a vector or dataframe containing values for the named field.
}

\examples{
#List all projects in the wiDB
wiDB_values("projects")
}
