\name{simple.fwf}
\alias{simple.fwf}
\title{read in files that are in fixed width format}
\usage{
  simple.fwf(vardefs, filename, ...)
}
\arguments{
  \item{vardefs}{a matrix (or data frame) with cols
  "begin", "size", and "varname" with the starting column,
  the variable width, and its name}

  \item{filename}{the file (in fixed-width format) to read
  in}

  \item{...}{other args to be passed along to read.fwf}
}
\value{
  the dataframe from the fixed-width file
}
\description{
  takes a simple matrix locating the variables of interest
  (created, eg, as a .csv file in excel) and composes a
  vector of widths suitable to be passed into read.fwf
  NOTE: for now, this can't handle a situation where the
  vardefs contains multiple variables using the same
  columns (ie, a fips state and county code)
}

