\name{dct.to.vardefs}
\alias{dct.to.vardefs}
\title{takes (some) stata .dct (dictionary) files and
turns them into a vardef data frame suitable for
passing into simple.fwf}
\usage{
  dct.to.vardefs(filename, remove.duplicates = TRUE)
}
\arguments{
  \item{filename}{the .dct file}

  \item{remove.duplicates}{should variables defined to
  start at the same column be removed? if so, first
  variable using a given start column is chosen (currently,
  simple.fwf can't handle them) this defaults to TRUE.}
}
\value{
  the vardefs dataframe
}
\description{
  NOTE: this is by no means encyclopaedic. i've just tested
  this on the nicely-formatted .dct files that accompany
  the multiple cod files found at
  http://www.nber.org/data/vital-statistics-mortality-data-multiple-cause-of-death.html
  TODO * better solution for multiple variables * better
  error handling
}

