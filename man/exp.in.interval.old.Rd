\name{exp.in.interval.old}
\alias{exp.in.interval.old}
\title{compute the amount of exposure in a given interval}
\usage{
  exp.in.interval.old(exp.startobs, exp.endobs, int.begin,
    int.end)
}
\arguments{
  \item{exp.startobs}{the start of the exposure window}

  \item{exp.endobs}{the end of the exposure window}

  \item{int.begin}{the start of the time period}

  \item{int.end}{the end of the time period}
}
\value{
  the amount of exposure in the interval
}
\description{
  given the boundaries of a time period (in cmc format),
  this fn returns the amount of exposure each person
  contributes. so, it computes the amount of exposure in
  the period\cr [exp.startobs, exp.endobs)\cr that happens
  during\cr [int.begin, int.end)\cr TODO: \itemize{ \item{
  add unit tests } \item{ explain the two intervals and how
  they are used (ie, improve documentation)} } Note that if
  there is any missingness in the exp.startobs or
  exp.endobs variables, then this assumes no exposure.
}

