\name{make.lifeline}
\alias{make.lifeline}
\title{make a lifeline matrix}
\usage{
  make.lifeline(startobs, endobs, agegroups)
}
\arguments{
  \item{startobs}{the date that eligibility for being
  observed starts (for example, the birth date)}

  \item{endobs}{the data that eligibility for being
  observed stops (including endobs itself)}

  \item{agegroups}{an agegroups object}
}
\value{
  a matrix representing a lifeline with the columns
  exp.start, exp.end, age.end, and exp.  the matrix's rows
  correspond to age groups, and the columns have the time
  of the start and end of exposure in each age group, the
  time with the highest age (whether or not it was
  reached), as well as the total amount of exposure
}
\description{
  make a lifeline matrix
}

