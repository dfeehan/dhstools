\name{remap.contig}
\alias{remap.contig}
\title{remap a categorical variable to have
contiguous numerical values.}
\usage{
  remap.contig(x)
}
\arguments{
  \item{x}{the variable to remap, assumed to be a factor}
}
\value{
  a list whose elements contain \describe{ \item{vals}{the
  variable x with the underlying values made contiguous}
  \item{map}{a vector with the mapping from the old to the
  new levels} \item{len}{the number of levels}
  \item{orig}{the original variable that was passed in} }
}
\description{
  Given a factor, this function re-assigns numerical values
  to each level in such a way that the resulting values are
  sequential. For example, if the factor has levels "red",
  "green", and "blue" represented by 1, 10, and 15, this
  function would return another factor with levels "red",
  "green", and "blue" with values 1, 2, and 3. This is
  useful in preparing categorical variables for use with
  JAGS or Stan.
}

