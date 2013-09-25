\name{same.remap}
\alias{same.remap}
\title{apply the same re-mapping that was used for one factor
to a different factor}
\usage{
  same.remap(x, mapping, new.to.na = FALSE)
}
\arguments{
  \item{x}{the variable to remap, assumed to be a factor}

  \item{mapping}{the mapping to use}

  \item{new.to.na}{if TRUE, then new levels not in the
  mapping get coded to NA; otherwise, they get assigned new
  numbers}
}
\value{
  a list whose elements contain \describe{
  \item{\code{vals}}{the variable x with the underlying
  values made contiguous} \item{\code{map}}{a vector with
  the mapping from the old to the new levels}
  \item{\code{len}}{the number of levels}
  \item{\code{orig}}{the original variable that was passed
  in} }
}
\description{
  apply the same re-mapping that was used for one factor to
  a different factor
}
\examples{
\dontrun{
  ## TODO add examples
}
}

