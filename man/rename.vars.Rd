\name{rename.vars}
\alias{rename.vars}
\title{quick & dirty fn for renaming variable names...}
\usage{
  rename.vars(vnames, fromvars, tovars)
}
\arguments{
  \item{vnames}{the list of names to remap}

  \item{fromvars}{the current variable name}

  \item{tovars}{the new variable name (positions in this
  vector should correspond to the ones in fromvars)}
}
\value{
  a vector with \code{vnames} renamed according to the
  mapping from \code{fromvars} to \code{tovars}
}
\description{
  it may make sense to take this out and just use the
  \code{plyr::rename} function from the \code{plyr} package
}

