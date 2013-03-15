\name{harmonize.levels}
\alias{harmonize.levels}
\title{harmonize.levels}
\usage{
  harmonize.levels(fac1, fac2)
}
\arguments{
  \item{fac1}{the first factor}

  \item{fac2}{the second factor}
}
\description{
  given two factors, where the second factor has the same
  levels as the first, or where the second factor's levels
  are a superset of the first's, return a new factor to
  replace the first one whose levels are the same as the
  second's. so if we have\cr fac1 <- c("old", "young",
  "old") # levels are c("old", "young")\cr fac2 <-
  c("young", "old", "infant") # levels are c("infant",
  "young", "old")\cr then\cr res <- harmonize.levels(fac1,
  fac2)\cr will return a factor c("old", "young", "old")
  whose levels are c("infant", "young", "old")
}

