\name{plot.mort.surface}
\alias{plot.mort.surface}
\title{given a (two-dimensional) mortality surface,
produce a plot of it}
\usage{
  plot.mort.surface(surf, palcols = c("blue", "red"),
    main = NULL, xnames = NULL, ynames = NULL,
    xnames.freq = 1, ynames.freq = 5, key.entries = 10,
    xlab = NULL, ylab = NULL, keylab = "test",
    key.round = 1, key.log = TRUE, log = FALSE,
    grid = FALSE, res = 100, range = NULL)
}
\arguments{
  \item{surf}{TODO}

  \item{palcols}{TODO}

  \item{main}{TODO}

  \item{xnames}{TODO}

  \item{ynames}{TODO}

  \item{xnames.freq}{TODO}

  \item{ynames.freq}{TODO}

  \item{key.entries}{TODO}

  \item{xlab}{TODO}

  \item{ylab}{TODO}

  \item{keylab}{TODO}

  \item{key.round}{TODO}

  \item{key.log}{TODO}

  \item{log}{TODO}

  \item{grid}{TODO}

  \item{res}{TODO}

  \item{range}{TODO}
}
\description{
  created by dennis, july '09 taken from usmcod-lib.R and
  put here in feb 2010
}
\details{
  i found this page very useful in writing this function:
  http://www.phaget4.org/R/image_matrix.html

  TODO...  -> allow key at bottom instead of side -> handle
  logged labels for key (as an option -- currently unlogs
  automatically) -> handle passing in scale to use for
  everyone -> handle making each age-period a square ->
  (later? -- add overlay for age-period / cohorts ) -> turn
  key on / off -> permit multiple plots per page -> avoid
  having to have big plots to avoid error...  -> x label
  getting cut off...
}

