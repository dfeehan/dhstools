\name{compute.occ.exp.old}
\alias{compute.occ.exp.old}
\title{tablulate occurrences and exposures}
\usage{
  compute.occ.exp.old(formula, intervals, data, doi = NULL,
    ages, start.obs, end.obs, na.action = na.pass,
    int.name = "year", age.name = "age", occ.weight = 1,
    exp.weight = 1, unique.exp = NULL,
    interval.names = NULL, age.names = NULL, exp.scale = 1)
}
\arguments{
  \item{formula}{a formula whose LHS is var that has the
  timing of events and whose RHS is covariatess (if any)}

  \item{intervals}{vector of CMC values that gives the time
  boundaries over which to compute events; for now, we
  assume that these are all of equal width. For example,
  intervals=seq(from=yr.to.cmc(years[1]),
  to=yr.to.cmc(max(years)+1), by=12) will use as time
  intervals the range of years in the vector called
  \code{years}. the intervals include the lower boundary,
  but not the upper one; eg, they have the form [a,b),
  [b,c), ...  Note that if \code{doi} is not NULL (see
  below), then these intervals are treated as time before
  the interview, so that the actual time period is
  different for each row.}

  \item{data}{the dataset to use}

  \item{doi}{if not NULL, the date of the interview for
  each row. in this case, the intervals are treated as
  years before the interview, so that the actual time
  period is potentially different for each row. (see the
  \code{intervals} argument, above)}

  \item{ages}{a vector with the ages. for example,
  ages=seq(from=0,to=60*12,by=12) would give single years
  of age last birthday in [0,60]. these intervals all have
  to be of equal width, and they can't be wider than the
  time intervals (see \code{intervals})}

  \item{start.obs}{vector of values (one per row of
  \code{data}) with the starting point of the observation
  window for each row, in CMC format}

  \item{end.obs}{vector of values (one per row of
  \code{data}) with the ending point of the observation
  window for each row, in CMC format}

  \item{na.action}{what action to take on missing values;
  defaults to \code{na.pass}}

  \item{int.name}{the name of the variable that has the
  time information; defaults to 'year'}

  \item{age.name}{the name of the variable that has the age
  information; defaults to 'age'}

  \item{occ.weight}{the weight to apply to occurrences;
  defaults to 1 (not yet implemented)}

  \item{exp.weight}{the weight to apply to exposures;
  defaults to 1 (not yet implemented)}

  \item{unique.exp}{tells which variable identifies unique
  units for computing exposure.  so, if the data is
  multiple records for units that experienced multiple
  events, this allows us to avoid overcounting exposure}

  \item{interval.names}{the name so of the time intervals}

  \item{age.names}{the names of the ages or age intervals}

  \item{exp.scale}{TODO}
}
\value{
  a list containing \describe{ \item{occ.exp}{an array of
  occurrences and exposures} \item{time.intervals}{the time
  intervals} \item{ages}{the ages} \item{exp.scale}{the
  amount by which exposure is scaled} }
}
\description{
  Given a variable indicating when an event happened, a
  time window we are interested in, and possibly a set of
  covariates, tabulate counts of event occurences and
  exposures in the given time interval.\cr Note that you
  have to be careful about observations that don't
  experience an event, but still count for exposure; see
  the example below. Also note that the current version can
  only handle age intervals of equal width and time
  intervals of equal width. (That is, the age and time
  intervals do not have to be the same as each other, but
  all of the age groups have to have one width and all of
  the time groups have to have another width.)
}
\details{


  \section{TODO} \itemize{ \item what if event date is
  missing?  \item what about things that vary with the
  event, eg mother's age when child was born?  \item
  sometimes, the order of the indices in the array isn't
  logical; eg, for a variable with levels {1,2,3,6,9}, the
  results might be ordered (2,1,6,3,9).  \item make this
  capable of handling different dates than just CMC
  codes... }
}
\examples{
## RECODE so that observations w/ no births show up
  ## in the dataset at least once by giving them a first
  ## birth at the (impossible) CMC code of -1. This ensures
  ## that they never contribute a birth, but that they
  ## still count for exposure.
  ##
  ## NB: this is a key step. if we don't do this,
  ## women who haven't had any births are removed
  ## from the dataset, biasing rates upward...

  bdata.coded <- bdata
  bdata.coded$bdate[ is.na(bdata.coded$bdate) &
                    bdata.coded$bnum == "01" ] <- -1
  bdata.coded <- subset(bdata.coded, ! is.na(bdate) )

  ## NO COVARIATES:
  ## now use compute.occ.exp to get counts of
  ## births and exposure between 1980 and 1990
  ## for ages 0 to 60
  singleyr.occ.exp <- compute.occ.exp(bdate ~ 1,
                                     data=bdata.coded,
                                     intervals=seq(from=yr.to.cmc(1980),
                                                   to=yr.to.cmc(1990+1),
                                                   by=12),
                                     interval.names=paste(years),
                                     start.obs=bdata.coded$dob,
                                     end.obs=bdata.coded$doi,
                                     ## NB: for now, have to pass ages in as
                                     ## months and include one more than we
                                     ## want results for
                                     ages=seq(from=0,to=61*12,by=12),
                                     age.names=paste(seq(from=0,to=61,by=1)),
                                     unique.exp="caseid",
                                     exp.scale=1/12)

  ## WITH COVARIATES:
  ##  use compute.occ.exp to get counts of
  ## births and exposure by 5-year period
  ## between 1970 and 2005,
  ## for 5-year age groups [0,5), ..., [60,65)
  ## by the covariates
  ## urban, highestedlevel, and religion
  ## (NOTE: this is just illustrative. we wouldn't recommend
  ##  substantively interpreting the results of this example.)

  gp.years <- seq(from=1970,to=2005,by=5)

   covars.occ.exp <- compute.occ.exp(bdate ~ urban + highestedlevel + religion,
                                     data=bdata.coded,
                                     intervals=seq(from=yr.to.cmc(gp.years[1]),
                                                   to=yr.to.cmc(max(gp.years)+5),
                                                   by=12*5),
                                     interval.names=paste(years),
                                     start.obs=bdata.coded$dob,
                                     end.obs=bdata.coded$doi,
                                     ## NB: for now, have to pass ages in as
                                     ## months and include one more than we
                                     ## want results for
                                     ages=seq(from=0,to=65*12,by=5*12),
                                     age.names=paste(seq(from=0,to=65,by=5)),
                                     unique.exp="caseid",
                                     exp.scale=1/12)
}

