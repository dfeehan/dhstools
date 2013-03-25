############################################################
## occ-exp.R
##
## various functions that are useful for handling data
## on occurrences and exposures
############################################################

############################################################
##' make an age.groups object
##'
##' @param start the first age
##' @param widths the widths of the subsequent age groups
##' @param names the names of the age groups
##' @return an object (list) with the widths, names, and
##' number of age groups, as well as a matrix called
##' template which has the start and end of each age interval.
##' the intervals in template are close on the left but not
##' the right; eg, exp.start of 10 and exp.end of 20
##' means [10, 20) in terms of exact ages
##' (this is useful later on, for making individual age
##'  schedules based on, eg, birth dates)
make.age.groups <- function(start, widths, names) {

  lhs <- start + c(0, cumsum(widths[-length(widths)]))
  
  rhs <- lhs + widths 

  template <- cbind(exp.start=lhs, exp.end=rhs)

  if (is.null(names)) {
    names <- paste(lhs)
  } else if (length(names) > nrow(template)) {
    warning("too many names specified for age groups; truncating...")    
    names <- names[1:nrow(template)]
  }

  return(list(widths=widths,
              names=names,
              template=template,
              num.groups=nrow(template)))
}

############################################################
##' make a time.periods object
##'
##' @param start the start of the time of interest
##' @param durations the durations of the subsequent time periods
##' @param names the names of the time periods
##' @return an object (list) with the widths, names, and
##' number of time periods
##' as well as a matrix called
##' template which has the start and end of each time period.
##' the intervals in template are closed on the left but not
##' on the right; that is, start of 1900 and end of 1910
##' means [1900, 1910) in terms of exact times.
make.time.periods <- function(start, durations, names) {

  lhs <- start + c(0, cumsum(durations[-length(durations)]))
  
  rhs <- lhs + durations  

  template <- cbind(start=lhs, end=rhs)

  if (is.null(names)) {
    names <- paste(lhs)
  } else if (length(names) > nrow(template)) {
    warning("too many names specified for time periods; truncating...")
    names <- names[1:nrow(template)]
  }
  
  return(list(durations=durations,
              names=names,
              template=template,
              num.groups=nrow(template)))  
}

############################################################
##' make a lifeline matrix
##'
##' @param startobs the date that eligibility for being
##'                 observed starts (for example, the birth date)
##' @param endobs   the data that eligibility for being
##'                 observed stops (including endobs itself)
##' @param agegroups an agegroups object
##' @return a matrix representing a lifeline with the columns
##'         exp.start, exp.end, age.end, and exp.
##'         the matrix's rows correspond to
##'         age groups, and the columns have the time of the
##'         start and end of exposure in each age group, the
##'         time with the highest age (whether or not it was reached),
##'         as well
##'         as the total amount of exposure
make.lifeline <- function(startobs,
                          endobs,
                          agegroups) {

  ## NB: all of the entries in the lifeline are inclusive,
  ## on the left side, ie lifeline from a to b is
  ## [a,b) NOT (a,b) or (a,b] or (a,b).
  
  tl <- agegroups$template + startobs

  tl.exp <- exp.in.interval(exp.start=tl[,1],
                            exp.end=tl[,2],
                            int.begin=-Inf,
                            int.end=endobs)

  age.end <- tl[,2]
  
  ## if exposure wasn't for the entire interval,
  ## reduce the end time accordingly
  tl[,2] <- tl[,2] - ((tl[,2]-tl[,1]) - tl.exp)

  ## TODO -- should we zero-out age groups for which there is
  ##         no exposure? not sure this is necessary, for now,
  ##         but might want to come back to this.
  ##return(cbind(tl, exp=tl.exp, age=agegroups$names))
  res <- cbind(tl, age.end=age.end, exp=tl.exp)
  colnames(res) <- c("exp.start", "exp.end", "age.end", "exposure")

  return(res)

}

### TODO -- prelim draft of these fns looks ok. next
###    * be sure that the [,) thing works in edge cases,
###      develop tests, and think about what the right
###      behavior should be for events instead of
###      intervals
###    * think about how to integrate these into bigger
###      picture rewrite of compute.occ.exp; need to
###      construct each alter's lifelines, then
###      what steps to get aggregated counts?
###    * remember to incorporate weights...
##age.gps <- make.age.groups(start=0, widths=rep(5,10),
##                           names=paste(seq(from=0, to=45, by=5)))

##make.lifeline(100, 128, age.gps)

############################################################
##' compute whether or not events fell in a given interval
##'
##' given the boundaries of a time period and a vector
##' of event times,
##' this fn returns the count of events that happened
##' within the time period. so, given a time period\cr
##' [exp.start, exp.end)\cr
##' and events\cr
##' (event1, event2, ...)\cr
##' this function returns a vector with 1's corresponding
##' to events that took place in the time window and 0 otherwise
##' TODO:
##' \itemize{
##'    \item{ add unit tests }
##'    \item{ explain the time intervals and how they are used
##'          (ie, improve documentation)}
##' }
##' Note that if there is any missingness in the
##' exp.start or exp.end variables, then this
##' assumes no exposure.
##' 
##' @param exp.start the start of the exposure window
##' @param exp.end the end of the exposure window
##' @param events the time of the events
##' @return a vector of the same length of events; the entries
##'         are 1 for each event that occurred in the given time
##'         window and 0 otherwise
##' @export
events.in.interval <- function(exp.start,
                               exp.end,
                               events)
{

  return(as.numeric(events >= exp.start &
                    events < exp.end))

}

############################################################
##' compute the amount of exposure in a given interval
##'
##' given the boundaries of a time period (in cmc format),
##' this fn returns the amount of exposure each person
##' contributes. so, it computes the amount of exposure
##' in the period\cr
##' [exp.start, exp.end)\cr
##' that happens during\cr
##' [int.begin, int.end)\cr
##' TODO:
##' \itemize{
##'    \item{ add unit tests }
##'    \item{ explain the two intervals and how they are used
##'          (ie, improve documentation)}
##' }
##' Note that if there is any missingness in the
##' exp.start or exp.end variables, then this
##' assumes no exposure.
##' 
##' @param exp.start the start of the exposure window
##' @param exp.end the end of the exposure window
##' @param int.begin the start of the time period
##' @param int.end the end of the time period
##' @return the amount of exposure in the interval
##' @export
exp.in.interval <- function(exp.start,
                            exp.end,
                            int.begin,
                            int.end )
{

  ## compute start (min) and end (max)
  ## of exposure in this interval
  i.exp   <- rep(0, length(exp.start))
  thismin <- rep(0, length(exp.start))
  thismax <- rep(0, length(exp.start))

  ## respondents who started observation before the end of the interval
  ## and who ended observation after the beginning of the interval
  ## defining this time period contributed some amt of exposure
  anyexp <- (exp.start <= exp.end) &
            (exp.start < int.end) &
            (exp.end > int.begin) &
            (int.begin <= int.end)
  ##anyexp <- (exp.start <= exp.end) &
  ##          (exp.start <= int.end) &
  ##          (exp.end >= int.begin) &
  ##          (int.begin <= int.end)  

  ## for now, if startobs / endobs not known, ignore
  anyexp[ is.na(anyexp) ] <- FALSE

  thismin <- pmax( exp.start, int.begin )
  thismax <- pmin( exp.end, int.end )

  i.exp[ anyexp ] <- thismax[anyexp] - thismin[anyexp] 

  return( i.exp )

}

##########################################################
##' tablulate occurrences and exposures
##'
##' Given a variable indicating when an event happened,
##' a time window we are interested in, and possibly a set
##' of covariates, tabulate counts of event occurences and
##' exposures in the given time interval.\cr
##' Note that you have to be careful about observations that
##' don't experience an event, but still count for exposure;
##' see the example below.
##' Also note that the current version can only handle
##' age intervals of equal width and time intervals of
##' equal width. (That is, the age and time intervals do
##' not have to be the same as each other, but all of the
##' age groups have to have one width and all of the time
##' groups have to have another width.)
##' 
##' 
##'
##' \section{TODO}
##' \itemize{
##'    \item what if event date is missing?
##'    \item what about things that vary with the event, eg mother's
##'                  age when child was born?
##'    \item sometimes, the order of the indices in the array isn't logical;
##'                  eg, for a variable with levels {1,2,3,6,9}, the results might be
##'                  ordered (2,1,6,3,9).
##'    \item make this capable of handling different dates than
##'         just CMC codes...
##' }
##'
##' @examples
##'
##'   ## RECODE so that observations w/ no births show up
##'   ## in the dataset at least once by giving them a first
##'   ## birth at the (impossible) CMC code of -1. This ensures
##'   ## that they never contribute a birth, but that they
##'   ## still count for exposure.
##'   ##
##'   ## NB: this is a key step. if we don't do this,
##'   ## women who haven't had any births are removed
##'   ## from the dataset, biasing rates upward...
##' 
##'   bdata.coded <- bdata
##'   bdata.coded$bdate[ is.na(bdata.coded$bdate) &
##'                     bdata.coded$bnum == "01" ] <- -1
##'   bdata.coded <- subset(bdata.coded, ! is.na(bdate) )
##'
##'   ## NO COVARIATES:
##'   ## now use compute.occ.exp to get counts of
##'   ## births and exposure between 1980 and 1990
##'   ## for ages 0 to 60
##'
##'   ## TODO -- need to write this example using new
##'   ##         version
##'
##'   ## WITH COVARIATES:
##'   ##  use compute.occ.exp to get counts of
##'   ## births and exposure by 5-year period
##'   ## between 1970 and 2005,
##'   ## for 5-year age groups [0,5), ..., [60,65)
##'   ## by the covariates
##'   ## urban, highestedlevel, and religion
##'   ## (NOTE: this is just illustrative. we wouldn't recommend
##'   ##  substantively interpreting the results of this example.)
##'
##'   ## TODO -- need to write this example using new
##'   ##         version
##' 
##' @param formula a formula whose LHS is var that has the timing of events
##'                and whose RHS is covariatess (if any)
##' @param age.groups an age.groups object
##' @param time.periods a time.periods object
##' @param data the dataset to use
##' @param doi if not NULL, the date of the interview for each row. in this case,
##'            the intervals are treated as years before the interview, so that the
##'            actual time period is potentially different for each row. (see the
##'            \code{intervals} argument, above)
##' @param start.obs vector of values (one per row of \code{data}) with the starting point
##'                  of the observation window for each row, in CMC format
##' @param end.obs   vector of values (one per row of \code{data}) with the ending point
##'                  of the observation window for each row, in CMC format
##' @param na.action what action to take on missing values; defaults to \code{na.pass}
##' @param weight the weight to apply to occurrences and exposures; defaults to 1
##' @param unique.exp tells which variable identifies unique units for computing exposure.
##'                   so, if the data is multiple records for units that experienced
##'                   multiple events, this allows us to avoid overcounting exposure
##' @param exp.scale TODO
##' @return a list containing
##' \describe{
##'    \item{occ.exp}{an array of occurrences and exposures}
##'    \item{time.periods}{the time intervals}
##'    \item{age.groups}{the ages}
##'    \item{exp.scale}{the amount by which exposure is scaled}
##' }
##' 
##' @export
compute.occ.exp <- function(formula,
                            data,
                            doi=NULL,
                            age.groups,
                            time.periods,
                            start.obs,
                            end.obs,
                            na.action=na.pass,
                            weights=NULL,
                            unique.exp=NULL,
                            exp.scale=1)
{

    ## TODO -- LEFT OFF HERE
    ##   -> be sure this can still handle time frames that
    ##      are defined as being X amt of time before interview
    ##      (ie, time frame varies for each respondent)
    ##   -> develop unit tests...
    ##      be sure to test
    ##        - weights
    ##        - unique.exp
    ##        - fixed time window
    ##        - X years before interview window
    ##   -> handling NAs?
    ##   -> would be nice to have a feature which lets
    ##      missingness in event variable automatically
    ##      be treated as no event (ie replaced by -1)
  
    ## figured this out using the
    ## code for lm(...) as a model

    ## first, call model.frame on the
    ##   formula and data we were passed in
    mf <- match.call()

    arg.idx <- match( c("formula","data","subset","na.action"),
                      names(mf), 0L )

    ## use model.frame to get a matrix with all of the covariates
    mf <- mf[c(1,arg.idx)]
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())

    mf.na <- attr(mf, "na.action")

    if (! is.null(mf.na)) {
      idx.touse <- (1:nrow(data))[-as.vector(mf.na)]
    } else {
      idx.touse <- 1:nrow(data)
    }
    
    covar.names <- colnames(mf)[-1]
    
    ## get the weights
    weights <- get.weights(data[idx.touse,], weights)
    mf$.weight <- weights
            
    ## unique.exp is a variable that tells which variable
    ## identifies unique units for computing exposure. so, if
    ## the data is multiple records for units that experienced
    ## multiple events, this allows us to avoid overcounting
    ## exposure
    if (! is.null(unique.exp)) {
      ## this picks (arbitrarily) one rowname for each unique
      ## value of the variable unique.exp
      ## eg if we have respondent id, this would only pick
      ## one row from each respondent to avoid overcounting
      ## exposure
      mf$.exp.counts <- as.numeric(! duplicated(data[idx.touse,unique.exp]))
    } else {
      ## if nothing specified, just take all of the rows
      ## in the data passed in
      mf$.exp.counts <- 1
    }
    
    events <- model.response(mf, "numeric")
    
    ## for now, we'll use the row indices in the data as
    ## our internal id for each row
    mf$.internal_id <- 1:nrow(mf)
    ids <- mf$.internal_id
    names(ids) <- paste(ids)

    names(start.obs) <- names(ids)
    names(end.obs) <- names(ids)
    names(events) <- names(ids)

    ## create a lifeline, describing exposure by age and time,
    ## for each row in the dataset
    all.ll <- llply(seq_along(start.obs),
                    function(idx) {
                      res <- make.lifeline(start.obs[idx],
                                           end.obs[idx],
                                           age.groups)
                    })
    names(all.ll) <- ids

    ## this step combines the lifelines and the time periods; it produces
    ## a list with an entry for each time period and, in each entry, another list
    ## with a lifeline matrix for each row in the dataset. the lifeline matrix
    ## summarizes how much exposure each row had in each age group during the
    ## given time period
    all.exp <- alply(time.periods$template,
                     1,
                     function(this.timerow) {

                       ## for each alter and each age, figure out how
                       ## much exposure there was in this time period
                       time.exp <- llply(seq_along(names(all.ll)),
                                         function(this.ll.id) {

                                           this.ll <- all.ll[[this.ll.id]]

                                           ## get exposure
                                           res.e <- mdply(this.ll[,c("exp.start",
                                                                   "exp.end")],
                                                        .fun=exp.in.interval,
                                                        int.begin=this.timerow["start"],
                                                        int.end=this.timerow["end"])
                                           
                                           ## get events
                                           ## note that we use the end of the age
                                           ## interval and not the end of the
                                           ## exposure interval to detect events
                                           ## w/in age groups here. this is because
                                           ## the fact that our exposure intervals
                                           ## are half-open means that we'll ever
                                           ## detect an event at d in an interval
                                           ## from [b, d)
                                           res.o <- events.in.interval(this.ll[,"exp.start"],
                                                                       this.ll[,"age.end"],
                                                                       events[this.ll.id])
                                           res <- data.frame(.exposure=res.e$V1)
                                           res$.occ <- res.o
                                           res$.internal_id <- this.ll.id
                                           res$.age <- age.groups$names

                                           return(res)
                                         })
                       return(time.exp)                                         
                     })
    names(all.exp) <- time.periods$names

    ## this step aggregates all of the exposures during different time
    ## periods to one big dataframe with id, time, age, and exposure
    all.exp.agg <- ldply(names(all.exp),
                         function(this.time) {
                           uberdat <- do.call("rbind",
                                              all.exp[[this.time]])
                           uberdat$.time <- this.time
                           return(uberdat)
                         })

    all.exp.agg <- merge(all.exp.agg,
                         mf,
                         by=".internal_id")

    all.exp.agg <- all.exp.agg[,c(covar.names,
                                  ".time", ".age",
                                  ".exposure", ".occ",
                                  ".weight", ".exp.counts")]

    ## apply all of the weights:
    ##  - weights
    ##  - whether or not exposure should count
    ##    (to avoid counting dups, see unique.exp)
    ##  - exp.scale for scaling exposure
    ##    (eg mult by 1/12 to convert months to years of exposure)
    all.exp.agg <- transform(all.exp.agg,
                             .exposure = .exposure * .exp.counts * .weight * exp.scale,
                             .occ = .occ * .weight)

    ## now we can get rid of the weighting vars...
    all.exp.agg <- all.exp.agg[,-match(c(".exp.counts", ".weight", ".exp.scale"),
                                       colnames(all.exp.agg),
                                       0L)]
    
    all.exp.melted <- melt(all.exp.agg,
                           id.vars=c(covar.names,
                                     ".time", ".age"))

    occ.exp.array <- acast(all.exp.melted,
                           as.list(c(covar.names,
                                     ".time", ".age",
                                     "variable")),
                           fun.aggregate=sum,
                           na.rm=TRUE)
    
    ## name the dimensions...
    ## (be sure to handle the case where there are
    ##  no covariates)
    names(dimnames(occ.exp.array)) <- c(covar.names,
                                        "time", "age", "variable")

    return( list(occ.exp=occ.exp.array,
                 time.periods=time.periods,
                 age.groups=age.groups,
                 exp.scale=exp.scale,
                 counted.in.exp=mf$.exp.counts,
                 na.action=mf.na) )

}
