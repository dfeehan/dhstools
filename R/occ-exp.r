############################################################
## occ-exp.R
##
## various functions that are useful for handling data
## on occurrences and exposures
############################################################

############################################################
##' make an age.groups object
##'
##' TODO -- would be nice if this could take either widths
##' or bins / breaks
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
##' @export
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
##' @export
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

##########################################################
##' tabulate occurrences and exposures
##'
##' Given a variable indicating when an event happened,
##' a time window we are interested in, and possibly a set
##' of covariates, tabulate counts of event occurences and
##' exposures in the given time interval.\cr
##' Note that you have to be careful about observations that
##' don't experience an event, but still count for exposure;
##' see the example below.
##' 
##' 
##' TODO
##' \itemize{
##'    \item write unit tests
##'    \item fill an example in the documentation below
##'    \item at start of code, handle defaults more elegantly
##'    \item id.var is not implemented; might be better to directly handle
##'          multiple events (see below)
##'    \item handle multiple time periods
##'    \item handle multiple events
##'    \item handle GK weights (possibly not in this function)
##'    \item possibly refactor in the future to pass in dataset of lifelines
##'          and separate list of event dates
##'    \item what if event date is missing?
##'    \item what about things that vary with the event, eg mother's
##'                  age when child was born?
##'    \item better description of dates; this was developed using CMC codes
##'          from the DHS surveys, but it should work for any interval scale
##' }
##'
##' @examples
##'
##'   ## THESE EXAMPLES ARE NOT UPDATED!
##'   ## Please disregard for the time being...
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
##' @param data the dataset containing events
##' @param start.obs vector of values (one per row of \code{data}) with the starting point
##'                  of the observation window for each row, in CMC format
##' @param end.obs   vector of values (one per row of \code{data}) with the ending point
##'                  of the observation window for each row, in CMC format
##' @param event the column of the dataset that indicates the date of an event. observations
##'        that contribute exposure but no events should have this set to a value that
##'        will never occur in the time period; for example, -1
##' @param age.groups an age.groups object
##' @param age.offsets if not NULL, then the age.periods are to be interpreted relative to ##'        these times (one for each row). this is usually a birth date
##' @param time.periods a time.periods object
##' @param time.offsets if not NULL, then the time.periods are to be interpreted relative to ##'        these times (one for each row). useful for computing quantities like 
##'        "X months before interview", where interview happened at different times 
##'        for different respondents
##' @param id.var the variable giving the unique rows of the dataset for each
##'        individual (UNDER DEVELOPMENT)
##' @param covars the name of covariates over which occurrences and exposures should be
##'        aggregated; defaults to NULL, meaning totals are computed over the entire dataset
##' @param weights the weight to apply to occurrences and exposures; defaults to 1
##' @param exp.scale amount by which to scale exposure; if, for example, dates are measured
##'                   in months, but you want to measure rates in years, then this should
##'                   be 1/12. It defaults to 1
##' @return a data frame with the covariates, age groups, occurences and expsoures
##' 
##' @export
compute.occ.exp <- function(data,
                            start.obs,
                            end.obs,
                            event,
                            age.groups,
                            time.periods,
                            id.var=NULL,
                            covars=NULL,
                            age.offsets=NULL,
                            time.offsets=NULL,
                            weights=NULL,
                            exp.scale=1)
{


    ## TODO - I think the next several blocks (handling defaults)
    ## could be improved 

    if (is.null(id.var)) {
        data$.id <- 1:nrow(data)
    } else {
        data$.id <- data[,id.var]
    }
    id.var <- ".id"

    if (is.null(age.offsets)) {
        data$.age.offset <- 0
    } else {
        data$.age.offset <- data[,age.offsets]
    }
    age.offsets <- ".age.offset"

    if (is.null(time.offsets)) {
        data$.time.offset <- 0
    } else {
        data$.time.offset <- data[,time.offsets]
    }
    time.offsets <- ".time.offset"

    if (is.null(weights)) {
        data$.weight <- 1
    } else {
        data$.weight <- data[,weights]
    }
    weights <- ".weight"

    ## we'll need this to use the programmatic version of the dply
    ## aggregation functions below
    ## see
    ## http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
    gpvars <- lapply(covars, as.symbol)

    full.dat <- data %>% select_(.dots=c(start.obs, end.obs, event, id.var,
                                         age.offsets, time.offsets,
                                         weights, covars))

    ## NOTE: if future versions allow multiple events, there
    ##  will be a separate list that maps ids to event lists
    ##  instead of having the third column of this matrix
    ##  have the event date (if any)
    lifeline.mat <- as.matrix(full.dat %>% select(1:3))
    colnames(lifeline.mat) <- c("start.obs", "end.obs", "event")

    this.time.period <- matrix(time.periods$template,
                               nrow=nrow(lifeline.mat),
                               ncol=2,
                               byrow=TRUE)
    this.time.period <- this.time.period + select_(full.dat, .dots=time.offsets)[,1]

    this.age.groups <- age.groups$template

    this.age.offset <- select_(full.dat, .dots=start.obs)[,1]

    raw.res <- cpp_compute_occ_exp(lifeline.mat,
                                   this.age.groups,
                                   this.age.offset,
                                   this.time.period)

    weighted.sum <- function(x, w) {
        return(sum(x*w))
    }

    ## summarize each qty (occ and exp) separately; then combine them
    agg.qty <- ldply(c('occ', 'exp'),
                     function(this.qty) {

                        res.qty <- as.data.frame(raw.res[[this.qty]])
                        colnames(res.qty) <- paste0("agegroup_", 1:ncol(res.qty))

                        # note that this assumes the order of the rows hasn't changed
                        res.qty <- cbind(full.dat, res.qty)

                        res.qty.agg <- res.qty %>% group_by_(.dots=gpvars) %>%
                                       summarise_each(funs(weighted.sum(., .weight)),
                                                      starts_with("agegroup"))

                        res.qty.agg <- res.qty.agg %>% 
                                       gather(agegroup, value, starts_with("agegroup")) %>%
                                       mutate(qty=this.qty)

                        return(res.qty.agg)

                     })

    agg.res <- spread(agg.qty, qty, value)

    ## rename the age group to match the definitions
    agename.remap <- data.frame(agegroup=paste0("agegroup_", 1:length(age.groups$names)),
                                agelabel=age.groups$names,
                                stringsAsFactors=FALSE)

    agg.res$agegroup <- paste(agg.res$agegroup)

    agg.res <- left_join(agg.res, agename.remap)

    ## rescale the exposure (for example, if our time units are months,
    ## as with DHS CMC codes, we'd want to scale by 1/12 to be able to
    ## produce yearly rates)
    agg.res <- agg.res %>% mutate(exp=exp*exp.scale)

    ## TODO 
    ##  -> test with fixed time periods (instead of time before interview)
    ##  -> allow multiple time periods (shouldn't be too hard)
    ##  -> allow multiple events (eg for fertility)
    ##  -> develop unit tests
    ##  -> be sure to test with missing data

    return(agg.res)

}


