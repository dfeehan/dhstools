############################################################
## occ-exp.R
##
## various functions that are useful for handling data
## on occurrences and exposures
############################################################

############################################################
##' compute the amount of exposure in a given interval
##'
##' given the boundaries of a time period (in cmc format),
##' this fn returns the amount of exposure each person
##' contributes. so, it computes the amount of exposure
##' in the period\cr
##' [exp.startobs, exp.endobs)\cr
##' that happens during\cr
##' [int.begin, int.end)\cr
##' TODO:
##' \itemize{
##'    \item{ add unit tests }
##'    \item{ explain the two intervals and how they are used
##'          (ie, improve documentation)}
##' }
##' Note that if there is any missingness in the
##' exp.startobs or exp.endobs variables, then this
##' assumes no exposure.
##' 
##' @param exp.startobs the start of the exposure window
##' @param exp.endobs the end of the exposure window
##' @param int.begin the start of the time period
##' @param int.end the end of the time period
##' @return the amount of exposure in the interval
##' @export
exp.in.interval <- function(exp.startobs,
                            exp.endobs,
                            int.begin,
                            int.end )
{

  ## compute start (min) and end (max)
  ## of exposure in this interval
  i.exp   <- rep(0, length(exp.startobs))
  thismin <- rep(0, length(exp.startobs))
  thismax <- rep(0, length(exp.startobs))

  ## respondents who started observation before the end of the interval
  ## and who ended observation after the beginning of the interval
  ## defining this year contributed some amt of exposure to this year
  anyexp <- (exp.startobs <= exp.endobs) &
            (exp.startobs <= int.end) &
            (exp.endobs >= int.begin) &
            (int.begin <= int.end)

  ## for now, if startobs / endobs not known, ignore
  anyexp[ is.na(anyexp) ] <- FALSE

  thismin <- pmax( exp.startobs, int.begin )
  thismax <- pmin( exp.endobs, int.end )

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
##'   singleyr.occ.exp <- compute.occ.exp(bdate ~ 1,
##'                                      data=bdata.coded,
##'                                      intervals=seq(from=yr.to.cmc(1980),
##'                                                    to=yr.to.cmc(1990+1),
##'                                                    by=12),
##'                                      interval.names=paste(years),
##'                                      start.obs=bdata.coded$dob,
##'                                      end.obs=bdata.coded$doi,
##'                                      ## NB: for now, have to pass ages in as 
##'                                      ## months and include one more than we
##'                                      ## want results for
##'                                      ages=seq(from=0,to=61*12,by=12),
##'                                      age.names=paste(seq(from=0,to=61,by=1)),
##'                                      unique.exp="caseid",
##'                                      exp.scale=1/12)
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
##'   gp.years <- seq(from=1970,to=2005,by=5)
##' 
##'    covars.occ.exp <- compute.occ.exp(bdate ~ urban + highestedlevel + religion,
##'                                      data=bdata.coded,
##'                                      intervals=seq(from=yr.to.cmc(gp.years[1]),
##'                                                    to=yr.to.cmc(max(gp.years)+5),
##'                                                    by=12*5),
##'                                      interval.names=paste(years),
##'                                      start.obs=bdata.coded$dob,
##'                                      end.obs=bdata.coded$doi,
##'                                      ## NB: for now, have to pass ages in as 
##'                                      ## months and include one more than we
##'                                      ## want results for
##'                                      ages=seq(from=0,to=65*12,by=5*12),
##'                                      age.names=paste(seq(from=0,to=65,by=5)),
##'                                      unique.exp="caseid",
##'                                      exp.scale=1/12)
##' 
##' @param formula a formula whose LHS is var that has the timing of events
##'                and whose RHS is covariatess (if any)
##' @param intervals vector of CMC values that gives the
##'                  time boundaries over which to compute events; for now,
##'                  we assume that these are all of equal width. For example,
##'                  intervals=seq(from=yr.to.cmc(years[1]),
##'                                to=yr.to.cmc(max(years)+1),
##'                                by=12)
##'                  will use as time intervals the range of years in the vector called
##'                  \code{years}. the intervals include the lower boundary, but not
##'                  the upper one; eg, they have the form [a,b), [b,c), ...
##'                  Note that if \code{doi} is not NULL (see below), then
##'                  these intervals are treated as time before the interview, so that the
##'                  actual time period is different for each row. 
##' @param data the dataset to use
##' @param doi if not NULL, the date of the interview for each row. in this case,
##'            the intervals are treated as years before the interview, so that the
##'            actual time period is potentially different for each row. (see the
##'            \code{intervals} argument, above)
##' @param ages a vector with the ages. for example,
##'             ages=seq(from=0,to=60*12,by=12) would give single years of age last
##'             birthday in [0,60]. these intervals all have to be of equal width, and
##'             they can't be wider than the time intervals (see \code{intervals})
##' @param start.obs vector of values (one per row of \code{data}) with the starting point
##'                  of the observation window for each row, in CMC format
##' @param end.obs   vector of values (one per row of \code{data}) with the ending point
##'                  of the observation window for each row, in CMC format
##' @param na.action what action to take on missing values; defaults to \code{na.pass}
##' @param int.name the name of the variable that has the time information; defaults to 'year'
##' @param age.name the name of the variable that has the age information; defaults to 'age'
##' @param occ.weight the weight to apply to occurrences; defaults to 1
##'                   (not yet implemented)
##' @param exp.weight the weight to apply to exposures; defaults to 1
##'                   (not yet implemented)
##' @param unique.exp tells which variable identifies unique units for computing exposure.
##'                   so, if the data is multiple records for units that experienced
##'                   multiple events, this allows us to avoid overcounting exposure
##' @param interval.names the name so of the time intervals
##' @param age.names the names of the ages or age intervals
##' @param exp.scale TODO
##' @return a list containing
##' \describe{
##'    \item{occ.exp}{an array of occurrences and exposures}
##'    \item{time.intervals}{the time intervals}
##'    \item{ages}{the ages}
##'    \item{exp.scale}{the amount by which exposure is scaled}
##' }
##' 
##' @export
compute.occ.exp <- function(formula,
                            intervals,
                            data,
                            doi=NULL,
                            ages,
                            start.obs,
                            end.obs,
                            na.action=na.pass,
                            int.name="year",
                            age.name="age",
                            occ.weight=1,
                            exp.weight=1,
                            unique.exp=NULL,
                            interval.names=NULL,
                            age.names=NULL,
                            exp.scale=1)
{
    ## figured this out using the
    ## code for lm(...) as a model

    ### first, call model.frame on the
    ###   formula and data we were passed in
    mf <- match.call()

    # for now, no weights or offset...
    arg.idx <- match( c("formula","data","subset","na.action"),
                      names(mf), 0L )

    mf <- mf[c(1,arg.idx)]
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())

    event <- model.response(mf, "numeric")

    covar.dims <- apply(mf[,-1,drop=FALSE],
                        2,
                        function(x) {
                          length(unique(x))
                        })

    covar.vals <- apply(mf[,-1,drop=FALSE],
                        2,
                        function(x) {
                          paste(unique(x))
                        })

    if (is.null(interval.names)) {
        interval.names <- paste(intervals[-length(intervals)])
    }

    if (is.null(age.names)) {
        age.names <- paste(ages)
    }

    ## check that age.names and interval.names have the right length
    if (length(interval.names) != (length(intervals)-1)) {
      stop("interval.names should have one fewer entry than intervals.")
    }
    if (length(age.names) != (length(ages))) {
      stop("age.names should have the same length as ages.")
    }
    
    ### unique.exp is a variable that tells which variable
    ### identifies unique units for computing exposure. so, if
    ### the data is multiple records for units that experienced
    ### multiple events, this allows us to avoid overcounting
    ### exposure
    if (! is.null(unique.exp)) {
        ## this picks (arbitrarily) one rowname for each unique
        ## value of the variable unique.exp
        ## eg if we have respondent id, this would only pick
        ## one row from each respondent to avoid overcounting
        ## exposure
        exp.subset <- rownames(data)[! duplicated(data[,unique.exp])]
    } else {
        ## if nothing specified, just take all of the rows
        ## in the data passed in
        exp.subset <- rownames(data)
    }

    # we'll have dropped obs because of missingness on
    # covariates;
    # inc.rows.occ indicates which of the rows in the
    #   original dataset (passed to this fn) should be
    #   retained for computing occurences
    inc.rows.occ <- rownames(data) %in% attr(mf,"row.names")
    # inc.rows.exp indicates which of the rows retained
    #   for computing occurences should also be used
    #   for computing exposure. if, eg, one person has
    #   several events recorded, using only one obs per person
    #   would avoid overcounting exposure
    inc.rows.exp <-  (attr(mf,"row.names") %in% exp.subset)

    start.obs <- start.obs[inc.rows.occ]
    end.obs <- end.obs[inc.rows.occ]

    ### create matrices for holding the results
    occ.exp <- array(0, c( covar.dims,
                           length(ages),
                           (length(intervals)-1),
                           2))

    ## recall that both time period and age intervals
    ## are assumed to be closed on the left and open on
    ## the right; ie, [start, end)
    
    ## NB: the assumption here is that the
    ##   time intervals are all of equal width
    interval.width <- intervals[2]-intervals[1]

    ## NB: the assumption here is that the age
    ##     intervals are all of equal width
    age.width <- ages[2]-ages[1]

    ### we're assuming that the respondent turns the next age
    ### at the start of the month of her birth
    ### (change this to 0.5 for halfway through the month...)
    ##b.offset <- 0.5
    b.offset <- 0

    ## note that bmo here is not literally the birth month,
    ## unless the age intervals are of width 12 months (ie, 1 year)
    ## otherwise, it is the month in the age interval where
    ## the age groups switch
    bmo <- (start.obs %% age.width) + b.offset

    if(length(covar.vals)==0) {
        dimnames(occ.exp) <-    list(paste(ages),
                                     paste(interval.names),
                                     c("occ", "exp") )
        names(dimnames(occ.exp)) <- c(age.name,
                                      int.name,
                                      "var")

    } else {

        dimnames(occ.exp) <-    c(as.list(data.frame(covar.vals)),
                                  list(paste(ages),
                                       paste(interval.names),
                                       c("occ", "exp") ))
        
        names(dimnames(occ.exp)) <- c(names(covar.dims),
                                      age.name,
                                      int.name,
                                      "var")
    }

    uberret <- c()
    
    for( yidx in 1:(length(intervals)-1) ) {

        cat("time interval: ", interval.names[yidx], "\n")

        ## figure out how much exposure each respondent had in
        ## time interval y

        ## this is the left-hand side of the time interval
        if (is.null(doi)) {
          y.begin <- rep(intervals[yidx], nrow(mf))
        } else {
          y.begin <- doi - intervals[yidx+1] 
        }
        
        ## this is the right-hand side of the time interval
        if (is.null(doi)) {
          y.end <- rep(intervals[yidx+1], nrow(mf))          
        } else {
          y.end <- doi - (intervals[yidx])
        }

        ## this is each row's 'birthday' in the given interval
        ## so, this is when each row switches age groups, and not necessarily the
        ## literal birthday (though it is the literal birthday if age.width is
        ## 12 months, ie, 1 year)
        min.age <- min(ages)
        this.bday <- y.begin + (age.width - ((y.begin - start.obs) %% age.width))

        ## we assume that the width of the time intervals <= width of age intervals, so that
        ## people will be two ages in the time interval
        age0 <- (floor((y.begin-start.obs)/age.width))*age.width + min(ages)
        age1 <- (floor((this.bday-start.obs)/age.width))*age.width + min(ages)

        ## exp.in.interval is in the dhstools package
        age0.exp <- exp.in.interval( start.obs, end.obs,
                                     y.begin, this.bday ) * exp.weight * exp.scale
        age1.exp <- exp.in.interval( start.obs, end.obs,
                                     this.bday, y.end ) * exp.weight * exp.scale

        if (any(age0.exp < 0) | any(age1.exp < 0)) {
          stop("ERROR: found negative exposure.
                There must be a problem with the time variables.\n")
        }
        
        ## figure out which rows experienced an event in
        ## the first age group in time period (event0) and
        ## the second age group in time period (event1)
        ## again, recall that we're treating intervals as
        ## closed on the left and open on the right
        event0 <- as.numeric(event >= y.begin &
                             event < this.bday &
                             event < y.end) * occ.weight
        event1 <- as.numeric(event >= y.begin &
                             event >= this.bday &
                             event < y.end) * occ.weight

        ## NAs are not events, so replace with 0s...
        event0[ is.na(event0) ] <- 0
        event1[ is.na(event1) ] <- 0

        ### get the formula for computing tabs
        table.fml <- formula
        ### take RHS out of formula...
        table.fml[2] <- NULL
        ### and add a term for age and exposure
        ##table.fml0 <- update(table.fml, ~ . + age0 + age0.exp)
        ##table.fml1 <- update(table.fml, ~ . + age1 + age1.exp)
        table.fml0 <- update(table.fml, ~ . + age0)
        table.fml1 <- update(table.fml, ~ . + age1)

        ### make the dataset we'll call xtabs on
        calldat <- cbind(data[inc.rows.occ,], age0, age0.exp, event0, age1, age1.exp, event1)

        ### and only use those observations we've marked as
        ### relevant for exposure (which could be different from
        ### the ones used to compute occurences)
        calldat.exp <- calldat[inc.rows.exp,]

        ## when we tally things up, only people w/ positive age will
        ## have the potential to be exposed or have an event occur
        min.age <- min(ages)
        max.age <- max(ages)

        calldat <- transform(calldat,
                             age0.inrange=(age0 >= min.age &
                                           age0 <= max.age),                             
                             age1.inrange=(age1 >= min.age &
                                           age1 <= max.age))        

        age0.res <- ddply(subset(calldat, age0.inrange),
                          as.quoted(table.fml0),
                          function(df) {
                            return(c(exp0=sum(df$age0.exp),
                                     occ0=sum(df$event0),
                                     age=df$age0[1]))
                          })
        age1.res <- ddply(subset(calldat, age1.inrange),
                          as.quoted(table.fml1),
                          function(df) {
                            return(c(exp1=sum(df$age1.exp),
                                     occ1=sum(df$event1),
                                     age=df$age1[1]))
                          })

        tot.res <- merge(age0.res,
                         age1.res,
                         by=c(all.vars(table.fml), "age"),
                         all=TRUE)

        tot.res <- transform(tot.res,
                             exp0=ifelse(is.na(exp0),0,exp0),
                             occ0=ifelse(is.na(occ0),0,occ0),
                             exp1=ifelse(is.na(exp1),0,exp1),
                             occ1=ifelse(is.na(occ1),0,occ1),                             
                             exp=exp0 + exp1,
                             occ=occ0 + occ1)

        tmp <- tot.res[,c(all.vars(table.fml),"age","occ","exp")]

        tmp$time <- interval.names[yidx]
        
        tmp2 <- melt(tmp,
                     measure.vars=c("occ","exp"))

        uberret <- rbind(uberret,
                         tmp2)
  
    }

    occ.exp <- acast(uberret,
                     paste(all.vars(table.fml),
                           "age",
                           "time",
                           "variable",
                           sep="~", collapse=""))

    names(dimnames(occ.exp)) <- c(all.vars(table.fml),
                                  "age",
                                  "time",
                                  "variable")

    ## convert to desired age names
    dimnames(occ.exp)$age <- age.names

    return( list(occ.exp=occ.exp,
                 time.intervals=intervals,
                 ages=ages,
                 exp.scale=exp.scale) )

}
