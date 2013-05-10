##########################################################################
##' helper fns for dhstools
##'
##' (NB: get.var and get.weights are taken from the
##'      networkreporting package)
##'


##########################################################################
##' get a variable from a dataframe or vector
##'
##' this function was written because a few of the estimator functions
##' need to use weights, and there are several cases to handle:
##' the user could pass in a column name, a vector of weights, or
##' nothing (in which case, the weights should default to 1 for each
##' row in the dataset). for the special case of getting weights, look
##' at the curried fn get.weights (right below)
##' 
##' @param survey.data the survey dataset
##' @param var either NULL, a column name, or a vector of values
##' @param default the default value to fill in if the variable
##'        is not found
##' @return a vector of values whose length is the same as the
##'         number of rows in survey.data; if var is NULL, this has
##'         the default values
##' @keywords internal
get.var <- function(survey.data, var, default=NA) {

  ## weights will default to 1 for everyone, unless the user specified
  ## a weights variable
  if (is.null(var)) {
    
    return(rep(default, nrow(survey.data)))
    
  } else if (length(var) == 1) {
  
    ## ... otherwise, see if the weights variable is referring
    ## to a column of the dataframe; try to
    ## grab sampling weights from survey dataframe
    var.vals <- try(subset(survey.data,
                           select=var),
                    silent=TRUE)

    if( inherits(var.vals, "try-error") ||
       ncol(var.vals) != 1 ||
       ! is.numeric(var.vals[,1]) ) {

      stop(paste(var,
                 " does not identify a valid column in the data.\n"))
    }

    var <- as.numeric(var.vals[,1])

    return(var)
    
  } else if (is.numeric(var) & length(var) == nrow(survey.data)) {

    ## if var a vector with one entry per row, then these
    ## are our values
    return(var)
  } else {    
    stop("can't determine what the values should be for ", var, ".")
  }
    
  
}

##########################################################################
##' get the weights column from a dataframe
##'
##' this is the same as get.var with the default value set to 1
##' instead of NA
##' @param ... (this is a function curried from \code{get.var})
##' @keywords internal
get.weights <- Curry(get.var, default=1)
