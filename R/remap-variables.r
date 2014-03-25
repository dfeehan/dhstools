############################################################
## remap-variables.R
##
## some functions which are useful in working with
## and recoding categorical variables
## 
############################################################

#########################################################
##' using a recode matrix (called a varmap), which has columns
##' 'code' and 'value' (and perhaps others),
##' rename a variable. this is a convenient wrapper around
##' the \code{car::recode} function
##'
##' @param x the variable to recode
##' @param varmap a data.frame describing the recodes, which has
##'               columns called 'code' and 'value'
##' @param as.factor.result if TRUE, turn the result into a factor
##' @param ... additional arguments to pass to car::recode
##' @return the recoded variable
##' @export
##' @examples \dontrun{
##'   ## TODO -- write example
##' }
recode.varmap <- function(x, varmap, as.factor.result=TRUE, ...) {

  ## NB: currently, we're relying on car::recode for this
  rstr <- paste("'", varmap$code, "'='", varmap$value, "'", sep="", collapse=";")

  res <- recode(x, rstr, as.factor.result=as.factor.result,
                ...)

  return(res)
}

###################################################
##' quick & dirty fn for renaming variable names...
##'
##' it may make sense to take this out and just
##' use the \code{plyr::rename} function
##' from the \code{plyr} package
##'
##' @param vnames the list of names to remap
##' @param fromvars the current variable name
##' @param tovars the new variable name (positions in this
##'               vector should correspond to the ones in
##'               fromvars)
##' @return a vector with \code{vnames} renamed according to
##'         the mapping from \code{fromvars} to \code{tovars}
##' @export
##' @seealso \code{\link[plyr]{rename}}
##' @examples \dontrun{
##'    colnames(my.data) <- rename.vars(colnames(my.data),
##'                                     c("old_var1", "old_var2"),
##'                                     c("new_var1", "new_var2"))
##' }
##################################################
rename.vars <- function( vnames, fromvars, tovars ) {

    toret <- vnames

    for( fv in 1:length(fromvars) ) {
        toret[(which(vnames==fromvars[fv]))] <- tovars[fv]
    }

    return( toret )

 }

############################################################
##' remap a categorical variable to have
##' contiguous numerical values.
##' 
##' Given a factor, this function re-assigns numerical values
##' to each level in such a way that the resulting values are
##' sequential. For example, if the factor has levels
##' "red", "green", and "blue" represented by 1, 10, and 15,
##' this function would return another factor with levels
##' "red", "green", and "blue" with values 1, 2, and 3. This
##' is useful in preparing categorical variables for use
##' with JAGS or Stan.
##'
##' @param x the variable to remap, assumed to be a factor
##' @return a list whose elements contain
##'         \describe{
##'            \item{vals}{the variable x with the underlying values
##'                        made contiguous}
##'            \item{map}{a vector with the mapping from the old to the new levels}
##'            \item{len}{the number of levels}
##'            \item{orig}{the original variable that was passed in}
##'         }
##' @export
##' @examples \dontrun{
##'    ## TODO add examples
##' }
remap.contig <- function(x) {
    newx <- rep(NA, length(x))
    uniq.x <- paste(unique(x))
    n.x <- length(uniq.x)
    for(i in 1:n.x) {
        newx[ paste(x)==uniq.x[i] ] <- i
    }
    newx <- as.numeric(newx)
    return( list( vals=newx, map=uniq.x, len=n.x, orig=x ))
}

############################################################
##' apply the same re-mapping that was used for one factor
##' to a different factor
##' 
##' 
##'
##' @param x the variable to remap, assumed to be a factor
##' @param mapping the mapping to use
##' @param new.to.na if TRUE, then new levels not in the mapping
##'        get coded to NA; otherwise, they get assigned new
##'        numbers
##' @return a list whose elements contain
##'         \describe{
##'            \item{\code{vals}}{the variable x with the underlying values
##'                        made contiguous}
##'            \item{\code{map}}{a vector with the mapping from the old to the new levels}
##'            \item{\code{len}}{the number of levels}
##'            \item{\code{orig}}{the original variable that was passed in}
##'         }
##' @export
##' @examples \dontrun{
##'   ## TODO add examples
##' }
same.remap <- function(x, mapping,
                       new.to.na=FALSE) {

  ## mapping has the mapping already performed
  ## (ie, the list that resulted from remap.contig)
  newx <- rep(NA, length(x))
  uniq.x <- paste(unique(x))
  n.x <- length(unique(c(paste(x), paste(mapping$orig))))

  diff.x <- uniq.x[ ! uniq.x %in% paste(mapping$map)]
  new.map <- c(paste(mapping$map), diff.x)
  
  for(i in 1:n.x) {
    if (! new.to.na) {
      newx[ paste(x)==new.map[i] ] <- i
    } else if (i <= mapping$len) {
      newx[ paste(x)==new.map[i] ] <- i
    }
  }
  newx <- as.numeric(newx)
  
  return( list( vals=newx, map=new.map, len=n.x, orig=x,
                new.to.na=new.to.na, diff=diff.x))
  
}

##########################################################################
##' harmonize.levels
##'
##' given two factors, where the second factor has the same levels as the
##' first, or where the second factor's levels are a superset of the
##' first's, return a new factor to replace the first one whose levels
##' are the same as the second's.
##' so if we have\cr
##'    \code{fac1 <- c("old", "young", "old") # levels are c("old", "young")}\cr
##'    \code{fac2 <- c("young", "old", "infant") # levels are
##'          c("infant", "young", "old")}\cr
##' then\cr
##'    \code{res <- harmonize.levels(fac1, fac2)}\cr
##' will return a factor
##' \code{c("old", "young", "old")} whose levels are
##' \code{c("infant", "young", "old")}
##'
##' @param fac1 the first factor
##' @param fac2 the second factor
##' @export
##' @examples \dontrun{
##'    ## TODO add examples
##' }
harmonize.levels <- function(fac1, fac2) {

  if (length(levels(fac1)) > length(levels(fac2))) {
    stop("can't harmonize these factors. the second one has fewer levels!\n")
  }
  
  res <- factor(fac1, levels=levels(fac2))

}
