############################################################
## cmc.R
##
## functions that work with century-month codes
## which are used for reporting lots of dates in
## DHS surveys
##
## 
############################################################

############################################################
##' decode a cmc value to a year and month
##'
##' Given a calendar month code (CMC) value, return the
##' associated year and month.\cr
##' TODO -- add checks to be sure the CMC value makes sense
##' 
##' @param cmc the cmc value to decode
##' @return a vector whose first entry is the year and
##'         whose second entry is the month
##' @export
cmc.decode <- function(cmc) {

  ## TODO -- check that the cmc value makes sense
  
  yr <- floor( (cmc-1)/12) + 1900
  mo <- cmc - ((yr-1900)*12)
  return( c("year"=yr, "month"=mo))
}


############################################################
##' convert a given year (and possibly month) to a
##' CMC value
##'
##' Given a year and, possibly, a month, return the
##' appropriate CMC value
##' 
##' @param year the year to convert to a CMC
##' @param month the month to convert to a CMC. if not
##'              specified, this defaults to 1 (January)
##' @return the CMC value corresponding to that year
##' @export
yr.to.cmc <- function(year, month=1) {
    return( (year-1900)*12 + month)
}

