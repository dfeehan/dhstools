##########################################################################
##' attributes.to.long
##'
##' Start with a wide-form dataframe reported about alters using network method
##' questions and convert it into a long-form dataset. For example, after a network
##' survey of out-migrants, there might be variables about sex and age of each emigre
##' reported to be connected to each respondent. In a study that encountered
##' a maximum of 3 reported emigres across all respondents, this wide-form
##' dataframe might look like:\cr
##' \tabular{ccccccccc}{
##'  resp.id\tab resp.d.hat\tab emage.1\tab emage.2\tab emage.3\tab
##' emsex.1\tab emsex.2\tab emsex.3\cr
##'        1\tab 100\tab 24\tab NA\tab NA\tab M\tab NA\tab NA\cr
##'        2\tab 110\tab NA\tab NA\tab NA\tab NA\tab NA\tab NA\cr
##'        3\tab 140\tab 33\tab 23\tab 53\tab F\tab M\tab F\cr
##'        ... \cr
##' }
##' The \code{attributes.to.long} function could convert that into a
##' long-form dataframe that looks like this:\cr
##' \tabular{ccc}{
##'      degree\tab      age\tab   sex\cr
##'        100\tab        24\tab    M\cr
##'        140\tab        33\tab    F\cr
##'        140\tab        23\tab    M\cr
##'        140\tab        53\tab    F\cr
##'               \tab...\tab\cr
##' }
##' (Note that we make no guarantees about the order in which the reshaped data
##' will be returned.)\cr
##'
##' @section TODO:
##' \itemize{
##'   \item{should follow the se / nse pattern in, eg, the kp functions;
##'         interim workaround -- eg as.data.frame(df)[,idvar] -- for now}
##'   \item{for now, this converts any factors into characters.
##'         this is obviously not ideal. eventually, data type should be
##'         preserved...}
##'   \item{handle the case of "" more effectively. Right now, we
##'         *assume* that all structural missings
##'         (eg, I only report one alter,
##'         though there are three columns for me to do so) are NA}
##'   \item{look at the code in the middle of the function that's
##'         commented out and be sure we know that the order of
##'         the rows will be the same, to that we can cbind them
##'         together.}
##' }
##' 
##' @param df the wide-form dataset to convert
##' @param attribute.prefix a vector whose entries have the prefixes of the
##'                         names of variables
##'                         in the dataframe \code{data} that pertain to each
##'                         alter. if you'd like these to be re-named in the long
##'                         form data, then the variable names you'd like to use
##'                         in the long form should be the names of each entry in
##'                         this vector. in the example above, we would use
##'                         \code{attribute.prefix=c("age"="emage",
##'                         "sex"="emsex")}.
##'                         see \code{regexp}, below, to understand
##'                         how these prefixes are used to match columns of the
##'                         dataset; by default, we assume that the variables
##'                         match <prefix><either '.' or '_'><number>. 
##'
##' @param ego.vars if not NULL, the names of columns in the dataset that refer
##'                 to the egos and so should not be converted to long-form. you
##'                 can specify that they should be renamed in the same way as with
##'                 \code{attribute.prefix}. in the example above, we would use
##'                 \code{ego.vars=c("degree"="resp.d.hat")}.
##' @param keep.na if FALSE, remove columns in the resulting dataset that are
##'                all NAs
##' @param idvar the index or name of the variable in the data that has the
##'              respondent id. if NULL, then new ids which correspond to the rows
##'              in data are created.
##' @param sep a regular expression that the wide-form variable names are split
##'        around. (eg, for "var_01", sep="_"; for "var.01" is is "\\.")
##' @param varname.first TRUE if the text before the separator is the variable name
##'        (eg var_01), and FALSE otherwise
##'               
##' @return a long-form dataframe with the attributes reported for all of the alters.
##'         the dataframe will include an alter.id variable which is formed using
##'         <respondent id>.<alter number>
##' @export
##' @examples \dontrun{
##'    ## TODO add example
##' }
attributes.to.long <- function(df,
                               attribute.prefix,
                               ego.vars=NULL,
                               keep.na=FALSE,
                               idvar=NULL,
                               sep="\\.|_",
                               varname.first=TRUE)
{

  if (is.null(idvar)) {
      df$.tmpid <- 1:nrow(df)
  } else {
      ## this is kind of a hack, but if we don't coerce df to
      ## be a data frame, then this breaks because accessing
      ## a tbl_df via [] never produces a vector (like drop=FALSE always)
      df$.tmpid <- as.data.frame(df)[,idvar]
  }
  internal.idvar <- ".tmpid"

  if (is.null(names(attribute.prefix))) {
      names(attribute.prefix) <- attribute.prefix
  }

  ## grab the columns that have variables related to the attributes
  cn <- colnames(df)
  regexp <- paste0("^(",
                   paste0(attribute.prefix, collapse="|"),
                   ")(", 
                   sep, ")(.+)")

  varmat <- as.data.frame(str_match(cn, regexp), stringsAsFactors=FALSE)
  varmat <- varmat[! is.na(varmat[,1]),]

  if (varname.first) {
    colnames(varmat) <- c('column', 'variable', 'sep', 'alternum')
  } else {
    colnames(varmat) <- c('column', 'alternum', 'sep', 'variable')
  }

  avmap <- data.frame('newname'=names(attribute.prefix), 
                      'variable'=attribute.prefix)

  varmat <- join(varmat, avmap,
                 by='variable')

  ## grab the idvar, if we were given one; otherwise,
  ## create one
  if(! is.null(idvar)) {
    alterdata <- data.frame(id=as.data.frame(df)[,idvar])
  } else {
    alterdata <- data.frame(id=1:nrow(df))
  }

  ## check that the ids passed in are unique...
  if (length(unique(alterdata$id)) != nrow(alterdata)) {
    stop("id does not appear to be unique.\n")
  }

  ## for each alter num
  ##   - grab appropriate vars
  ##   - remove empty rows (if so ordered)

  alternums <- unique(varmat$alternum)

  res <- ldply(alternums,
                 function(this.alternum) {

                   these.cols <- varmat[varmat[,'alternum']==this.alternum,]

                   these.altercols <- these.cols[,'column']
                   names(these.altercols) <- paste(these.cols[,'newname'])
                   
                   tograb <- c('.tmpid'='.tmpid',
                               ego.vars, 
                               these.altercols)

                   these.alterdata <- select_(df, .dots=tograb) %>%
                     mutate(alternum = this.alternum)

                   if (! keep.na) {

                      ## this is tricky because we need to convert the usual
                      ## non-standard evaluation function calls to use
                      ## the standard evaluation version

                      ## for each row, we'll count the number of alter columns
                      ## that have missing values
                      ## and we'll filter out rows where that number is all of them
                      filter.cond <- paste(paste0("as.numeric(is.na(",
                                                       names(these.altercols), 
                                                       "))", collapse="+"))
                      numvar <- length(these.altercols)
                      
                      these.alterdata <- mutate_(these.alterdata,
                                                 .misscount=interp(filter.cond))
                      
                      these.alterdata <- filter_(these.alterdata,
                                                 .dots=interp(~ .misscount < numvar))

                      these.alterdata <- select_(these.alterdata,
                                                 .dots=interp(~ -.misscount))


                   }

                   return(these.alterdata)
                   
                 })

  return(res)

}




