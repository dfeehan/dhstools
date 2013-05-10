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
##' \itemize{
##'   \item{TODO - }{for now, this converts any factors into characters.
##'                  this is obviously not ideal. eventually, data type should be
##'                  preserved...}
##'   \item{TODO - }{handle the case of "" more effectively. Right now, we
##'                  *assume* that all structural missings
##'                  (eg, I only report one alter,
##'                  though there are three columns for me to do so) are NA}
##'   \item{TODO - }{look at the code in the middle of the function that's
##'                  commented out and be sure we know that the order of
##'                  the rows will be the same, to that we can cbind them
##'                  together.}
##' }
##' 
##' @param data the wide-form dataset to convert
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
##' @param regexp the regular expression which describes the wide-form variable names
##'               the default is anything that ends in a "." or a "_" and a number.
##'               if you specify your own regular expression, it should use groups
##'               (specified with "(" and ")"). the default is
##'               \code{"^(.+)([\\.|_])(\\d+$)"}, which specifies three groups
##'               (see below for how these groups are used).
##' @param regexp.index.gp the group(s) in regexp which should match the
##'               wide-form variable name prefixes specified in
##'               \code{attribute.prefix}. in the default, 
##'               this is the first group, so \code{regexp.index.gp=1}.
##' @param regexp.vname.gp the group(s) in regexp which should vary over
##'               the different alters; in the default, this is the third group,
##'               so \code{regexp.vname.gp=3}.
##'               
##'               
##' @return a long-form dataframe with the attributes reported for all of the alters.
##'         the dataframe will include an alter.id variable which is formed using
##'         <respondent id>.<alter number>
##' @export
##' @examples \dontrun{
##'    ## TODO add example
##' }
attributes.to.long <- function(data,
                               attribute.prefix,
                               ego.vars=NULL,
                               keep.na=FALSE,
                               idvar=NULL,
                               regexp="^(.+)([\\.|_])(\\d+$)",
                               regexp.index.gp=3,
                               regexp.vname.gp=1)
{

  ## step 1: grab the names of the variables for each attribute
  attnames <- alply(attribute.prefix,
                    1,
                    function(pre) {
                      cn <- colnames(data)
                      this.idx <- str_match(cn,
                                            regexp)
                      this.idx <- as.data.frame(this.idx,stringsAsFactors=FALSE)

                      ## now need to take results matrix and figure out how to get
                      ## indices of the pre variables from it...
                      this.idx$is.match <- ! is.na(this.idx[,1])
                      this.idx$index <- 1:nrow(this.idx)
                      this.idx$cname <- cn

                      this.idx$vname <- apply(this.idx,
                                              1,
                                              function(x) {
                                                res <- paste(x[regexp.vname.gp+1],
                                                             collapse="")
                                                return(res)
                                              })

                      res.idx <- subset(this.idx,
                                        vname==pre)$cname

                      ## if we can't find one of the attributes in the dataset,
                      ## then return an error
                      if (is.null(res.idx) | length(res.idx)==0) {
                        stop("Can't find attribute ",
                             pre,
                             " in the column names of the dataset.\n")
                      }

                      return(res.idx)
                    })

  ## grab the idvar, if we were given one; otherwise,
  ## create one
  if(! is.null(idvar)) {
    alterdata <- data.frame(id=data[,idvar])
  } else {
    alterdata <- data.frame(id=1:nrow(data))
  }

  ## check that the ids passed in are unique...
  if (length(unique(alterdata$id)) != nrow(alterdata)) {
    stop("id does not appear to be unique.\n")
  }

  l_ply(unlist(attnames),
        function(this.attname) {
          if (is.factor(data[,this.attname])) {
            cat("converting ", this.attname, "from factor to numeric.\n")
            data[,this.attname] <<- as.numeric(data[,this.attname])
          }
        })

  adata <- llply(attnames,
                 function(these.attnames) {

                   ## grab the wide-form data for this attribute
                   these.alterdata <- data[,c(ego.vars, these.attnames)]
                   ## add the id variable
                   these.alterdata$id <- alterdata$id

                   ## reshape long
                   tmp <- melt(these.alterdata,
                               id.vars=c("id",ego.vars))

                   thesepat <- str_match(tmp$variable,
                                         regexp)
                   tmp$variable <- apply(thesepat,
                                         1,
                                         function(x) {
                                           res <- paste(x[regexp.vname.gp+1],
                                                        collapse="")
                                           return(res)
                                         })                   
                   tmp$alternum <- apply(thesepat,
                                         1,
                                         function(x) {
                                           res <- paste(x[regexp.index.gp+1],
                                                        collapse="")
                                           return(res)
                                         })                                      

                   ## NOTE:
                   ## if variable is not numeric,
                   ## as it is not in the application i'm working with now,
                   ## then this causes an error. figure out how to fix this...

                   tmp2 <- dcast(tmp,
                                 ... ~ variable)


                   tmp2$alter.id <- paste(tmp2$id, ".", tmp2$alternum, sep="")
                   
                   tmp2 <- subset(tmp2, select=-c(id,alternum))

                   return(tmp2[order(tmp2$alter.id),])
                   
                 })

  ## ensure that the ids are all in the same order, so that
  ## we can just cbind the data.frames from each attribute together
  if(1==0) {
  check <- laply(adata,
                 function(x) {
                   return(x$alter.id)
                 })
  check <- t(check)

  checkres <- aaply(check,
                    1,
                    function(x) {
                      return(length(unique(x))==1)
                    })
  
  ## now we have a list of dataframes, one for each attribute,
  ## with id variable id.alternum
  ## we'll merge these all into one dataset...
  if(! all(checkres==TRUE)) {
    ## (this should never happen)
    stop("alignment error in reassembling long-form attribute dataset.\n")
  }
}

  ## get the alter.id values (they will be in the same order
  ## for all of the datasets)
  att.ids <- adata[[1]]$alter.id
  
  ## if we're keeping any ego vars, grab them too
  ego.var.dat <- adata[[1]][,ego.vars]

  ## remove the id value from each dataset, so that we
  ## don't have it duplicated

  adata <- llply(adata,
                 function(x) {
                   tolose <- match(c("alter.id", ego.vars),colnames(x))
                   x <- subset(x,select=-tolose)
                   ##x <- subset(x,select=-alter.id)                   
                   return(x)
                 })

  ## column-bind the datasets together
  att.data <- do.call("cbind", adata)

  ## if we're renaming the attributes of the
  ## alters, then do so now
  if (! is.null(names(attribute.prefix))) {
    toren <- names(attribute.prefix)
    names(toren) <- paste(attribute.prefix)
    att.data <- rename(att.data,
                       toren)
                       
  }
  
  ## ... and replace the alter.id values
  att.data$alter.id <- att.ids

  ## and add the ego vars to our final dataset
  att.data[,ego.vars] <- ego.var.dat

  ## if we're renaming the ego variables,
  ## do so now
  if(! is.null(names(ego.vars))) {
    toren <- names(ego.vars)
    names(toren) <- paste(ego.vars)
    att.data <- rename(att.data,
                       toren)
  }

  ## if we are to remove columns that are all NAs, do so now
  if (! keep.na) {

    ### TODO -- LEFT OFF HERE:
    ### THIS IS WHERE WE NEED TO CHANGE - now that there are
    ### ego vars, they are also in att.data; we want to lost rows
    ### that have all of the attribute columns missing...

    if (! is.null(names(attribute.prefix))) {
      avars <- names(attribute.prefix)
    } else {
      avars <- attribute.prefix
    }

    attidx <- which(colnames(att.data) %in% avars)

    tolose <- ddply(att.data,
                    .(alter.id),
                    function(x) {
                      return(data.frame(tolose=all(is.na(x[attidx]))))
                    })

    att.data <- merge(att.data,
                      tolose,
                      by="alter.id",
                      all=TRUE)
    att.data <- subset(att.data,
                       ! tolose,
                       select=-tolose)
                   
  }
  
  return(att.data)

}
