############################################################
## read-files.R
##
## various functions that are useful for reading DHS
## files into R
############################################################

############################################################
##' takes (some) stata .dct (dictionary) files and
##' turns them into a vardef data frame suitable for
##' passing into simple.fwf
##'
##' NOTE: this is by no means encyclopaedic. i've just tested
##' this on the nicely-formatted .dct files that accompany
##' the multiple cod files found at
##' http://www.nber.org/data/vital-statistics-mortality-data-multiple-cause-of-death.html
##' TODO
##'       * better solution for multiple variables
##'       * better error handling
##'
##' @param filename the .dct file
##' @param remove.duplicates should variables defined to start
##'                           at the same column be removed? if so,
##'                           first variable using a given start column
##'                           is chosen (currently, simple.fwf can't handle them)
##'                           this defaults to TRUE.
##'
##' @return the vardefs dataframe
##' @export
############################################################
 dct.to.vardefs <- function( filename, remove.duplicates=TRUE )
 {

   rawdct <- readLines(filename)

   vardefs <- NULL

   ## go through each line in the file
   for( i in seq(along=rawdct) )
   {
     ## if the line has a _column( statement,
     ## then we want to process it
     if( length( grep("1:", rawdct[i])) > 0 ) {

       ## this is a regular expression that looks for
       ##     some amount of space
       ##     some string (the datatype)
       ##     some amount of space
       ##     <<a word (the varname)>>
       ##     some amount of space
       ##     a number:
       ##     some amount of space
       ##     <<a number>>-<a number>>
       ## the quantities in << >> are what we want; the call to
       ## sub(...) returns a string with those three things separated by spaces
       this.tmp <- sub("[[:space:]]*[[:alnum:]_]+[[:space:]]+([[:alnum:]_]+)[[:space:]]+[[:alnum:]_]+:[[:space:]]*[[:space:]]*([[:digit:]]+)-([[:digit:]]+).*", "\\1 \\2 \\3", rawdct[i])
       ## the result is a space-separated string with the variable name,
       ## the starting column and the ending column. split this string up and stash
       ## the components in the appropriate columns in vardefs
       this.tmp <- strsplit(this.tmp, " +")[[1]]

       vardefs <- rbind(vardefs, c(this.tmp[2], this.tmp[1], as.numeric(this.tmp[3])-as.numeric(this.tmp[2])+1))

     }

   }

   ## coerce the vardefs matrix into a dataframe with
   ## each column having the appropriate datatype
   vardefs <- as.data.frame(vardefs, stringsAsFactors=FALSE)
   colnames(vardefs) <- c("begin", "varname", "size")
   vardefs$begin <- as.numeric(vardefs$begin)
   vardefs$size <- as.numeric(vardefs$size)

   ## take out rows that have the same value for begin
   ## if the remove.duplicates option is true
   if (remove.duplicates) {

     if (sum(duplicated(vardefs$begin) > 0)) {
       cat("Removed", sum(duplicated(vardefs$begin)), "overlapping variable(s) with names: \n")
       cat( vardefs[ duplicated(vardefs$begin), "varname"] )
       cat("\n")
     }

     vardefs <- vardefs[ ! duplicated(vardefs$begin), ]
   }

   ## finally, sort the rows so that they go in ascending order
   vardefs <- vardefs[ order(vardefs$begin), ]
   rownames(vardefs) <- paste(1:nrow(vardefs))

   return(vardefs)

 }

############################################################
##' read in files that are in fixed width format
##'
##' takes a simple matrix locating the variables
##' of interest (created, eg, as a .csv file in excel)
##' and composes a vector of widths suitable to be passed
##' into read.fwf
##' NOTE: for now, this can't handle a situation where
##' the vardefs contains multiple variables using the
##' same columns (ie, a fips state and county code)
##'
##' @param vardefs a matrix (or data frame) with cols
##'                 "begin", "size", and "varname" with the starting
##'                 column, the variable width, and its name
##' @param filename the file (in fixed-width format) to
##'                  read in
##' @param ... other args to be passed along to read.fwf
##'
##' @return the dataframe from the fixed-width file
##' @export
############################################################

 simple.fwf <- function(vardefs, filename, ...)
 {

    ## sort to be sure that variables are in order of appearance
    vardefs <- vardefs[ order(vardefs$begin), ]

    these.widths <- NULL

    ## if the first variable we grab is at column 1, then
    ## the first width is positive
    if (vardefs$begin[1] != 1) {
        these.widths <- c(-(vardefs$begin[1]-1), vardefs$size[1])
    ## ... otherwise, it's negative (since we have to skip columns
    ## to get to the first variable)
    } else {
        these.widths <- vardefs$size[1]
    }

    ## ... update our index into the columns
    this.pos <- sum(abs(these.widths)) + 1

    ## now go through all of the rest of the variables
    for(i in 2:nrow(vardefs)) {

        ## if the next var doesn't begin at the next
        ## position, then we have to skip columns until
        ## the start of the next var
        if (vardefs$begin[i] > this.pos) {

            ## figure out how many to skip
            this.gap <- (vardefs$begin[i] - this.pos)
            ## add negative that number to the vector of widths
            these.widths <- c(these.widths, -this.gap)
            ## and move the current position up to the start
            ## of the next variable
            this.pos <- this.pos + this.gap
        } else if (vardefs$begin[i] < this.pos) {
            cat("these.widths: ", these.widths, "\n")
            stop("The input is invalid. Check that no variable definitions overlap.\n")
        }

        ## add the width of the next variable
        these.widths <- c(these.widths, vardefs$size[i])
        ## and move the position index along appropriately
        this.pos <- this.pos + vardefs$size[i]

    }

    data  <- read.fwf( filename,
                       width=these.widths,
                       col.names=vardefs$varname,
                       ...)

    return(data)

 }




