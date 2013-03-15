############################################################
## plot-mort-surface.R
##
## function for producing a plot of a mortality surface
############################################################



################################################
##' given a (two-dimensional) mortality surface,
##' produce a plot of it
##'
##' created by dennis, july '09
##' taken from usmcod-lib.R and put here in
##'    feb 2010
##'
##' i found this page very useful in writing
##' this function:
##' http://www.phaget4.org/R/image_matrix.html
##'
##' TODO...
##'   -> allow key at bottom instead of side
##'   -> handle logged labels for key (as an option -- currently unlogs automatically)
##'   -> handle passing in scale to use for everyone
##'   -> handle making each age-period a square
##'   -> (later? -- add overlay for age-period / cohorts )
##'   -> turn key on / off
##'   -> permit multiple plots per page
##'   -> avoid having to have big plots to avoid error...
##'   -> x label getting cut off...
##'
##' @param surf TODO
##' @param palcols TODO
##' @param main TODO
##' @param xnames TODO
##' @param ynames TODO
##' @param xnames.freq TODO
##' @param ynames.freq TODO
##' @param key.entries TODO
##' @param xlab TODO
##' @param ylab TODO
##' @param keylab TODO
##' @param key.round TODO
##' @param key.log TODO
##' @param log TODO
##' @param grid TODO
##' @param res TODO
##' @param range TODO
##' @export
########################################################
plot.mort.surface <- function( surf, palcols=c("blue","red"), main=NULL,
                               xnames=NULL, ynames=NULL,
                               xnames.freq=1, ynames.freq=5,
                               key.entries=10,
                               xlab=NULL, ylab=NULL,
                               keylab="test", key.round=1,
                               key.log=TRUE,
                               log=FALSE, grid=FALSE,
                               res=100, range=NULL) {

  ##################################
  # clean up the arguments...

  xlab <- ifelse(is.null(xlab),"",xlab)
  ylab <- ifelse(is.null(ylab),"",ylab)
  keylab <- ifelse(is.null(keylab),"",keylab)

  if (is.null(xnames)) {
    if (is.null(dimnames(surf))) {
      xnames <- 1:ncol(surf)
    } else {
      xnames <- dimnames(surf)[[2]]
    }
  }

  if (is.null(ynames)) {
    if (is.null(dimnames(surf))) {
      ynames <- 1:nrow(surf)
    } else {
      ynames <- dimnames(surf)[[1]]
    }
  }

  if (is.null(range)) {
    min.val <- min(surf,na.rm=TRUE)
    max.val <- max(surf,na.rm=TRUE)
  } else {
    min.val <- range[1]
    max.val <- range[2]
  }

  ##################################
  # plot the surface and its axes

  # make the color ramp...
  library(grDevices)
  palgen <- colorRampPalette(palcols)
  pal <- palgen(res)

  cols <- seq(from=min.val,to=max.val,length=res)

  curpar <- par()

  # want to split the screen into a place for the surface
  # and a place for the key
  #layout(matrix(1:2,nrow=1),widths=c(8,1),heights=c(1,1))
  par(curpar, fig=c(0,0.9,0,1),new=FALSE,mar=c(3,5,4,2))

  # plot the actual image
  #par()
  image(x=1:dim(surf)[2], y=1:dim(surf)[1], t(surf), axes=FALSE,
        xlab=xlab, ylab=ylab,
        col=pal, zlim=c(min.val,max.val))

  if(! is.null(main)) {
    title(main=main)
  }

  # pull out every ynames.freq / xnames.freq'th name to actually display
  # (allows us to avoid clutter)
  at.y <- 1:nrow(surf)
  tokeep <- ((at.y-1) %% ynames.freq==0)
  ynames[ ! tokeep ] <- ""
  axis( 2, at=at.y, labels=ynames,las=2, cex.axis=.8)
  at.x <- 1:ncol(surf)
  tokeep <- ((at.x-1) %% xnames.freq==0)
  xnames[ ! tokeep ] <- ""
  axis( 1, at=at.x, labels=xnames, las=1, cex.axis=.8)
#browser()
  ##################################
  # plot the key
  par(fig=c(0.9,1,0,1),new=TRUE,curpar,mar=c(3,4,4,1))
  #par(mar=c(3,4,4,1))
  keyvals <- seq(from=min.val,to=max.val,length=res)
  image(x=1,y=1:res,
        z=matrix(keyvals,nrow=1),
        col=pal,
        xlab="",ylab=keylab,
        axes=FALSE)
  #key.entries
  at.key <- 1:res
  tokeep <- ((at.key-1)%%key.entries==0)
  keyvals <- keyvals[ tokeep ]
  at.key <- at.key[ tokeep ]

  ## TODO -- add options
  if (key.log) {
      keyvals <- exp(keyvals) * 1000
  } else {
      keyvals <- keyvals
  }

  keyvals <- round(keyvals,key.round)

  axis(2, at=at.key, labels=keyvals, cex.axis=.8,las=2)
  #layout(1)
  par <- curpar
}






