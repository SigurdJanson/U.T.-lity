
#' pxpplot
#'
#' A plot visualising the distribution of found defects by user. The plot can
#' visualise frequency of occurrence similar to a heat map.
#'
#' @param x matrix containing usability problems and their frequency
#' @param darkfigure add columns (rows) with zero probability that represent hidden problems.
#' @param percentage show the total percentage of each defect.
#' @param ... Additional parameters will be passed to the plotting function
#'
#' @details The plot will only count defect occurrences once. If a defect was
#' evident more than once for a user this is not counted here.
#'
#' @return Returns the grid as matrix.
#' @export
pxpplot <- function(x, darkfigure = NULL, percentage = TRUE, ...)
  UseMethod("pxplot")



#' @describeIn pxpplot Throws an error because there are no methods for other objects
#' than data frames and matrices.
#' @export
pxpplot.default <- function(x, darkfigure = NULL, percentage = TRUE, ...)
  stop("Object cannot be shown as Participant X Problem Plot")


pxpplot.defectgrid <- function(x, darkfigure = NULL, percentage = TRUE,
                               lib = c("ggplot", "graphics"), ...) {
  lib <- match.arg(lib)
  # if (lib == "ggplot" && !require(ggplot2) )
  #   lib <- "graphics"
  # else
  #   "ggplot2"

  if (lib == "ggplot" || missing(lib))
    ciplot_gg(lower, upper, names, ...)
  else
    ciplot_gr(lower, upper, names, ...)
}



#' @describeIn  pxpplot Accepts a matrix as input with participants in rows and
#' defects/problems in columns.
#'
#' @param names.arg A vector of names to be plotted below each tile.
#' @param horiz If true users will be plotted on the horizontal axis (default is  \code{FALSE})
#' @param density A vector giving the density of shading lines (in lines per inch) for the tiles.
#' @param angle Slope of shading lines, given as an angle in degrees (counter-clockwise),
#' for the tiles.
#' @param col A vector giving the colors to be used to show defect frequencies
#' @param col.opt if TRUE, colors will be optimised with equal distances.
#' @param border The color to be used for the border of the tiles.
#' Use \code{border = NA} to omit borders. If there are shading lines,
#' \code{border = TRUE} means use the same colour for the border as for the shading lines.
#' @param main An overall title for the plot.
#' @param sub A sub title for the plot.
#' @param xlab Label for the horizontal axis.
#' @param ylab Label for the vertical axis.
#' @param xpd Logical. Should tiles be allowed to go outside region?
#' @param axes Logical. Indicated whether both axes should be drawn on the plot.
#' @param cex.axis Expansion factor for numeric axis labels.
#' @param cex.names Expansion factor for axis names (tile labels).
#' @param axis.lty The graphics parameter \code{lty} applied to the axis and
#' tick marks of the 'defect' (default horizontal) axis. Note that by default
#' the axis is suppressed.
#' @param plot Logical. If  \code{FALSE}, nothing is plotted.
#' @param add Logical. Specifies if grid should be added to an already existing
#' plot (default is \code{FALSE}).
#' @param offset A vector indicating how much the tiles should be shifted relative
#' to the x-axis.
#'
#' @export
#'
#' @note code inspired by [barplot.default()].
#' @references
#' Sauro, J. (2012) "Report Usability Issues in a User by Problem Matrix" \url{http://www.measuringusability.com/blog/problem-matrix.php}; retrieved at June 23rd, 2012
#' Sauro, J., & Lewis, J. R. (2012). Quantifying the User Experience. Elsevier.
#' @importFrom grDevices colors gray palette
#' @importFrom graphics axis plot.new plot.window rect title
#' @examples
#' x <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
#' pxpplot_default(x, darkfigure = 3, percentage = FALSE)
pxpplot_default <- function (x, darkfigure = NULL,
                             percentage = TRUE, names.arg = NULL,
                             horiz = FALSE,
                             density = NULL, angle = 45, col = NULL, col.opt = TRUE,
                             border = par("fg"),
                             main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                             xpd = TRUE,
                             axes = TRUE, cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
                             axis.lty = 0,
                             plot = TRUE, add = FALSE, offset = 0, ...)
{
  if (!is.defectgrid(x)) stop("'m' must be a defect grid")
  if(plot && is.null(col)) {
    col <- colors()[seq(from=253L, to=153L, by=-5L)]
  }
  if(!is.null(darkfigure))
    if(darkfigure < 0)
      stop("The dark figure cannot be less than zero")

  NR <- nrow(x)
  NC <- ncol(x)

  if(!is.null(darkfigure)) {	# extend matrix by a number of zeroed columns
    x <- cbind(x, matrix(rep(0, length.out = NR*darkfigure), nrow = NR))
    NC <- ncol(x)
  }

  #    if (is.logical(legend.text))
  #        legend.text <- if (legend.text && is.matrix(m))
  #            rownames(m)

  width  <- rep(1 / NC, length.out = NC)
  height <- rep(1 / NR, length.out = NR)
  offset <- rep(as.vector(offset), length.out = length(width))
  if(!horiz) {
    w.r <- cumsum(width) # x coordinate
    delta <- width/2
  } else {
    w.r <- cumsum(height)# y coordinate
    delta <- height/2
  }
  w.m <- w.r - delta
  w.l <- w.m - delta   # x coordinate

  if(!horiz) {
    y.r <- cumsum(height)# y coordinate, right corner
    delta <- height/2
  } else {
    y.r <- cumsum(width) # x coordinate
    delta <- width/2
  }
  y.m <- y.r - delta
  y.l <- y.m - delta   # y coordinate, left corner

  ## determine color value of all squares
  squares <- (x > 0) * 1 # count observations only once, ignore repetitions
  .colsum <- colSums(squares)
  squares <- squares[order(NR:1),] # reverse order for Ss Nr.1 shall be on top
  squares <- squares[,order(.colsum, decreasing=TRUE)] # sort cols by frequency
  if (!.isAlive(colnames(squares)))
    colnames(squares) <- order(.colsum, decreasing=TRUE)
  else
    colnames(squares) <- colnames(squares)[order(.colsum, decreasing=TRUE)]
  .colsum <- sort(.colsum, decreasing=TRUE)

  if(isTRUE(col.opt)) {	# equi-distance: scale colors to an optimal range
    col.range <- length(col)

    # convert values in squares to ranking
    col.index <- match(.colsum,unique(.colsum)) # create vector of (color) indices

    if (!(0 %in% .colsum)) {   # if there are no zero columns, leave zero out of color palette
      i <- length(unique(col.index))
      colsteps <- round( seq(from=col.range, to=1+(col.range/i), length.out=i ) )
    } else {
      colsteps <- round( seq(from=col.range, to=1, length.out=length(unique(col.index)) ) )
    }

    for (i in 1L:NC)
      squares[,i] <- squares[,i] * colsteps[col.index[i]];
    squares[squares==0] <- squares[squares==0]+1 # no zeroes allowed
  }
  else {	# scale colors to range from [0, max%]; steps match the proportions in the data
    squares <- t(t(squares) * .colsum)
    #print(squares) #DEBUG
    toPercentage <- length(col) / max(.colsum)
    #print(toPercentage) #DEBUG
    squares <- round( squares * toPercentage )
    squares[squares==0] <- squares[squares==0]+1 # no zeroes allowed
    #print(squares) #DEBUG
  }

  ## Compute dimensions of output window
  if (horiz) {
    xlim <- c(min(y.l), max(y.r))
    ylim <- c(min(w.l), max(w.r))
  }
  else {
    xlim <- c(min(w.l), max(w.r))
    ylim <- c(min(y.l), max(y.r))
  }

  ## PLOTTING
  if (plot) {
    palette.backup <- palette()
    if(!is.null(col))
      palette(col)
    else
      palette(gray(0:(NR-1)/(NR-1)))
    on.exit(palette(palette.backup))

    opar <- if (horiz)
      par(xaxs = "i", yaxs = "i", xpd = xpd, mar=c(5,4,8,2) + 0.1)
    else
      par(xaxs = "i", yaxs = "i", xpd = xpd, mar=c(5,4,8,2) + 0.1)
    on.exit(par(opar), add=TRUE)
    if (!add) {
      plot.new()
      plot.window(xlim, ylim, log = "", ...)
    }

    ##<CONTENT
    for (i in 1L:NC) {
      if(is.null(col))
        rect( offset[i]+w.l[i], y.l,
              offset[i]+w.r[i], y.r,
              angle = angle, density = density, col = col[squares[,i]],
              border = border)
      else
        rect( offset[i]+w.l[i], y.l,
              offset[i]+w.r[i], y.r,
              angle = angle, density = density, col = col[squares[,i]],
              border = border)
    }
    #CONTENT/>
    ## Defect axis
    if (axes) {
      if(is.null(names.arg)) names.arg <- colnames(x)
      if(is.null(names.arg)) names.arg <- colnames(squares) #1:NC
      if(length(names.arg) != NC) stop( "Incorrect number of axis names" )
      if(horiz)
        axis(2, at = y.m, labels=names.arg, lty=axis.lty, cex.axis=cex.names, ...)
      else
        axis(3, at = w.m, labels=names.arg, lty=axis.lty, cex.axis=cex.names, ...)
    }
    ## Percentages at the bottom (or right of horiz=TRUE)
    if(percentage) {
      .colsum <- (.colsum / NR) * 100
      collabels <- paste(format(.colsum, digits=3L, nsmall=1L),"%", sep="")
      if(horiz)
        axis(4, at = y.m, labels=collabels, lty = axis.lty, cex.axis = cex.names, ...)
      else
        axis(1, at = w.m, labels=collabels, lty = axis.lty, cex.axis = cex.names, ...)
    }
    ## User Axis
    if (axes) {
      if(horiz)
        axis(3, w.m, NR:1, cex.axis = cex.axis, tick=FALSE, ...)
      else
        axis(2, y.m, NR:1, cex.axis = cex.axis, tick=FALSE, ...)
    }

    ## Add a legend
    #        if (!is.null(legend.text))
    #		{
    #            legend.col <- rep(col, length.out = length(legend.text))
    #            if ((horiz & beside) || (!horiz & !beside))
    #			{
    #                legend.text <- rev(legend.text)
    #                legend.col <- rev(legend.col)
    #                density <- rev(density)
    #                angle <- rev(angle)
    #            }
    #            xy <- par("usr")
    #            if (is.null(args.legend))
    #			{
    #                legend(xy[2L] - xinch(0.1), xy[4L] - yinch(0.1),
    #                  legend = legend.text, angle = angle, density = density,
    #                  fill = legend.col, xjust = 1, yjust = 1)
    #            }
    #            else
    #			{
    #                args.legend1 <- list(x = xy[2L] - xinch(0.1),
    #                  y = xy[4L] - yinch(0.1), legend = legend.text,
    #                  angle = angle, density = density, fill = legend.col,
    #                  xjust = 1, yjust = 1)
    #                args.legend1[names(args.legend)] <- args.legend
    #                do.call("legend", args.legend1)
    #            }
    #        }

    ## Main title and axis titles
    if(is.null(main)) main <- "Defect by User Matrix"
    if(is.null(xlab)) {
      if(!horiz) xlab <- "Defect"
      else xlab <- "User"
    }
    if(is.null(ylab)) {
      if(!horiz) ylab <- "User"
      else ylab <- "Defect"
    }
    title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)

    invisible(squares)
  }
  else squares
  #! PLOTTING
}




#' @describeIn pxpplot Creates a plot using `ggplot2`.
#'
#' @details
#' Dimension names of a matrix must be complete or will be overwritten.
#'
#' It is usually not necessary to call `pxpplot_gg` directly. Call `pxpplot()`
#' instead.
#' @return A `ggplot` graph
#' @export
#' @importFrom stats reshape setNames
pxpplot_gg <- function(x, darkfigure = NULL, percentage = TRUE, ...) {
  if (is.matrix(x)) {
    y <- as.data.frame(x)
    y$partid <- row.names(y)

    # give it proper names if necessary
    if (any(lengths(dimnames(x)) != dim(x))) {
      dimnames(x) <- list(paste0("part", 1:nrow(x)), paste0("d.", 1:ncol(x)))
      x <- reshape(x, idvar="id", direction = "long")
      x$partid <- row.names(x)
      names(x)[names(x) == "time"] <- "defect"
      names(x)[names(x) == "d"] <- "occurence"
    }
  } else if (is.data.frame(x)) {
    n <- colnames(x)
    if (anyNA(match(c("cc", "a", "0", "f"), n)))
      stop("Missing columns in data frame 'x'")
  } else {
    stop(sprintf("Cannot process object 'x' of class '%s'",
                 paste0(class(x), collapse=", ")))
  }


  ggplot(data = x, aes(x=defect, y=partid, fill=d)) +
    geom_tile() +
    scale_fill_gradient(low = "white")
}