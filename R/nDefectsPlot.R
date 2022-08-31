#' nDefectsPlot
#'
#' Returns a plot that shows the number of detected usability defects as a function of
#' the number of test participants. This chart is well-known from Nielsens AlertBox
#' "Why You Only Need to Test with 5 Users" (2000).
#'
#' @param p.occ Probability of Occurrence: how big is the probability of the
#' problems that can be detected when testing a single user (default: p.occ = L = 31%, as
#' assumed by Nielsen, 2000).
#' @param subjects Range of number of subjects (i.e. sample size)
#' @param ... Further arguments handed over to `plot()`.
#' @param growth if `TRUE` the plot shows an additional curve for each visibility
#' that shows the growth from one sample size to the next.
#' @param col see [graphics::par()].
#' @param pch see [graphics::par()].
#' @param las style of axis labels, see [graphics::par()].
#' @param xlim,ylim the x and y limits of the plot, see [graphics::plot()].
#' @param axes show axes (`TRUE`/`FALSE`).
#'
#' @details Without any arguments the function provides the chart from Nielsens AlertBox
#' (Nielsen, 2000)
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics grid legend lines mtext
nDefectsPlot <- function (p.occ = 0.31, subjects = 0:15, growth=FALSE,
                                col = NULL, pch=20,
                                las = 1, xlim = NULL, ylim = NULL, axes = TRUE, ...) {
  .args <- list(...)
  d.total <- 100
  nlines <- length(p.occ)

  # get the range for the x and y axis
  xrange <- range(subjects)
  yrange <- range(0, 100)

  # set up the plot
  xlab <- .args[["xlab"]]
  if (!.isAlive(xlab)) xlab <- "Number of Subjects"
  ylab <- .args[["ylab"]]
  if(!.isAlive(ylab)) ylab <- "Found Defects (%)"

  plot(xrange, yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE )

  if(is.null(col)) {
    colors <- rainbow(nlines)
  } else {
    if(length(col) != nlines) stop("Number of colors does not match number of lines")
  }

  # add lines
  linetype <- c(1:nlines)
  plotchar <- seq(18L, 18L + nlines,1) # 18 is the base character (diamond)
  for (i in 1:nlines) {
    lines(subjects, ndefects(p.occ[i], d.total, subjects),
          type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
  }

  if(growth&& length(subjects) > 1) {
    for (i in 1:nlines) {
      nsubject <- length(subjects)
      values <- ndefects(p.occ[i], d.total, subjects)
      diff <- c(NaN, values[2:nsubject] - values[1:(nsubject-1)])
      colors <- rainbow(nlines, alpha=0.5)
      lines(subjects, diff,
            type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
    }
  }

  # add a title and subtitle
  main <- .args[["main"]]
  if(!.isAlive(main)) main <- "Detection Rate for Usability Problems"
  sub <- .args[["sub"]]
  if(!.isAlive(sub)) sub <- "as outlined by Nielsen & Landauer (1993)"
  title(main)
  mtext(sub)

  # add a legend (if there is more than one curve)
  if(length(p.occ) > 1) {
    legend(max(xrange)*0.9, max(yrange)*0.5, p.occ, cex=0.8, col=colors,
           pch=plotchar, lty=linetype, title="Probability")
  }

  if(axes) {
    axis(1, at=seq(0, max(subjects), 5), labels=seq(0, max(subjects), 5))
    axis(2, at=seq(0,max(yrange), by=max(yrange)*0.1), las=las)
  }
  grid()
}

