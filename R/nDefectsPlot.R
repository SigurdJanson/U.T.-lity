
#' nDefectsPlot
#'
#' Returns a plot that shows the probability to detect each usability
#' defect (at least once) as a function of the number of test participants.
#' This type of chart is well-known from Nielsens AlertBox "Why You Only
#' Need to Test with 5 Users" (2000).
#'
#' @param p.occ Probability of Occurrence: how big is the probability of the
#' problems that can be detected when testing a single user (default: p.occ = L = 31%, as
#' assumed by Nielsen, 2000).
#' @param subjects Range of number of subjects (i.e. sample size)
#' @param growth if `TRUE` the plot shows an additional curve for each visibility
#' that shows the growth from one sample size to the next.
#' @param col see [graphics::par()]. Must be as long as there are lines to draw.
#' @param lib draws the plot either with `ggplot2` or `graphics`.
#' @param ... further arguments handed over to the plotting function. For `plot()` this
#' could be e.g. `las = 1`.
#' @details Without any arguments the function provides the chart from Nielsens AlertBox
#' (Nielsen, 2000).
#' @export
#' @order 1
nDefectsPlot <- function (p.occ = 0.31, subjects = 0:15, growth = FALSE,
                             col = NULL, lib = c("ggplot", "graphics"),
                             ...) {
  lib <- match.arg(lib)

  if (lib == "graphics")
    nDefectsPlot_gr(p.occ, subjects, growth, col, ...)
  else
    nDefectsPlot_gg(p.occ, subjects, growth, col, ...)
}



#' @describeIn nDefectsPlot Alternative to plot with the `graphics` library.
#' @param las style of axis labels, see [graphics::par()].
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics grid legend lines mtext
#' @order 3
nDefectsPlot_gr <- function (p.occ = 0.31, subjects = 0:15, growth = FALSE,
                                col = NULL, las = 1, ...) {
  .args <- list(...)
  nlines <- length(p.occ)

  # get the range for the x and y axis
  xrange <- range(subjects)
  yrange <- range(0, 100)

  # set up the plot
  xlab <- .args[["xlab"]]
  if (!.isAlive(xlab)) xlab <- gettext("Number of Subjects")
  ylab <- .args[["ylab"]]
  if(!.isAlive(ylab)) ylab <- paste(gettext("Chance of Observing"), "(%)")

  plot(xrange, yrange, type="n", xlab=xlab, ylab=ylab, axes=FALSE ) # add axes later

  if(is.null(col)) {
    colors <- rainbow(nlines)
  } else {
    if(length(col) != nlines) stop("Number of colors does not match number of lines")
  }

  # add lines
  linetype <- c(1:nlines)
  plotchar <- seq(18L, 18L + nlines,1) # 18 is the base character (diamond)
  for (i in 1:nlines) {
    lines(subjects, getPObs(p.occ[i], subjects)*100,
          type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
  }

  if(growth && length(subjects) > 1) {
    for (i in 1:nlines) {
      nsubject <- length(subjects)
      values <- getPObs(p.occ[i], subjects)*100
      diff <- c(NaN, values[2:nsubject] - values[1:(nsubject-1)])
      colors <- rainbow(nlines, alpha=0.5)
      lines(subjects, diff,
            type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
    }
  }

  # add a title and subtitle
  main <- .args[["main"]]
  if(!.isAlive(main)) main <- gettext("Detection Rate for Usability Problems")
  sub <- .args[["sub"]]
  if(!.isAlive(sub)) sub <- gettext("as outlined by Nielsen & Landauer (1993)")
  title(main)
  mtext(sub)

  # add a legend (if there is more than one curve)
  if(length(p.occ) > 1) {
    legend(max(xrange)*0.9, max(yrange)*0.5, p.occ, cex=0.8, col=colors,
           pch=plotchar, lty=linetype, title=gettext("Probability"))
  }

  axis(1, at=seq(0, max(subjects), 5), labels=seq(0, max(subjects), 5))
  axis(2, at=seq(0,max(yrange), by=max(yrange)*0.1), las=las)
  grid()
}



#' @describeIn nDefectsPlot Defects plot with the `ggplot2` library.
#' @export
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom scales percent
#' @order 2
nDefectsPlot_gg <- function(p.occ = 0.31, subjects = 0:15, growth=FALSE,
                            col = NULL, ...) {
  .args <- list(...)
  nlines <- length(p.occ)

  dt <- data.frame(
    subjects = rep(subjects, nlines),
    p.obs = as.vector(sapply(p.occ, \(x) getPObs(p.occ=x, subjects))),
    p.occ = rep(p.occ, each=length(subjects))
  )

  p <- ggplot(dt, aes(x = subjects, y = p.obs,
                      group = factor(p.occ), color = factor(p.occ))) +
         geom_point(size=1L) + geom_line() +
         scale_y_continuous(labels = scales::percent) +
         labs(
           x = gettext("Number of Subjects"),
           y = paste(gettext("Chance of Observing"), "(%)"),
           color = gettext("Visibility"),
           title = gettext("Detection Rate for Usability Problems"),
           caption = gettext("as outlined by Nielsen & Landauer (1993)"))
  if (.isAlive(col))
    p <- p + scale_colour_manual(values=col)

  return(p)
}
