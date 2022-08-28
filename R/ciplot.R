# Confidence
#
# Draws an illustration of the confidence interval as pie chart.
#
# @param lower vector with the lower limits of the confidence intervals.
# @param upper vector with the upper limits of the confidence intervals.
#
# @return A chart visualising confidence intervals
# @importFrom graphics pie
# pie.ci <- function( lower, upper ) {
#   width <- upper - lower
#   pie(c(width, 1-width), labels = NA, edges = 200, radius = 1.6, clockwise = TRUE,
#       init.angle = 360*lower+90, col = c("#000000","#FFFFFF") )
# }



#' Confidence Plots
#'
#' Draws an illustration of the confidence interval as horizontal chart. To easily verify
#' overlaps.
#'
#' @param x An object containing the confidence intervals
#' @param ... arguments to be passed to/from other methods.
#' @return A chart visualising confidence intervals
#' @export
ciplot <- function(x, ...) UseMethod("ciplot")



#' @describeIn ciplot ...
#' @export
ciplot.default <- function(x, ...) stop("Not able to determine the type of 'x'")



#' @describeIn ciplot Simplified base version to draw confidence limit plots.
#' @param lower numeric vector with the lower limits of the confidence intervals.
#' @param upper numeric vector with the upper limits of the confidence intervals.
#' @param names A vector of names applied to each pair of `lower`/`upper`
#'
#' @return A chart visualising confidence intervals
#' @export
#' @importFrom graphics barplot par
ciplot_default <- function(lower, upper, names=NA, ...) {
  if (any(lower > upper)) stop("Upper limits must be greater than the lower ones")

  # maximumrows <- 13 # used to set the bar height, ylim = c(0,maximumrows)

  lower <- rev(lower) # reverse order
  upper <- rev(upper)
  width <- upper - lower
  top <- upper + (width * 0.05) # 1 - upper
  bottom <- lower - (width * 0.05)
  h <- rbind(lower, width, top)

  barplot(h, width = 1,
          names.arg = names, legend.text = FALSE, beside = FALSE,
          horiz = TRUE, density = NULL,
          col = c("#FFFFFF","#000000", "#FFFFFF"),
          border = par("fg"),
          xlim = c(min(bottom), max(top)), xpd = FALSE,
          axisnames = .isAlive(names),
          ...)
}


#' @describeIn ciplot Plot the result of a call to [t.test()].
#' @export
ciplot.htest <- function(x, ...) {
  ciplot_default(
    lower=x$conf.int[1],
    upper=x$conf.int[2], ...
  )
}



#' @describeIn ciplot Plot the result of a call to [ci.numeric()].
#' @export
ciplot.cidf <- function(x, ...) {
  ciplot_default(
    lower=x$lower,
    upper=x$upper,
    names=rownames(x), ...
  )
}
