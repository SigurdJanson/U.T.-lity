##
## the density function (dbinom) gives the probability (integral) from x1 to x2.
## the distribution function (cdf, pbinom) gives the probability (integral) from -infinity to x.
## the quantile function (qbinom) is the inverse of the distribution function.
##

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



#' Confidence
#'
#' Draws an illustration of the confidence interval as horizontal chart. To easily verify
#' overlaps.
#'
#' @param lower numeric vector with the lower limits of the confidence intervals.
#' @param upper numeric vector with the upper limits of the confidence intervals.
#' @param names A vector of names applied to each pair of `lower`/`upper`
#'
#' @return A chart visualising confidence intervals
#' @export
#' @importFrom graphics barplot par
bar.ci <- function(lower, upper, names=NA) {
  if (any(lower > upper)) stop("Upper limits must be greater than the lower ones")

  # maximumrows <- 13 # used to set the bar height, ylim = c(0,maximumrows)

  lower <- rev(lower) # reverse order
  upper <- rev(upper)
  width <- upper - lower
  top <- upper + (width * 0.05) # 1 - upper
  bottom <- lower - (width * 0.05)
  h <- rbind(lower, width, top)

  barplot(h, width = 1, space = NULL,
          names.arg = names, legend.text = FALSE, beside = FALSE,
          horiz = TRUE, density = NULL,
          col = c("#FFFFFF","#000000", "#FFFFFF"), border = par("fg"),
          xlim = c(min(bottom), max(top)), xpd = FALSE, log = "",
          axes = TRUE, axisnames = .isAlive(names),
          plot = TRUE, axis.lty = 0, offset = 0, add = FALSE)
}


#' ci.numeric
#'
#' Tests if a value `xi` is inside the confidence interval around the mean of the given
#' sample `x`.
#'
#' @param x a (non-empty) numeric vector of data values or a list of vectors.
#' @param mu an assumed true value µ of the mean to test against.
#' @param conf.level confidence level of the interval.
#' @param plot TRUE if the result should be plotted.
#' @details
#' If x is a list of vectors `ci` will use the names of the list to
#' label the plot.
#'
#' This function is a wrapper for [t.test()].
#' @return A data frame that lists the confidence intervals
#' @export
#' @importFrom stats t.test
#' @examples
#' ci.numeric(rnorm(15, 3.7, 1.8), 4.4)
#' ci.numeric(list(a=rnorm(15, 3.7, 1.8), f=rnorm(15, 3.7, 1.8)), c(4.4, 4.6), plot=FALSE)
ci.numeric <- function(x, mu, conf.level = 0.95, plot=TRUE) {
  if (is.atomic(x))
    x <- list(x)
  if (!is.list(x)) stop("Unknown format of argument 'x'")
  # replicate mu when necessary for equal length
  if (length(x) > length(mu)) {
    mu <- rep_len(mu, length(x))
  }

  # Create empty data frame - use length for efficiency
  result <- data.frame(mean = numeric(length(x)),
                       lower = numeric(length(x)),
                       upper = numeric(length(x)),
                       width = numeric(length(x)),
                       inside = logical(length(x)),
                       stringsAsFactors = FALSE)
  for (i in seq_along(x)) {
    test <- t.test(x[[i]], y = NULL, alternative = "two.sided", mu = mu[i], conf.level = conf.level)
    result[i,] <- list(test$estimate,
                       test$conf.int[1],
                       test$conf.int[2],
                       test$conf.int[2] - test$conf.int[1],
                       mu[i] >= test$conf.int[1] & mu[i] <= test$conf.int[2])
  }

  # provide a graphical representation of the intervals
  if (plot)
    bar.ci(result$lower, result$upper, names(x))

  result
}

