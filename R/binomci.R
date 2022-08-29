

#' binomci
#'
#' @param x number of successes in the test.
#' @param n number of independent trials in the test.
#' @param conf.level confidence level of the interval.
#' @note Based on a recommendation by Brown, Cai, and DasGupta (2001)
#' `binomci` uses the Wilson method (Wilson, 1927) (even if Sauro and Lewis, 2005,
#' recommend the adjusted Wald).
#' @return A data frame that lists the confidence intervals
#' @export
#' @importFrom binom binom.confint
#' @examples
#' binomci(0:5, 10)
binomci <- function(x, n, conf.level = 0.95 ) { # returns ci
  #
  ci <- binom.confint(x, n, conf.level = conf.level, methods = "wilson", tol = 1e-8)

  # add "width" of confidence interval to table
  width <- ci["upper"] - ci["lower"]
  dimnames(width)[[2]] <- "width"
  ci2 <- cbind(ci, width)

  # provide a graphical representation of the intervals
  ###.barplot.ci( ci2$lower, ci2$upper)
  as.data.frame(ci2)
}

