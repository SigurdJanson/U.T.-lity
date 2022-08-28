##
## the density function (dbinom) gives the probability (integral) from x1 to x2.
## the distribution function (cdf, pbinom) gives the probability (integral) from -infinity to x.
## the quantile function (qbinom) is the inverse of the distribution function.
##




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
  if (.isAlive(names(x)))
    rownames(test) <- names(x)

  # provide a graphical representation of the intervals
  if (plot)
    ciplot_default(result$lower, result$upper, names(x))

  result
}

