##
## the density function (dbinom) gives the probability (integral) from x1 to x2.
## the distribution function (cdf, pbinom) gives the probability (integral) from -infinity to x.
## the quantile function (qbinom) is the inverse of the distribution function.
##


#' Confidence intervals
#'
#' @param .est a vector of estimates.
#' @param .lower a vector of the lower levels of the confidence intervals.
#' @param .upper a vector of the upper levels.
#' @param .lvl the `conf.level` argument (numeric, `.5 <= conf.level <= 1`).
#' @param .alt the `alternative` argument for functions calculating confidence levels
#' @param .dstr the probability distribution the interval is based on.
#' @param .mthd a method to compute the interval.
#' @param .call the function call used to create the interval.
#' @param ... further vectors added to the data (same length as .est).
#'
#' @return A `ci` object derived from a data frame with the columns `est`, `lower`, `upper`,
#' and further columns given through `...`.
#'
#' @details
#' Confidence intervals are stored in an S3 class derived from a data frame. In its basic form it has the following (mandatory) columns:
#'
#' * `est` - the estimate to which the confidence interval applies.
#' * `lower` - the lower boundary
#' * `upper` - the upper boundary
#'
#' Additional columns are allowed.
#'
#' Attributes are:
#'
#' * `alternative` - a character string describing the alternative hypothesis.
#' * `conf.level` - confidence level of the interval.
#' * `distr` - statistical distribution.
#' * `method` - method used to construct the intervals.
#' * `call` - a character representation of the original function call.
#'
#' @export
#'
#' @examples
#' ci_new(1:2, 0:1, 2:3, .alt="two-sided", .dstr="joke", .mthd="guessing", .call="none")
ci_new <- function(.est, .lower, .upper, .lvl, .alt, .dstr, .mthd, .call, ...) {
  if (!(.isAlive(.est) && .isAlive(.lower) && .isAlive(.upper)))
    stop("Confidence intervals require an estimate, a lower and an upper boundary")
  if (!all(.lower <= .upper))
    stop("Lower boundaries of an interval must be less than upper boundaries")

  result <- data.frame(est    = .est,
                       lower  = .lower,
                       upper  = .upper,
                       ...,
                       stringsAsFactors = FALSE)

  if (.isAlive(.alt) && is.character(.alt))
    attr(result, "alternative") <- .alt
  if (.isAlive(.lvl) && is.character(.lvl))
    attr(result, "conf.level") <- .lvl

  if (.isAlive(.dstr) && is.character(.dstr))
    attr(result, "distr") <- .dstr
  if (.isAlive(.mthd) && is.character(.mthd))
    attr(result, "method") <- .mthd
  if (.isAlive(.call) && is.character(.call))
    attr(result, "call") <- .call

  class(result) <- c("U.T.lity", "ci", class(result))
  return(result)
}



#' @describeIn ci_new Prints a ci-object
#'
#' @param x an object of class `ci`.
#' @param ... further arguments passed on to `print.data.frame()`.
#'
#' @return Returns `x` invisibly.
#' @export
print.ci <- function(x, ...) {
  if (!inherits(x, "U.T.lity"))
    stop("x is not a ci-object from U.T.lity")

  cat("Call:", attr(x, "call"), "\n")
  print.data.frame(x, ...)

  invisible(x)
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
  means  <- numeric(length(x))
  lower  <- numeric(length(x))
  upper  <- numeric(length(x))
  inside <- logical(length(x))

  for (i in seq_along(x)) {
    test <- t.test(x[[i]], y = NULL, mu = mu[i], conf.level = conf.level, alternative = "two.sided")
    means[i]  <- test$estimate
    lower[i] <- test$conf.int[1]
    upper[i] <- test$conf.int[2]
    inside[i]<- mu[i] >= test$conf.int[1] & mu[i] <= test$conf.int[2]
  }
  if (.isAlive(names(x)))
    names(means) <- names(x) # TODO: restore this

  result <- ci_new(means, lower, upper, inside=inside,
                   .lvl = conf.level,
                   .alt = test$alternative,
                   .dstr = "t distribution",
                   .mthd = test$method,
                   .call = deparse(match.call()))
  return(result)

  # provide a graphical representation of the intervals
  if (plot)
    ciplot_default(result$lower, result$upper, names(means))

  result
}

