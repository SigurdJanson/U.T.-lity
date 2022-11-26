

#' Confidence Intervals
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
#' ci_new(1:2, 0:1, 2:3, .alt="two.sided", .dstr="joke", .mthd="guessing", .call="none")
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

  if (.isAlive(.alt) && is.character(.alt)) {
    .alt <- match.arg(.alt, setNames(.alternative, NULL))
    attr(result, "alternative") <- .alt
  }
  if (.isAlive(.lvl) && is.numeric(.lvl))
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



#' @describeIn ci_new Function to check if an object is a `ci` object.
#'
#' @param x any R object.
#' @return TRUE/FALSE
#' @export
is.ci <- function(x)
  !any(inherits(x, c("U.T.lity", "ci"), which=TRUE) == 0)




#' @describeIn ci_new Prints a ci-object
#'
#' @param x an object of class `ci`.
#' @param dropInf drops upper/lower columns with `Inf` values for one-sided
#' intervals. Ignored when alternative hypothesis is two-sided.
#' @param scientific Either a logical specifying whether elements of a
#' real or complex vector should be encoded in scientific format, or an
#' integer penalty (see [options("scipen")]). Missing values correspond
#' to the current default penalty.
#' @param ... further arguments passed on to `print.data.frame()`.
#'
#' @return Returns `x` invisibly.
#' @export
print.ci <- function(x, dropInf = TRUE, scientific = FALSE, ...) {
  .prettyPerc <- function(x)
    paste0(format(x*100, digits=3, drop0trailing=TRUE), "%")

  if (!is.ci(x))
    stop("x is not a ci-object from U.T.lity")

  .cl <- attr(x, "conf.level")
  .alt <- attr(x, "alternative")

  px <- x # px == x to print

  # Ensure well ordered columns
  neworder <- c("lower", "est", "upper")
  # intersect: keep only those elements in 'neworder' that exist in 'px'
  # setdiff:   add the elements that 'px' has in addition to 'neworder'
  neworder <- c(intersect(neworder, names(px)), setdiff(names(px), neworder))
  px <- px[neworder] # finally: order it

  # Drop upper/lower limit for one-sided confidence intervals
  if (dropInf && .alt != .alternative["two.sided"]) {
    if (.alt == .alternative["less"]) {
      px[names(px) == "lower"] <- NULL
    } else if (.alt == .alternative["greater"]) {
      px[names(px) == "upper"] <- NULL
    }
  }

  # Use numeric labels for confidence limits instead of "upper"/"lower"
  if (.isAlive(.cl)) {
    if (.alt == .alternative["two.sided"]) {
      names(px)[names(px) == "lower"] <- .prettyPerc((1-.cl) / 2)
      names(px)[names(px) == "upper"] <- .prettyPerc(1-((1-.cl) / 2))
    } else if (.alt == .alternative["less"]) {
      if (!dropInf)
        names(px)[names(px) == "lower"] <- .prettyPerc(0.0) #"0.0"
      names(px)[names(px) == "upper"] <- .prettyPerc(.cl)
    } else if (.alt == .alternative["greater"]) {
      names(px)[names(px) == "lower"] <- .prettyPerc(.cl)
      if (!dropInf)
        names(px)[names(px) == "upper"] <- .prettyPerc(1.0) #"1.0"
    }
  }

  if (isFALSE(scientific))
    oldopt <- options(scipen = 100L)
  if (is.numeric(scientific))
    oldopt <- options(scipen = scientific)

  cat(gettext("Call:"), attr(x, "call"), "\n")
  print.data.frame(px, digits, ...)

  if (exists("oldopt"))
    options(oldopt)

  invisible(x)
}



#' ci.numeric
#'
#' Tests if a value `mu` is inside the confidence interval around the mean of the given
#' sample `x`.
#'
#' @param x a (non-empty) numeric vector of data values or a list
#' of such vectors.
#' @param mu an assumed true value µ of the mean to test against.
#' Values will be replicated if there are more data sets `x` than
#' elements of `mu`.
#' @param conf.level confidence level of the interval.
#' @details
#' This function performs a one-sample [t.test()] and returns the
#' confidence interval. See [t.test()] for more information.
#' @return A `ci` object containing the confidence intervals.
#' @export
#' @importFrom stats t.test
#' @examples
#' ci.numeric(rnorm(15, 3.7, 1.8), 4.4)
#' ci.numeric(list(a=rnorm(15, 3.7, 1.8), f=rnorm(15, 3.7, 1.8)), c(4.4, 4.6), plot=FALSE)
ci.numeric <- function(x, mu = 0, conf.level = 0.95) {
  if (is.numeric(x))
    x <- list(x)
  if (!is.list(x)) stop("Unknown format of argument 'x'")
  # replicate mu when necessary for equal length
  if (length(x) > length(mu)) {
    mu <- rep_len(mu, length(x))
  }

  # Create empty data - use length for efficiency
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
}

