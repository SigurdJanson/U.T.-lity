

#' binomci
#'
#' @param x number of successes in the test.
#' @param n number of independent trials in the test.
#' @param ... Further arguments passed on to the `ci`-generating function. these are foremost:
#' `conf.level` and `method`. See `binom::binom.confint()`.
#' @note Based on a recommendation by Brown, Cai, and DasGupta (2001)
#' `binomci` uses the Wilson method (Wilson, 1927) by default (even if Sauro and Lewis, 2005,
#' recommend the adjusted Wald).
#' @return A `ci` object lists the confidence intervals.
#' @export
#' @importFrom binom binom.confint
#' @examples
#' binomci(0:5, 10)
binomci <- function(x, n, ...) {
  .args <- list(...)

  if ("conf.level" %in% ...names())
    conf.level <- .args[["conf.level"]]
  else
    conf.level <- .args[["conf.level"]] <- 0.95

  if ("method" %in% ...names()) {
    method <- .args[["method"]]
  } else {
    method <- .args[["method"]] <- "wilson"
  }

  if ("alternative" %in% ...names()) {
    if (.args[["alternative"]] != "two-sided")
      stop("'binomci' only computes two-sided confidence intervals")
  }

  ci <- do.call(binom.confint, args=c(x=x, n=n, tol=1e-8, .args))

  result <- ci_new(x, ci["lower"], ci["upper"],
                   .lvl = conf.level,
                   .alt = "",
                   .dstr = "binomial",
                   .mthd = method,
                   .call = deparse(match.call()))
  return(result)
}

