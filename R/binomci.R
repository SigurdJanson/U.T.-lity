

#' binomci
#'
#' @param x number of successes in the test.
#' @param n number of independent trials in the test.
#' @param ... Further arguments passed on to the `ci`-generating function. these are foremost:
#' `conf.level`, `alternative`, and `method`. See `binom::binom.confint()`.
#' @note Based on a recommendation by Brown, Cai, and DasGupta (2001)
#' `binomci` uses the Wilson method (Wilson, 1927) by default (even if Sauro and Lewis, 2005,
#' recommend the adjusted Wald).
#' @return A `ci` object containing the observed proportions
#' and the lower and upper bounds of the confidence interval.
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

  if ("alternative" %in% ...names()) {
    alternative <- .args[["alternative"]]
  } else {
    alternative <- .alternative["two.sided"]
  }

  if ("method" %in% ...names()) {
    method <- .args[["method"]]
  } else {
    method <- .args[["method"]] <- "wilson"
  }

  # if 1-sided, convert 1-sided confidence to 2-sided by doubling it
  if (alternative != .alternative["two.sided"])
    conf.level <- 1 - ((1 - conf.level) * 2)

  ci <- do.call(binom.confint, args=c(list(x=x, n=n, tol=1e-8), .args))

  result <- ci_new(x,
                   if (alternative == .alternative["less"]) 0 else ci["lower"],
                   if (alternative == .alternative["less"]) 1 else ci["upper"],
                   .lvl = conf.level,
                   .alt = alternative,
                   .dstr = "binomial",
                   .mthd = method,
                   .call = deparse(match.call()))
  return(result)
}

