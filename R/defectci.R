


#' defectci
#'
#' Confidence Intervals of Usability Defects
#'
#' @param freq Frequency of found defects
#' @param n (interpretation depends on distribution)
#' @param distr the probability distribution used to estimate the confidence interval.
#' Default is `binom`.
#' @param ... Further arguments passed on to the `ci`-generating function. these are foremost:
#' `conf.level`, `alternative`, and `method`. See `binomci()` or `poissonci()`.
#' @return A `ci`object with the confidence intervals.
#' @export
#'
#' @examples defectci(0:5, 10)
defectci <- function(freq, n, distr = c("binom", "poisson"), ...) {
  if (missing(freq))
    stop("Provide a frequency with which the defect occurred")
  if (missing(n))
    stop("Provide a sample size")

  distr <- match.arg(distr)
  result <- switch(distr,
                   "binom" = binomci(freq, n), #, conf.level = 0.95
                   "poisson" = poissonci(freq, n, ...)
  )
  return(result)
}

