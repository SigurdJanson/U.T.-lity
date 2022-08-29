


#' Confidence Intervals of Usability Defects
#'
#' @param freq Frequency of found defects
#' @param n (interpretation depends on distribution)
#' @param distr the probability distribution used to estimate the confidence interval.
#' Default is `binom`.
#' @param ... Further arguments passed on to the `ci`-generating function. these are foremost:
#' `conf.level`, `alternative`, and `method`. See `binomcii()` or `poissonci()`.
#' @details
#' Lewis (1992) suggests that the binomial distribution is sufficient to compute the statistics
#' behind usability defects. Nielsen & Landauer (1993) suggest a poisson distribution.
#' @return A `ci`object with the confidence intervals.
#' @export
#' @references
#' Nielsen, J., & Landauer, T. K. (1993). A mathematical model of the finding of
#' usability problems. Proceedings of the INTERACT ’93 and CHI ’93 Conference on
#' Human Factors in Computing Systems, 206–213. https://doi.org/10.1145/169059.169166
#'
#' Lewis, J. R. (1992). Sample Sizes for Usability Studies (technical report 54.711). IBM.
#'
#' @examples defectci(0:5, 10)
defectci <- function(freq, n, distr = c("binom", "poisson"), ...) {
  if (missing(freq))
    stop("Provide a frequency with which the defect occurred")
  if (missing(n))
    stop("Provide a sample size")

  distr <- match.arg(arg=distr, choices=c("binom", "poisson"))
  result <- switch(distr,
                   "binom" = binomci(freq, n), #, conf.level = 0.95
                   "poisson" = poissonci(freq, n, ...)
  )
  return(result)
}

