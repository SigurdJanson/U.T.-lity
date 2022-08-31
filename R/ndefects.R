

#' ndefects
#'
#' Number of defects that can be found in a usability test given
#' the visibility `p.occ` (probability of occurrence) and
#' the sample size `n`.
#'
#' @param p.occ Probability of Occurrence: how big is the probability of
#' the problems that shall be detected when testing a single user
#' (Nielsen, 2000, assumes p.occ = L = 31%).
#' @param d.total Total number of existing (usability) problems in the design.
#' @param n Number of subjects
#'
#' @returns
#' Use `d.total = 1` to get a percentage. That is the "chance of observing" each
#' defect at least once in the study.
#' Otherwise the function returns
#' the expected number of defects you find with the given sample size. Note that
#' you will not find this exact number. It is simply an estimator and the exact
#' number of found defects is a random result.
ndefects <- function(p.occ = 0.31, d.total = 1L, n) {
  # PRECONDITIONS
  if (!.isAlive(d.total) || any(d.total <= 0L))
    stop("Total number of existing problems must be specified and > 0")
  if (!.isAlive(n) || any(n < 0))
    stop("Number of test subjects must be specified and >= 0")
  if (p.occ < 0 || p.occ > 1)
    stop("Probability 'p.occ' must be 0 <= p.occ <= 1")

  #
  d.total * (1 - (1 - p.occ)^n)
}

