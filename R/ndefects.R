

#' getPObs
#'
#' Calculate the probability to see each defect in a sample at least once.
#'
#' @param p.occ Probability of OCCurrence: how big is the probability of
#' the problems that shall be detected when testing a single user
#' (Nielsen, 2000, assumes p.occ = L = 31%). Numeric scalar between 0 and 1.
#' @param n Number of participants of the sample.
#'
#' @returns
#' Returns the "chance of observing" each defect at least once in the study.
#'
#' That is not the number of defects you will find. It is merely an estimate
#' and the exact number of found defects is a random result.
#' @references
#' Nielsen, J. (2000). Why You Only Need to Test with 5 Users.
#' https://www.nngroup.com/articles/why-you-only-need-to-test-with-5-users/
#' @export
getPObs <- function(p.occ = 0.31, n) {
  # PRECONDITIONS
  if (!.isAlive(n) || any(n < 0))
    stop("Number of test subjects must be specified and >= 0")
  if (p.occ < 0 || p.occ > 1)
    stop("Probability 'p.occ' must be 0 <= p.occ <= 1")

  #
  (1 - (1 - p.occ)^n)
}

