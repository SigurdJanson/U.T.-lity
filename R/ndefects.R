

#' ndefects
#'
#' Get the probability to see each defect in a sample at least once.
#'
#' Number of defects that can be found in a usability test given
#' the visibility `p.occ` (probability of occurrence) and
#' the sample size `n`.
#'
#' @param p.occ Probability of Occurrence: how big is the probability of
#' the problems that shall be detected when testing a single user
#' (Nielsen, 2000, assumes p.occ = L = 31%). Numeric scalar between 0 and 1.
#' @param n Number of participants of the sample.
#'
#' @returns
#' Returns the "chance of observing" each defect at least once in the study.
#'
#' That is not the number of defects you will find. It is merely an estimate
#' and the exact number of found defects is a random result.
ndefects <- function(p.occ = 0.31, n) {
  # PRECONDITIONS
  if (!.isAlive(n) || any(n < 0))
    stop("Number of test subjects must be specified and >= 0")
  if (p.occ < 0 || p.occ > 1)
    stop("Probability 'p.occ' must be 0 <= p.occ <= 1")

  #
  (1 - (1 - p.occ)^n)
}

