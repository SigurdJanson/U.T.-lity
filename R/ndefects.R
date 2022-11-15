

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


#' ndefects
#'
#' Gets the number of defects found in a test given the problem-by-participant matrix
#'
#' @param dg a problem by participant matrix (e.g. a defect grid object).
#' for each defect.
#' @details Defects are not counted if there are no observations (i.e. all
#' values in the column are zero) because defects that have not been observed
#' are only theoretical.
#' @return A scalar with the number of found defects.
#' @export
ndefects <- function(dg) sum(marginSums(dg, 2L) > 0L)



#' @describeIn ndark
#'
#' Estimates the number of unobserved (usability) defects.
#' @param d.obs The observed number of defects.
#' @param p.obs The chance of observing each defect at least once in the study.
#'
#' @return The dark figure (i.e. the number of defects that have been missed).
#' @export
#'
#' @examples
#' # Example in Sauro & Lewis, p. 155
#' .ndark(7L, 0.927) #> 7.6
.ndark <- function(d.obs, p.obs) {
  total <- d.obs / p.obs
  black <- total - d.obs
  return(black)
}



#' ndark
#'
#' Returns the number of unobserved (usability) defects based on an existing data set.
#' @details Estimation is done using the binomial model.
#'
#' @param dg a problem by participant matrix (e.g. a defect grid object)
#' for each defect.
#' @param method Adjustment method used to estimate unobserved defects
#' (@seealso{estimatePOcc()}).
#'
#' @references Lewis (2001). Evaluation of Procedures for Adjusting Problem-Discovery Rates
#' Estimated From Small Samples. International Journal of Human–Computer Interaction, 13 (4), 445–479
#' @export
ndark <- function(dg, method = c("none", "GT", "defl", "both") ) {
  method <- match.arg(method)

  n <- nrow(dg)
  p.occ <- estimatePOcc(dg, adj=method)
  p.obs <- getPObs(p.occ, n)
  d.obs <- ndefects(dg) # get number of defects

  # compute hidden defects
  return(.ndark(d.obs, p.obs))
}
