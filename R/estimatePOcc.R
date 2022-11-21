

#' estPOcc
#' @description Estimate the visibility of usability defects from a data set.
#'
#' @param dg A problem by participant matrix. A defect grid object or matrix
#' with the same structure.
#' @param method Currently only "binom".
#' @param adj An adjustment method for small sample sizes. See [adjustPOcc()].
#'
#' @return An estimate of the visibility.
#' @details
#' `estPOcc` function ignores empty columns with only zeroes.
#'
#' Estimating the visibility `p.occ` from a sample can be tricky. Simply
#' averaging it from a sample can substantially overestimate it's value
#' when the sample is small. And "small" means 20 participants or fewer
#' (Hertzum and Jacobsen, 2001).
#' @export
#' @references
#' Hertzum, M., & Jacobsen, N. J. (2001). The evaluator effect: A chilling
#' fact about usability evaluation methods. International Journal of
#' Human–Computer Interaction, 13, 421–443.
estPOcc <- function(dg, method = c("binom"), adj = c("none", "GT", "defl", "both")) {
  method <- match.arg(method)
  if(method != "binom")
    stop("Other methods than binomial have not been implemented")

  if (!is.matrix(dg))
    stop("Argument 'dg' must be a defect grid or matrix object")

  # Determine for subjects across how many defects each subject stumbled
  DefectsPerSubj <- margin.table(dg > 0, 1L)
  # How many times was each defect found?
  DefectCount <- margin.table(dg > 0, 2L)

  nSample <- nrow(dg)
  Ntotal  <- ndefects(dg) #sum(marginSums(x, 2L) > 0L) #ncol(x)
  pByRow  <- DefectsPerSubj / Ntotal
  N1 <- sum(DefectCount == 1)
  pAvg <- mean(pByRow)

  adj <- match.arg(adj)
  if (adj != "none")
    result <- adjustPOcc(pAvg, nSample, Ntotal, N1, adj = adj)
  else
    result <- pAvg

  return(result)
}



#' @describeIn estPOcc Estimating the visibility of events from a
#' sample may considerably overestimate it's value when samples are small.
#' Small samples 20 participants have a bias that can result in substantial
#' overestimation of its value. If you call `estPOcc` with an argument
#' `adj` other than `none`, `estPOcc` will adjust already by calling this
#' function.
#'
#' @param p.occ a non-adjusted estimate of the visibility of an event.
#' @param nSample the sample size.
#' @param Ntotal the total number of events (e.g. usability defects).
#' @param N1 the number of events occurring only once.
#' @param adj the adjustment technique. Either "GT" for a Good-Turing correction,
#' "defl" for the deflated adjustment, or "both" to return the average of both
#' correction methods (the default as recommended by Turner, 2006; Sauro & Lewis, 2012).
#'
#' @return Returns corrected values of `p`.
#' @references
#' Sauro, J., & Lewis, J. R. (2012). Quantifying the User Experience. Elsevier.
#'
#' Turner, C. W., Lewis, J. R., & Nielsen, J. (2006). Determining Usability Test
#' Sample Size. In W. Karwowski & B. Raton, International Encyclopedia of Ergonomics
#' and Human Factors (2 ed., Vol. 3, pp. 3084–3088). CRC Press.
#' @export
#' @keywords internal
adjustPOcc <- function(p.occ, nSample, Ntotal, N1, adj = c("GT", "defl", "both")) {
  .gt <- function(.p, .Ntotal, .N1) .p / (1 + .N1/.Ntotal)
  .defl <- function(.p, .nSample) (.p - 1/nSample) * (1 - 1/nSample)

  adj <- match.arg(adj)
  if (!.isAlive(adj)) adj <- "both"
  result <- switch(adj,
        GT = .gt(p.occ, Ntotal, N1),
        defl = .defl(p.occ, nSample),
        both = (.gt(p.occ, Ntotal, N1) + .defl(p.occ, nSample)) / 2
  )
  return(result)
}





#' estPObs
#'
#' Estimate the chance of observing from a sample.
#'
#' @inheritParams estPOcc
#' @param estp.occ an estimate of the visibility of an event.
#' See [estPOcc()]
#'
#' @return An estimate of the chance of observing.
#' @export
estPObs <- function(dg, estp.occ) {
  getPObs(estp.occ, nrow(dg))
}


# This could be a second method to estimate `p.occ`.
#' #' pDefect
#' #'
#' #' Estimates the defect visibility `p.occ` (i.e. the probability of a usability
#' #' defect to become evident in each test session) from an existing data set.
#' #'
#' #' This version uses observations.
#' #'
#' #' @param defects Vector with number of observation per usability defect.
#' #' @param n Number of independent test sessions.
#' #' @param method
#' #'
#' #' @return The visibility (a scalar).
#' #' @export
#' #'
#' #' @examples
#' pdefect <- function( defects, n, method = c("mixed", "good-turing", "normal", "zero-truncation", "none") )
#' {
#'   stop("This function has not been implemented, yet")
#'   if(n == 0) stop("pDefect.obs requires the number of independent test sessions (n)")
#'
#'   method <- match.arg(method)
#'   if(method == "zero-truncation") {
#'     stop("Not implemented, yet") # TODO: implement
#'   }
#'   else {
#'     p.obs <- sum(defects) / (length(defects) * n )
#'     Correct.pDefect(p.obs, n, d.unique = length(defects[defects==1]), d.total = length(defects), method)
#'   }
#' }


