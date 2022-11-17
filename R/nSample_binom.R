


#' nSample_binom
#'
#' @param p.occ The assumed probability of occurrence (or visibility) of the
#' defects in a study.
#' @param p.obs The desired chance of observing (i.e. the chance to see each
#' defect in the study at least once).
#' @param exact if `FALSE` (default) the result will be rounded up to get the
#' a whole number of participants. Otherwise it will return the exact result.
#'
#' @return A sample size for a usability study
#' @export
#'
#' @examples
#' nSample_binom(0.15, 0.8)
nSample_binom <- function(p.occ=0.20, p.obs=0.80, exact=FALSE) {
  # PRECONDITIONS
  if (p.occ < 0 || p.occ > 1)
    stop("Visibility must be 0 <= p.occ <= 1")
  if (p.obs < 0 || p.obs > 1)
    stop("Chance of observing must be 0 <= p.obs <= 1")

  #
  result <- log(1 - p.obs) / log(1 - p.occ)
  if (isTRUE(exact))
    return(result)
  else
    return(ceiling(result))
}


