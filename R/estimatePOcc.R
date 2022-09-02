

#' estimatePOcc
#'
#' @param x A problem by participant matrix (e.g. a defect grid object).
#' @param method Currently only "binom".
#' @param adj An adjustment method for small sample sizes.
#'
#' @return An estimate of the visibility
#' @details
#' Estimating the visibility `p.occ` from a sample can be tricky. Simply
#' averaging it from a sample can substantially overestimate it's value
#' when the sample is small. And small means 20 participants or fewer
#' (Hertzum and Jacobsen, 2001).
#'
#'  from a small sample because small sample estimates of p
#' (from fewer than 20 participants) have a bias that can result in
#' substantial overestimation of its value .
#' @export
estimatePOcc <- function(x, method = c("binom"), adj = c("none", "GT", "defl", "both")) {
  method <- match.arg(method)
  if(method != "binom")
    stop("Other methods than binomial have not been implemented")

  if (!is.matrix(x))
    stop("")

  # Determine for subjects across how many defects each subject stumbled
  DefectsPerSubj <- margin.table(x > 0, 1L)
  # How many times was each defect found?
  DefectCount <- margin.table(x > 0, 2L)

  nSample <- nrow(x)
  Ntotal  <- ncol(x)
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



adjustPOcc <- function(p, nSample, Ntotal, N1, adj = c("GT", "defl", "both")) {
  .gt <- function(.p, .Ntotal, .N1) .p / (1 + .N1/.Ntotal)
  .defl <- function(.p, .nSample) (p - 1/nSample) * (1 - 1/nSample)

  adj <- match.arg(adj)
  result <- switch(adj,
        GT = .gt(p, Ntotal, N1),
        defl = .defl(p, nSample),
        both = (.gt(p, Ntotal, N1) + .defl(p, nSample)) / 2
  )
  return(result)
}

