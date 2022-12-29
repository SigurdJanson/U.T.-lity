
#' samplesize
#'
#' Constructor function to create sample size objects created by simulation.
#'
#' @param simresult a double vector containing the confidences per sample size.
#' The length of the vector must match the range of `srange`.
#' @param conf.level the targeted confidence level.
#' @param desired.events the targeted proportion of evens/defects that need to
#' be found with the given confidence.
#' @param srange two-element vector with the lower and upper value the simulation
#' was made.
#' @param .call the original function call.
#' @param ... further arguments passed on to other methods.
#'
#' @details The function takes the raw simulation results, interprets them and
#' binds all together to get an object with all information required to replicate
#' the simulation.
#'
#' @return a `samplesize` object.
#' The object value is an integer with the sample size. The attributes are:
#' \describe{
#'   \item{result}{The results vector as provided in `simresult`.}
#'   \item{conf.level}{The desired confidence to reach.}
#'   \item{desired}{The targeted proportion of evens/defects that need to
#' be found with the given confidence.}
#'   \item{converged}{A logical that is `FALSE` in case the simulation did not converge.}
#'   \item{search.range}{The search range in which the simulation had been run (`srange`).}
#'   \item{call}{The function call used to get the simulation result.}
#' }
#' @export
new_samplesim <- function(simresult, conf.level, desired.events, srange, .call, ...) {
  if (length(simresult) > max(srange) - min(srange) + 1)
    stop("Result vector 'simresult' does not match the given range 'srange'")

  matched <- simresult >= conf.level

  runs <- rle(matched)
  lastrun <- length(runs$values)
  nhigh   <- length(simresult) + min(srange) -
    ifelse(runs$values[lastrun], runs$lengths[lastrun], -Inf)

  x <- structure(nhigh,
                 class=c("samplesize", "simulation"),
                 result = simresult,
                 conf.level = conf.level,
                 desired = desired.events,
                 converged = !is.infinite(nhigh),
                 search.range = c(min(srange), max(srange)),
                 call = .call)
  return(x)
}


#' @describeIn new_samplesim method for sample sizes estimated by simulation
#' @export
print.simulation <- function(x, ...) NextMethod("print")


#' @describeIn new_samplesim method for sample sizes estimated by simulation
#'
#' @param x A `samplesize` object
#' @param ... further arguments passed on to other methods.
#'
#' @return prints `x` and returns it invisibly.
#' @export
print.samplesize <- function(x, ...) {
  if (!attr(x, "converged")) {
    cat("Sample size simulation has not converged")
  }

  cat(paste0(gettext("Recommended sample size: "), as.integer(x)), "\n") #", quote = FALSE)

  matched <- attr(x, "result") >= attr(x, "conf.level")
  runs <- rle(matched)
  nlowest <- attr(x, "search.range")[1]
  nhigh   <- x

  cat(gettext("Smallest sample size meeting the conf. level:"), nlowest, "\n")
  cat(gettext("Lowest before saturation:"), nhigh, "\n")
  cat("\n")
  cat(gettext("Summarized simulation results (in runs)"), "\n")
  cat(gettext("Count"), format(runs$lengths, width=6L), "\n")
  cat(gettext("Meets confidence"), format(runs$values, width=6L), "\n")

  invisible(x)
}


#' nSample_sim
#'
#' Determines the sample size for a usability test. The criterion for this function
#' is a minimum rate of detection. It will determine the sample size that
#' "guarantees" the desired rate of detection with the given confidence.
#'
#' @param p.occ the visibility of each event.
#' @param threshold the desired number of events (defects) to find with the
#' given confidence (`conf.level`).
#' @param conf.level the confidence level (default 0.95).
#' @param search.range limit the search to sample sizes in this range
#' (default `c(3, 99)`).
#' @param maxiter maximum number of samples to draw.
#' @param stop reduce the number of iterations by using the stop count.
#' The algorithm will skip further iterations once it received a positive
#' result for `stop` times in a row.
#'
#' @details
#' This function uses a binomial distribution to model the detection of
#' usability defects. It calculates how many samples from a monte carlo
#' simulation reach the desired `threshold` for the chance of observing.
#' If the `threshold` is 85% every simulation sample will marked as
#' positive if 0.85 (85%) of the defects are found. Then it sums up all samples
#' and determines if the positive results reach `conf.level` of all samples.
#'
#' The function uses a search algorithm to determine the desired sample size.
#' The arguments `p.occ`, `threshold`, and `conf.level` are requirements to
#' determine the sample size.
#' `search.range`, `maxiter`, and `stop` control the search. They affect the
#' precision of the result. and they only need to be changed when the search
#' function does not return a valid result.
#'
#' @return an object with the classes `samplesize` and `simulation` containing
#' the simulation result (@seealso samplesize).
#' It contains the simulation results and recommended sample size.
#' @export
#' @importFrom stats rbinom
#' @examples
#' nSample_sim_binom()
nSample_sim_binom <- function(p.occ=0.31, threshold=0.8, conf.level=0.95,
                              search.range=c(3L, 99L), maxiter=250L, stop=10L) {
  # nmin smallest sample size to start the search
  nmin <- search.range[1L]
  # nmax largest sample size to consider
  nmax <- search.range[2L]

  # Create empty vector to contain confidences
  confidence <- double(nmax-nmin)
  stopcount <- 0L

  for (s in nmin:nmax) {
    # get iter sample
    result <- sapply(1:maxiter, \(x) rbinom(n=s, size=1L, prob=p.occ))
    # get percentages of found defects
    percFound <- colSums(result > 0) / nrow(result)
    # get "confidence level" in this sample
    index <- s-nmin+1
    confidence[index] <- mean(percFound > 0)

    if (confidence[index] > conf.level)
      stopcount <- stopcount + 1L
    else
      stopcount <- 0L
    if (stopcount > stop) {
      confidence <- confidence[1:index]
      break
    }
  }

  return(
    new_samplesim(confidence, conf.level, threshold, search.range, .call=deparse(match.call()))
  )
}
