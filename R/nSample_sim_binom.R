
#' samplesize
#'
#' Constructor function to create sample size objects created by simulation.
#'
#' @param simresult a vector containing the confidences per sample size.
#' The length of the vector must match the range of `srange`.
#' @param conf.level the targeted confidence level.
#' @param srange two-element vector with the lower and upper value the simulation
#' was made.
#' @param .call the original function call.
#' @param ... further arguments passed on to other methods.
#'
#' @return a `samplesize` object.
#' @export
#'
#' @examples
new_samplesim <- function(simresult, conf.level, srange, .call, ...) {
  matched <- simresult >= conf.level

  runs <- rle(matched)
  lastrun <- length(runs$values)
  nlowest <- min(srange) + ifelse(runs$values[1L], 0L, runs$lengths[1L])
  nhigh   <- (length(simresult) + min(srange)) - ifelse(runs$values[lastrun], runs$lengths[lastrun]-1L, -Inf)

  x <- nhigh
  class(x) <- c("samplesize", "simulation")
  attr(x, "result") <- simresult
  attr(x, "conf.level") <- conf.level
  attr(x, "converged") <- !is.infinite(nhigh)
  attr(x, "search.range") <- c(min(srange), max(srange))
  attr(x, "call") <- .call

  return(x)
}


#' @describeIn samplesize method for sample sizes estimated by simulation
#' @export
print.simulation <- function(x, ...) NextMethod("print")


#' @describeIn samplesize
#'
#' @param x A `samplesize` object
#' @param ... further arguments passed on to other methods.
#'
#' @return prints `x` and returns it invisibly.
#' @export
print.samplesize <- function(x, ...) {
  if (!attr(x, "converged")) {
    print("Sample size simulation has not converged")
  }

  cat(paste0("Recommended sample size: ", as.integer(x)), "\n") #", quote = FALSE)

  matched <- attr(x, "result") >= attr(x, "conf.level")
  runs <- rle(matched)
  nlowest <- attr(x, "search.range")[1]
  nhigh   <- x

  cat("Smallest sample size meeting the conf. level:", nlowest, "\n")
  cat("Lowest before saturation:", nhigh, "\n")
  cat("\n")
  cat("Summarized simulation results (in runs)\n")
  cat("Count           ", format(runs$lengths, width=6L), "\n")
  cat("Meets confidence", format(runs$values, width=6L), "\n")

  invisible(x)
}


#' nSample_sim
#'
#' @param p.occ the visibility of each event.
#' @param threshold the desired number of events (defects) to find with the
#' given confidence (`conf.level`).
#' @param conf.level the confidence level (default 0.95).
#' @param search.range limit the search to sample sizes in this range
#' (default `c(3, 99`).
#' @param maxiter maximum number of samples to draw.
#' @param stop reduce the number of iterations
#'
#' @return an object with the classes "samplesize" and "simulation"
#' (@seealso samplesize).
#' It contains the simulation results and recommended sample size.
#' @export
#' @importFrom stats rbinom
#' @examples
#' nSample_sim_binom()
nSample_sim_binom <- function(p.occ = 0.31, threshold=0.8, conf.level=0.95,
                              search.range=c(3L, 99L), maxiter=250L, stop=10L) {
  # nmin smallest sample size to start the search
  nmin <- search.range[1]
  # nmax largest sample size to consider
  nmax <- search.range[2]

  # Create empty vector to contain confidences
  confidence <- double(nmax-nmin)
  stopcount <- 0

  for (s in nmin:nmax) {
    # get iter sample
    result <- sapply(1:maxiter, \(x) rbinom(n=s, size=1, prob=p.occ))
    # get percentages of found defects
    percFound <- colSums(result > 0) / nrow(result)
    # get "confidence level" in this sample
    index <- s-nmin+1
    confidence[index] <- mean(percFound > 0)

    if (confidence[index] > conf.level)
      stopcount <- stopcount + 1
    else
      stopcount <- 0
    if (stopcount > stop) {
      confidence <- confidence[1:index]
      break
    }
  }

  # matched <- confidence >= conf.level
  # runs <- rle(matched)
  # nlowest <- nmin + ifelse(runs$values[1], 0L, runs$lengths[1])
  # nhigh   <- nmax - ifelse(runs$values[length(runs$values)], runs$lengths[length(runs$values)]-1, -Inf)

  # plot(nmin:nmax, confidence)

  return(
    new_samplesim(confidence, conf.level, search.range, .call=deparse(match.call()))
  )
}
