# Confidence Intervals for Poisson mean
# Taken from https://github.com/AndriSignorell/DescTools/R/StatsAndCIs.r
# See https://search.r-project.org/CRAN/refmans/DescTools/html/PoissonCI.html




#' poissonci
#'
#' Computes the confidence intervals of a poisson distributed variable's lambda.
#' Several methods are implemented, see details.
#'
#' @param x number of events.
#' @param n time base for event count.
#' @param conf.level confidence level, default is 0.95.
#' @param alternative character string specifying the side of the confidence
#' interval, must be one of "two.sided" (default), "left" or "right".
#' @param method character string specifying which method to use; can be one
#' out of "wald", "score" (default), "exact" or "byar".
#' @note Tanusit (2012) recommends "Score" intervals for small sample `n`
#' and "Wald" for large ones.
#' @return A vector with 3 elements for estimate, lower confidence interval,
#' and upper for the upper one.
#' @references Patil & Kulkarni (2012).
#' ["Comparison of Confidence Intervals for the Poisson Mean: Some New Aspects"](https://www.ine.pt/revstat/pdf/rs120203.pdf).
#' REVSTAT - Statistical Journal, 10(2),  p. 211–227
#' Tanusit, M. (2012). ["Two-Side Confidence Intervals for the Poisson Means"](http://www.ijmo.org/papers/189-S083.pdf).
#' International Journal of Modeling and Optimization, 2 (5), p. 589-591
#' @export
#' @examples
#' poissonci(0:5, 10)
#' @importFrom stats poisson.test qnorm
poissonci <- function(x, n = 1, conf.level = 0.95,
                     alternative = c("two.sided", "less", "greater"),
                     method = c("exact", "score", "wald", "byar")) {

  if(missing(method)) method <- "score"
  if(missing(alternative)) alternative <- .alternative["two.sided"]


  iPoissonCI <- function(x, n = 1, conf.level = 0.95, sides = c("two.sided", "less", "greater"),
                         method = c("exact", "score", "wald", "byar")) {

    # see also: pois.conf.int {epitools}

    sides <- match.arg(sides, choices = c("two.sided", "less", "greater"), several.ok = FALSE)
    if(sides != .alternative["two.sided"])
      conf.level <- 1 - 2*(1-conf.level)

    if(length(conf.level) != 1)
      stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)
      stop("'conf.level' has to be in [0.5, 1]")

    alpha <- 1 - conf.level
    z <- qnorm(1 - alpha/2)

    lambda <- x/n

    switch( match.arg(arg=method, choices=c("exact", "score", "wald", "byar")),
            "exact" = {
              ci <- poisson.test(x, n, conf.level = conf.level)$conf.int
              lwr.ci <- ci[1]
              upr.ci <- ci[2]
            },
            "score" = {
              term1 <- (x + z^2/2)/n
              term2 <- z * n^-0.5 * sqrt(x/n + z^2/(4*n))
              lwr.ci <- term1 - term2
              upr.ci <- term1 + term2
            },
            "wald" = {
              term2 <- z*sqrt(lambda/n)
              lwr.ci <- lambda - term2
              upr.ci <- lambda + term2
            },
            "byar" = {
              xcc <- x + 0.5
              zz  <- (z/3) * sqrt(1/xcc)
              lwr.ci <- (xcc * (1 - 1/(9 * xcc) - zz)^3)/n
              upr.ci <- (xcc * (1 - 1/(9 * xcc) + zz)^3)/n
            }
    )

    ci <- c( est=lambda, lwr.ci=lwr.ci, upr.ci=upr.ci )

    if(sides=="left")
      ci[3] <- Inf # upr.ci
    else if(sides=="right")
      ci[2] <- -Inf # lwr.ci

    return(ci)
  }

  # handle vectors
  # which parameter has the highest dimension
  lst <- list(x=x, n=n) #--, conf.level=conf.level, sides=alternative, method=method)
  maxdim <- max(lengths(lst))
  lgp <- lapply(lst, rep, length.out=maxdim) # recycle all params to maxdim

  # Compute
  res <- sapply(1:maxdim, \(i) iPoissonCI(x=lgp$x[i], n=lgp$n[i],
                                          conf.level=conf.level,
                                          sides=alternative,
                                          method=method))
  ci_new(res[1,], res[2,], res[3,],
         conf.level, alternative, "poisson", method, deparse(match.call()))
}
