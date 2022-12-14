% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomci.R
\name{binomci}
\alias{binomci}
\title{binomci}
\usage{
binomci(x, n, ...)
}
\arguments{
\item{x}{number of successes in the test.}

\item{n}{number of independent trials in the test.}

\item{...}{Further arguments passed on to the \code{ci}-generating function. these are foremost:
\code{conf.level}, \code{alternative}, and \code{method}. See \code{binom::binom.confint()}.}
}
\value{
A \code{ci} object containing the observed proportions
and the lower and upper bounds of the confidence interval around them.
}
\description{
binomci
}
\details{
The \code{method} argument allows only a single value (though \code{binom.confint()}
would accept several arguments at the same time).
}
\note{
Based on a recommendation by Brown, Cai, and DasGupta (2001)
\code{binomci} uses the Wilson method (Wilson, 1927) by default.
}
\examples{
binomci(0:5, 10)
}
\references{
Wilson, E. B. (1927). Probable Inference, the Law of Succession,
and Statistical Inference. Journal of the American Statistical Association, 22 (158),
209–212. https://doi.org/10.2307/2276774
}
