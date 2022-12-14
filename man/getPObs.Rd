% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ndefects.R
\name{getPObs}
\alias{getPObs}
\title{getPObs}
\usage{
getPObs(p.occ = 0.31, n)
}
\arguments{
\item{p.occ}{Probability of OCCurrence: how big is the probability of
the problems that shall be detected when testing a single user
(Nielsen, 2000, assumes p.occ = L = 31\%). Numeric scalar between 0 and 1.}

\item{n}{Number of participants of the sample.}
}
\value{
Returns the "chance of observing" each defect at least once in the study.

That is not the number of defects you will find. It is merely an estimate
and the exact number of found defects is a random result.
}
\description{
Calculate the probability to see each defect in a sample at least once.
}
\references{
Nielsen, J. (2000). Why You Only Need to Test with 5 Users.
https://www.nngroup.com/articles/why-you-only-need-to-test-with-5-users/
}
