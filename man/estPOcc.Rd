% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimatePOcc.R
\name{estPOcc}
\alias{estPOcc}
\alias{adjustPOcc}
\title{estPOcc}
\usage{
estPOcc(dg, method = c("binom"), adj = c("none", "GT", "defl", "both"))

adjustPOcc(p.occ, nSample, Ntotal, N1, adj = c("GT", "defl", "both"))
}
\arguments{
\item{dg}{A problem by participant matrix. A defect grid object or matrix
with the same structure.}

\item{method}{Currently only "binom".}

\item{adj}{the adjustment technique. Either "GT" for a Good-Turing correction,
"defl" for the deflated adjustment, or "both" to return the average of both
correction methods (the default as recommended by Turner, 2006; Sauro & Lewis, 2012).}

\item{p.occ}{a non-adjusted estimate of the visibility of an event.}

\item{nSample}{the sample size.}

\item{Ntotal}{the total number of events (e.g. usability defects).}

\item{N1}{the number of events occurring only once.}
}
\value{
An estimate of the visibility.

Returns corrected values of \code{p}.
}
\description{
Estimate the visibility of usability defects from a data set.
}
\details{
\code{estPOcc} function ignores empty columns with only zeroes.

Estimating the visibility \code{p.occ} from a sample can be tricky. Simply
averaging it from a sample can substantially overestimate it's value
when the sample is small. And "small" means 20 participants or fewer
(Hertzum and Jacobsen, 2001).
}
\section{Functions}{
\itemize{
\item \code{adjustPOcc()}: Estimating the visibility of events from a
sample may considerably overestimate it's value when samples are small.
Small samples 20 participants have a bias that can result in substantial
overestimation of its value. If you call \code{estPOcc} with an argument
\code{adj} other than \code{none}, \code{estPOcc} will adjust already by calling this
function.

}}
\references{
Hertzum, M., & Jacobsen, N. J. (2001). The evaluator effect: A chilling
fact about usability evaluation methods. International Journal of
Human–Computer Interaction, 13, 421–443.

Sauro, J., & Lewis, J. R. (2012). Quantifying the User Experience. Elsevier.

Turner, C. W., Lewis, J. R., & Nielsen, J. (2006). Determining Usability Test
Sample Size. In W. Karwowski & B. Raton, International Encyclopedia of Ergonomics
and Human Factors (2 ed., Vol. 3, pp. 3084–3088). CRC Press.
}
\keyword{internal}
