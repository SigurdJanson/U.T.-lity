% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pxpplot.R
\name{pxpplot_gr}
\alias{pxpplot_gr}
\title{pxpplot_gr}
\usage{
pxpplot_gr(
  x,
  darkfigure = 0L,
  percentage = TRUE,
  names.arg = NULL,
  horiz = FALSE,
  density = NULL,
  angle = 45,
  col = NULL,
  col.opt = TRUE,
  border = par("fg"),
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  xpd = TRUE,
  axes = TRUE,
  cex.axis = par("cex.axis"),
  cex.names = par("cex.axis"),
  axis.lty = 0,
  plot = TRUE,
  add = FALSE,
  offset = 0,
  ...
)
}
\arguments{
\item{x}{object containing usability problems and their frequency}

\item{darkfigure}{add columns (rows) with zero probability that represent hidden problems.}

\item{percentage}{show the total percentage of each defect.}

\item{names.arg}{A vector of names to be plotted below each tile.}

\item{horiz}{If true users will be plotted on the horizontal axis (default is  \code{FALSE})}

\item{density}{A vector giving the density of shading lines (in lines per inch) for the tiles.}

\item{angle}{Slope of shading lines, given as an angle in degrees (counter-clockwise),
for the tiles.}

\item{col}{A vector giving the colors to be used to show defect frequencies}

\item{col.opt}{if TRUE, colors will be optimised with equal distances.}

\item{border}{The color to be used for the border of the tiles.
Use \code{border = NA} to omit borders. If there are shading lines,
\code{border = TRUE} means use the same colour for the border as for the shading lines.}

\item{main}{An overall title for the plot.}

\item{sub}{A sub title for the plot.}

\item{xlab}{Label for the horizontal axis.}

\item{ylab}{Label for the vertical axis.}

\item{xpd}{Logical. Should tiles be allowed to go outside region?}

\item{axes}{Logical. Indicated whether both axes should be drawn on the plot.}

\item{cex.axis}{Expansion factor for numeric axis labels.}

\item{cex.names}{Expansion factor for axis names (tile labels).}

\item{axis.lty}{The graphics parameter \code{lty} applied to the axis and
tick marks of the 'defect' (default horizontal) axis. Note that by default
the axis is suppressed.}

\item{plot}{Logical. If  \code{FALSE}, nothing is plotted.}

\item{add}{Logical. Specifies if grid should be added to an already existing
plot (default is \code{FALSE}).}

\item{offset}{A vector indicating how much the tiles should be shifted relative
to the x-axis.}

\item{...}{Additional parameters will be passed to the plotting function.}
}
\description{
Creates a \code{pxpplot} using the default graphics device. It is usually
not necessary to call \code{pxpplot_gg} directly. Call \code{pxpplot()} instead.
}
\note{
code inspired by \code{\link[=barplot.default]{barplot.default()}}.
}
\examples{
\dontrun{
x <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
pxpplot_gr(x, darkfigure = 3, percentage = FALSE)
}
}
\references{
Sauro, J. (2012) "Report Usability Issues in a User by Problem Matrix" \url{http://www.measuringusability.com/blog/problem-matrix.php}; retrieved at June 23rd, 2012
Sauro, J., & Lewis, J. R. (2012). Quantifying the User Experience. Elsevier.
}
