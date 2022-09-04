
#' simplot
#' @export
simplot <- function(x, ...) UseMethod("simplot")

#' @describeIn simplot method for sample sizes estimated by simulation
#' @export
simplot.simulation <- function(x, ...) NextMethod("print")


#' @describeIn simplot ...
#'
#' @param x A `samplesize` object
#' @param ... further arguments passed on to other methods.
#' @param x0 if `TRUE` the x-axis is extended to show `x=0`. The default `FALSE`
#' optimises it to the data range.
#' @param y0 if `TRUE` the y-axis is extended to show `y=0`. The default `FALSE`
#' optimises it to the data range.
#'
#' @return prints `x` and returns it invisibly.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_rect geom_point labs theme_light
#' position_nudge scale_x_continuous scale_y_continuous .data
simplot.samplesize <- function(x, x0 = FALSE, y0 = FALSE, ...) {
  if (!attr(x, "converged")) {
    warning("Sample size simulation has not converged")
  }

  ypad <- ifelse(y0, 1.05, 1.01) # padding
  srange <- attr(x, "search.range") # original search range
  nmax <- length(attr(x, "result")) + srange[1] -1 # highest sample size

  # plot dimensions
  pdim.x <- c(0, nmax) # min = 0 or srange[1]
  if (y0)
    pdim.y <- c(0, 1 * ypad)
  else
    pdim.y <- c(min(attr(x, "result")) * (2-ypad), 1 * ypad)

  #-nafill <- max(srange[1] - pdim.x[1] - 1, 0)

  dt <- data.frame(
    sample=seq(srange[1], nmax),
    confidence = attr(x, "result")
  )
  highlighted <- dt[dt$confidence >= attr(x, "conf.level"), ]
  default     <- dt[dt$confidence < attr(x, "conf.level"), ]
  rects <- data.frame(start=min(dt$sample), end=nmax, group=seq_along(0))

  #-print(dt)

  ggplot(dt, aes(x=.data$sample, y=.data$confidence)) +
    geom_line() +
    geom_point(data=default, aes(x=.data$sample, y=.data$confidence),
               color='black', size=1) +
    geom_point(data=highlighted, aes(x=.data$sample, y=.data$confidence),
               color='orange', size=2) +
    #scale_x_continuous(expand = c(0, 0))+ #, limits = pdim.x) + #
    scale_y_continuous(expand = c(0, 0), limits = pdim.y) +
    labs(
      x = paste0("Sample Size (", srange[1], " - ", nmax, ")"),
      y = "Confidence",
      title = paste("Confidence Depending on Sample Size"),
      caption = paste0("Sample size required to find ", attr(x, "desired")*100,
                      "% of defects with a confidence of ", attr(x, "conf.level"))
    ) +
    geom_rect(data=rects, inherit.aes=FALSE, #data is needed for transparency (weirdly so)
              aes(xmin=srange[1], xmax=pdim.x[2],
                  ymin=min(attr(x, "conf.level")), ymax=1, group=1),
              color="transparent", fill="orange", alpha=0.3) +
    theme_light()
}
