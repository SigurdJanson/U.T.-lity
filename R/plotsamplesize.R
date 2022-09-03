
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
#'
#' @return prints `x` and returns it invisibly.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_rect geom_point labs theme_light
simplot.samplesize <- function(x, ...) {
  if (!attr(x, "converged")) {
    warning("Sample size simulation has not converged")
  }

  srange <- attr(x, "search.range")
  nmax <- length(attr(x, "result")) + srange[1] -1
  dt <- data.frame(
    sample=seq(srange[1], nmax),
    confidence = attr(x, "result"))
  highlighted <- dt[dt$confidence >= attr(x, "conf.level"), ]
  rects <- data.frame(start=0, end=nmax, group=seq_along(0))

  ggplot(dt, aes(sample, confidence)) +
    geom_line() +
    geom_point(data=highlighted,
               aes(sample, confidence),
               color='orange', size=2) +
    geom_rect(data=rects, inherit.aes=FALSE,
              aes(xmin=start, xmax=end,
                  ymin=min(attr(x, "conf.level")), ymax=1.00,
                  group=group),
              color="transparent", fill="orange", alpha=0.3) +
    labs(
      x = "Sample Size",
      y = "Confidence",
      title = paste("Confidence depends on Sample Size"),
      caption = paste("Sample Size required to find", attr(x, "desired"),
                      "% of defects with a confidence of", attr(x, "conf.level"))
    )
}
