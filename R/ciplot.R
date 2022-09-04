
#' Confidence Plots
#'
#' Draws an illustration of the confidence interval as horizontal chart. To easily verify
#' overlaps.
#'
#' @param x An object containing the confidence intervals
#' @param ... arguments to be passed to/from other methods.
#' @return A chart visualising confidence intervals
#' @export
ciplot <- function(x, ...) UseMethod("ciplot")



#' @describeIn ciplot ...
#' @export
ciplot.default <- function(x, ...) stop("Not able to determine the type of 'x'")



#' @describeIn ciplot Simplified base version to draw confidence limit plots.
#'
#' @param lower numeric vector with the lower limits of the confidence intervals.
#' @param upper numeric vector with the upper limits of the confidence intervals.
#' @param names A vector of names applied to each pair of `lower`/`upper`.
#' @param engine draw the plot either with `ggplot2` or `graphics`.
#' @param ... further arguments to be passed on to other methods
#'
#' @return A chart visualising confidence intervals
#' @export
ciplot_default <- function(lower, upper, names=NA, engine = c("ggplot", "graphics"), ...) {
  engine <- match.arg(engine)
  if (engine == "ggplot" && !require(ggplot2) )
    engine <- "graphics"
  else
    "ggplot2"

  if (engine == "ggplot" || missing(engine))
    ciplot_gg(lower, upper, names, ...)
  else
    ciplot_gr(lower, upper, names, ...)
}



#' @describeIn ciplot Create a `ciplot` with the core `graphics` library
#' @export
#' @importFrom graphics barplot par
ciplot_gr <- function(lower, upper, names=NA, ...) {
  if (any(lower > upper)) stop("Upper limits must be greater than the lower ones")

  # maximumrows <- 13 # used to set the bar height, ylim = c(0,maximumrows)

  lower <- rev(lower) # reverse order
  upper <- rev(upper)
  width <- upper - lower
  top <- upper + (width * 0.05) # 1 - upper
  bottom <- lower - (width * 0.05)
  h <- rbind(lower, width, top)

  # TODO: make sure that users can overwrite thos arguments below
  #  by using `...`

  barplot(h, width = 1,
          names.arg = names, legend.text = FALSE, beside = FALSE,
          horiz = TRUE, density = NULL,
          col = c("#FFFFFF","#000000", "#FFFFFF"),
          border = par("fg"),
          xlim = c(min(bottom), max(top)), xpd = FALSE,
          axisnames = .isAlive(names),
          ...)
}



#' @describeIn ciplot Create a `ciplot` with the `ggplot2` library
#' @return Returns a `ggplot`
#' @export
#' @importFrom ggplot2 ggplot aes geom_crossbar .data
ciplot_gg <- function(lower, upper, names=NA, ...) {

  if (any(lower > upper)) stop("Upper limits must be greater than the lower ones")

  dt <- data.frame(
    lower = rev(lower), # reverse order
    upper = rev(upper),
    width = rev(upper - lower),
    mean  = rev(upper + lower) / 2,
    names = if (.isAlive(names)) rev(names) else rev(seq_along(lower))
  )
  print(dt)
  top <- dt$upper + (dt$width * 0.05) # 1 - upper
  bottom <- dt$lower - (dt$width * 0.05)

  ggplot(dt, aes(x = mean, y = factor(.data$names))) +
    geom_crossbar(aes(xmin = lower, xmax = upper, fill = 1), fatten=1) +
    scale_fill_gradient(low = "black", high = "black") +
    labs(
      x = "Value",
      y = "Category",
      title = "Compare Confidence Intervals",
      caption = NULL)
}



#' @describeIn ciplot Plot the result of a call to [t.test()].
#' @export
ciplot.htest <- function(x, ...) {
  ciplot_default(
    lower=x$conf.int[1],
    upper=x$conf.int[2], ...
  )
}



#' @describeIn ciplot Plot the result of a call to [ci.numeric()].
#' @export
ciplot.cidf <- function(x, ...) {
  ciplot_default(
    lower=x$lower,
    upper=x$upper,
    names=rownames(x), ...
  )
}
