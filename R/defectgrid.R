#' Defect Grid
#'
#' The defect grid is a class to store a PROBLEM-BY-PARTICIPANT matrix.
#'
#' @param x a numeric or logical matrix with participants in rows and defects
#' in columns.
#' @details the `defectgrid` object has an attribute `type` that can be either
#' `exists` or `count`. `exist` indicates that each cell contains only information
#' about the existence of the defect but not it's frequency (contrary to `count`).
#'
#' A second class "U.T.lity" indicates that this object is from this package.
#'
#' If `x` is a vector it will be coerced to a matrix with one row (i.e. participant).
#' @return A defect grid object.
#' @export
defectgrid_new <- function(x) {
  type <- c("count", "exists")
  if (is.logical(x))
    type <- type[2L]
  else if (is.numeric(x)) {
    if (is.integer(x)  && all(is.element(unique(x), 0:1L)))
      type <- type[2L]
    else
      type <- type[1L]
  } else {
    stop("Objects must bei either numeric or logical to become a defect grid")
  }
  if (!is.matrix(x)) x <- matrix(x, 1L)

  attr(x, "type") <- type
  class(x) <- c("U.T.lity", "defectgrid", class(x))
  return(x)
}




#' @describeIn defectgrid_new Function to check if an object is a defect grid.
#'
#' @param x any R object.
#' @return TRUE/FALSE
#' @export
is.defectgrid <- function(x)
  !any(inherits(x, c("U.T.lity", "defectgrid"), which=TRUE) == 0)


