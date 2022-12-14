

#' .recycle
#'
#' Coerces several vectors to the same length by replicating
#' elements of the shorter vectors.
#'
#' @param ... vectors to recycle
#' @return A list with new vectors.
#' @keywords internal
.recycle <- function(...){
  lst <- list(...)

  # Longest of objects
  maxdim <- max(lengths(lst))
  # recycle all arguments to maxdim
  res <- lapply(lst, rep, length.out = maxdim)

  attr(res, "maxdim") <- maxdim

  return(res)
}



#' .is.infinite
#' Helper function that does not throw an error if `is.infinite` cannot be
#' applied to an object. It rather just returns `FALSE`.
#' @param x An object
#' @return TRUE/FALSE
#' @keywords internal
.is.infinite <- function(x) {
  if ((is.numeric(x) || is.logical(x) || is.complex(x)) && length(x) > 0)
    is.infinite(x)
  else
    FALSE
}


#' .isAlive
#'
#' Helper function to verify that an object is actionable in some way.
#' Value like `NA`, `NULL`, `Inf` ... are considered dead ends.
#'
#' @param x An object
#' @details Here the list of conditions that will qualify `X` as dead end:
#'
#' * `NULL`
#' * `NA`
#' * `Inf`
#' * `NaN`
#' * Empty vectors
#' * Character vectors containing only `NA` and empty strings
#' * An object of class "`try-error"`
#'
#' `.isAlive` not only checks the object itself. Alive is also checked inside of vectors
#' and lists. Recursive checks
#' @return TRUE/FALSE
#' @export
#' @examples
#' .isAlive(TRUE) # TRUE
#' .isAlive(integer()) # FALSE
.isAlive <- function (x) {
  if (missing(x)) # has to be first
    return(FALSE)
  if (is.null(x))
    return(FALSE)
  if (length(x) == 0L)
    return(FALSE)
  if (inherits(x, "try-error"))
    return(FALSE)

  if (is.atomic(x)) {
    return(!all(is.na(x) | !nzchar(x))) # | is.infinite(x)
  } else if (is.list(x)) {
    dead <- is.na(x) | !nzchar(x) | sapply(x, \(y) length(y) == 0) #| sapply(x, .is.infinite)
    return(!all(dead))
  }

  return(TRUE)
}
