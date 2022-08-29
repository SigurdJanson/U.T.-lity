

.recycle <- function(...){
  lst <- list(...)

  # Longest of objects
  maxdim <- max(lengths(lst))
  # recycle all arguments to maxdim
  res <- lapply(lst, rep, length.out = maxdim)

  attr(res, "maxdim") <- maxdim

  return(res)
}
