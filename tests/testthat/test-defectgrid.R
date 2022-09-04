# defectgrid_new =============

test_that("'defectgrid_new()' creates an object that is still recognised as matrix", {
  x <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  expect_s3_class(x, "matrix")
})

test_that("'defectgrid_new()' coerces vectors a matrix", {
  row <- 3L; col <- 4L
  x <- defectgrid_new(c(1,0,1,0, 0,1,0,1, 0,0,1,1))
  expect_s3_class(x, "matrix")
  expect_identical(nrow(x), 1L)
  expect_identical(ncol(x), row * col)
})

test_that("'defectgrid_new()' sets correct type attributes", {
  testobj <- matrix(as.integer(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  x <- defectgrid_new(testobj)
  expect_identical(attr(x, "type"), "exists")

  x <- defectgrid_new(matrix(c(1,0,2,0, 0,1,0,1, 0,0,1,1), 3, 4))
  expect_identical(attr(x, "type"), "count")

  testobj <- matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4)
  x <- defectgrid_new(as.logical(testobj))
  expect_identical(attr(x, "type"), "exists")
})

# is.defectgrid ==============
test_that("'defectgrid_new()' creates an object detected by 'is.defectgrid()'", {
  x <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  expect_true(is.defectgrid(x))
})

test_that("'defectgrid_new()' creates an object detected by 'is.defectgrid()'", {
  x <- matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4)
  class(x) <- c("U.T.lity", "defectgrid")
  expect_true(is.defectgrid(x))
})



test_that("matrix is not denied by 'is.defectgrid()'", {
  x <- matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4)
  expect_false(is.defectgrid(x))
})


# pxpplot_default ===========
test_that("4 regular columns work", {

  # Act
  dg <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  result <- pxpplot_default(dg, percentage=TRUE )

  # Assert
  expect_identical(
    result,
    matrix(c(21,1,21, 21,21,1, 12,1,1, 1,12,1),
           nrow=3L, dimnames = list(NULL, c(1, 4, 2, 3)))
  )
})


test_that("...adding 3 columns of darkfigures works", {

  # Act
  dg <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  result <- pxpplot_default( dg, darkfigure=3, percentage=TRUE )

  # Assert
  expect_identical(
    result,
    matrix(c(21,1,21, 21,21,1, 11,1,1, 1,11,1, 1,1,1, 1,1,1, 1,1,1),
           nrow=3L, dimnames = list(NULL, c(1, 4, 2, 3, 5, 6, 7)))
  )
})
