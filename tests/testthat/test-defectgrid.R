# tests for 'defectgrid.R'

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



#
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



test_that("`is.defectgrid()` fails if either `defectgrid` or `U.T.lity` is removed", {
  # Act
  result <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  # ... remove class "ci"
  class(result) <- class(result)[class(result) != "defectgrid"]
  # Assert
  expect_false(is.ci(result))

  # Act
  result <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  # ... remove class "U.T.lity"
  class(result) <- class(result)[class(result) != "U.T.lity"]
  # Assert
  expect_false(is.ci(result))
})


test_that("matrix is not denied by 'is.defectgrid()'", {
  x <- matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4)
  expect_false(is.defectgrid(x))
})


