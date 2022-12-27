test_that("equal sized vectors stay the same", {
  x <- y <- sample(1:10, 10)
  expect_identical(.recycle(x, y), structure(list(x, y), maxdim=10L))
})


test_that("replication with 2 vectors disregards vector order", {
  x <- sample(1:10, 5)
  y <- sample(1:10, 10)
  x_rep <- c(x, x)
  expect_identical(
    .recycle(x, y),
    structure(list(x_rep, y), maxdim=10L))
  expect_identical(
    .recycle(y, x),
    structure(list(y, x_rep), maxdim=10L))
})


test_that("replication with 2 vectors, length not a multiple", {
  x <- sample(1:10, 3)
  y <- sample(1:10, 10)
  x_rep <- c(x, x, x, x[1])

  expect_identical(
    .recycle(x, y),
    structure(list(x_rep, y), maxdim=10L))
  expect_identical(
    .recycle(y, x),
    structure(list(y, x_rep), maxdim=10L))
})

test_that("3 vectors of different length", {
  x <- sample(1:10, 4)
  y <- sample(1:10, 10)
  z <- sample(1:10, 12, TRUE)
  x_rep <- c(x, x, x)
  y_rep <- c(y, y[1:2])

  expect_identical(
    .recycle(x, y, z),
    structure(list(x_rep, y_rep, z), maxdim=12L))
})
