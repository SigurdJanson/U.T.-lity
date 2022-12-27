
test_that("vector with no infinite values return `rep(FALSE, length)`", {
  expect_identical(.is.infinite(c(1L, 2L, 5L)), rep(FALSE, 3L))
  expect_identical(.is.infinite(c(1.0, 2.1, -5.4)), rep(FALSE, 3L))
  expect_identical(
    .is.infinite(complex(real = c(1.0, 2.1, -5.4), imaginary = c(1.0, 2.1, -5.4))),
    rep(FALSE, 3L))
})


test_that("empty vector returns FALSE", {
  expect_false(.is.infinite(integer()))
  expect_false(.is.infinite(double()))
  expect_false(.is.infinite(complex()))
})


test_that("character vectors return FALSE", {
  expect_false(.is.infinite(character()))
  expect_false(.is.infinite(LETTERS))
})



test_that("infinite elements return TRUE", {
  expect_identical(
    .is.infinite(c(1L, Inf, 5L, -Inf)),
    c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(
    .is.infinite(c(Inf, 1.2, 2.3, -Inf)),
    c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(
    .is.infinite(complex(real = c(Inf, -2, 1.0, -5.4), imaginary = c(1.0, 2.1, -8, -Inf))),
    c(TRUE, FALSE, FALSE, TRUE))
})
