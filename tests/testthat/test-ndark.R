
#
test_that(".ndark works", {
  n <- 7L
  result <- .ndark(n, 0.927)
  expect_equal(result, 7.6 - n, tolerance = 0.1)

  n <- 6L
  result <- .ndark(n, 0.33)
  expect_equal(result, 19 - n, tolerance = 0.1)

  n <- 33L
  result <- .ndark(n, 0.68)
  expect_equal(result, 49 - n, tolerance = 0.1)
})




# source: Sauro & Lewis "Quantifying the UX", p. table 7.4 and page 155
test_that("example in QTUX works", {
  dg <- defectgrid_new(matrix(c(1,1,1,1, 1,1,1,1, 0,0,0,0, 1,1,1,1, 0,0,1,0,
                                1,1,1,0, 1,0,0,1, 1,1,0,0, 0,0,0,0, 0,0,0,0), 4, 10))
  if (any(marginSums(dg, 2L) != c(4,4,0,4,1,3,2,2,0,0)))
    fail("Input for test is wrong (dg)")
  ndef <- ndefects(dg)
  if (ndef != 7L)
    fail("Input for test is wrong (ndef)")

  result <- ndark(dg, "both")
  expected <- 0.52259722271383157732 # 7.6 - ndef # value from the book cannot be used because of rounding errors
  expect_equal(result, expected)
})


test_that("case study example 1 in QTUX (p. 156) works", {
  dg <- defectgrid_new(matrix(c(0,1,1, 0,1,0, 0,1,0,
                                0,0,1, 0,0,1, 0,0,1), 3L, 6L, byrow = FALSE))

  expected <- 12.23093 # The 13 in the book does not work because of rounding errors
  result <- ndark(dg, "both")
  expect_equal(result, expected, tolerance=1E-6)
})

