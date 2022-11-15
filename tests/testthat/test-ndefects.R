test_that("ndefects() ignores columns with zeroes only", {
  dg <- defectgrid_new(matrix(c(1,1,1,1, 1,1,1,1, 0,0,0,0, 1,1,1,1, 0,0,1,0,
                                1,1,1,0, 1,0,0,1, 1,1,0,0, 0,0,0,0, 0,0,0,0), 4, 10))
  result <- ndefects(dg)
  expect_identical(result, 7L)
})


test_that("ndefects() returns number of columns when all columns contain observations", {
  dg <- defectgrid_new(matrix(c(1,1,1,1, 1,1,1,1, 0,0,0,1, 1,1,1,1, 0,0,1,0,
                                1,1,1,0, 1,0,0,1, 1,1,0,0, 1,0,0,0, 0,1,0,0), 4, 10))
  result <- ndefects(dg)
  expect_identical(result, 10L)

  dg <- defectgrid_new(matrix(c(1,1,1,1, 1,1,1,1, 1,1,1,1, 0,0,1,0,
                                1,1,1,0, 1,0,0,1, 1,1,0,0), 4, 7))
  result <- ndefects(dg)
  expect_identical(result, 7L)

  dg <- defectgrid_new(matrix(c(99,999,9999,0, 1,9191913747239374,1,1, 1,1,1,1, 0,0,1,0,
                                1,1,1,0, 1,0,0,1, 1,1,0,0), 4, 7))
  result <- ndefects(dg)
  expect_identical(result, 7L)
})

