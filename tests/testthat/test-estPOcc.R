

# P Occurrence =====================

test_that("", {
  # Sauro & Lewis, table 7.5
  dg <- matrix(c(1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  0, 0, 1, 0,
                 1, 1, 1, 0,  1, 0, 0, 1,  1, 1, 0, 0),
               nrow = 4)
  result <- estPOcc(dg, adj = "none")
  expect_equal(result, 5/7)

  result <- estPOcc(dg, adj = "defl")
  expect_equal(result, 0.345, tolerance=0.01)

  result <- estPOcc(dg, adj = "GT")
  expect_equal(result, 0.621, tolerance=0.01)

  result <- estPOcc(dg, adj = "both")
  expect_equal(result, 0.483, tolerance=0.01)
})


# ADJUSTMENT =======================



# Sauro & Lewis, pp. 152
test_that("sample for adjustPOcc works", {
  result <- adjustPOcc(0.71, 4, adj="defl") # 0.7142857
  expect_equal(result, 0.345)

  result <- adjustPOcc(0.71, Ntotal=7, N1=1, adj="GT") # 0.7142857
  expect_equal(result, 0.621, tolerance=0.001)


  # result == 0.48 was re-calculcated to reduce the rounding error in the book
  # The more correct number is 0.483
  result <- adjustPOcc(0.71, 4, Ntotal=7, N1=1, adj="both") # 0.7142857
  expect_equal(result, 0.483, tolerance=0.001)
})
