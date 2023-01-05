test_that("ggplot is generated and is printable", {
  result <- nDefectsPlot(lib = "ggplot")

  expect_true(ggplot2::is.ggplot(result))
  expect_error(print(result), NA)
})
