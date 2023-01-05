test_that("ggplot is generated and is printable", {
  simulation <- nSample_sim_binom()
  result <- simplot(simulation)

  expect_true(ggplot2::is.ggplot(result))
  expect_error(print(result), NA)
})


