
test_that("function returns 'samplesim' object", {
  expect_s3_class(nSample_sim_binom(), c("samplesize", "simulation"))
})

# ARGUMENTS HANDLING #########################
test_that("Default arguments are applied", {
  # All arguments have defaults
  expect_error(nSample_sim_binom(), NA)

  # Defaults are in the result
  resultD <- nSample_sim_binom()
  expect_identical(attr(resultD, "conf.level"), 0.95)
  expect_identical(attr(resultD, "desired"), 0.8)
  expect_true(attr(resultD, "converged"))
  expect_identical(attr(resultD, "search.range"), c(3L, 99L))
})



