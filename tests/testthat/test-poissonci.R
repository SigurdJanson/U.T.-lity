# This is the result from
# library(DescTools)
# PoissonCI(0:5, 10, method="score")
# est     lwr.ci    upr.ci
# x.1 0.0 0.00000000 0.3841459
# x.2 0.1 0.01765246 0.5664934
# x.3 0.2 0.05484721 0.7292987
# x.4 0.3 0.10202707 0.8821188
# x.5 0.4 0.15555219 1.0285937
# x.6 0.5 0.21357011 1.1705758
test_that("comparison with original function gives same results: default 'score' method", {
  expected <- structure(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0,
                          0.0176524554935152, 0.0548472138257576,
                          0.102027072836432, 0.155552188371689, 0.213570113757345,
                          0.384145882069412, 0.566493426575897, 0.729298668243655,
                          0.88211880923298, 1.02859369369772, 1.17057576831207),
                        .Dim = c(6L, 3L),
                        .Dimnames = list(NULL, c("est", "lwr.ci", "upr.ci")))
                        # row names removed: c("x.1", "x.2", "x.3", "x.4", "x.5", "x.6")
  # Act
  result_default <- poissonci(0:5, 10) # method="score" is default

  # Assert
  expect_equal(result_default$est, expected[, "est"], tolerance=10^-14)
  expect_equal(result_default$lower, expected[, "lwr.ci"], tolerance=10^-14)
  expect_equal(result_default$upper, expected[, "upr.ci"], tolerance=10^-14)
})


# This is the result from
# library(DescTools)
# PoissonCI(c(2, 4, 9), 10, method="byar")
# est     lwr.ci    upr.ci
# x.1 0.2 0.03988414 0.6410834
# x.2 0.4 0.13373174 0.9510080
# x.3 0.9 0.44450562 1.6427064
test_that("comparison with original function gives same results: byar", {
  expected <- structure(c(0.2, 0.4, 0.9,
                          0.03988414, 0.13373174, 0.44450562,
                          0.6410834, 0.9510080, 1.6427064),
                        .Dim = c(3L, 3L),
                        .Dimnames = list(NULL, c("est", "lwr.ci", "upr.ci")))
  x <- c(2, 4, 9)
  n <- 10
  # Act
  result <- poissonci(x, n, method="byar")

  # Assert
  expect_equal(result$est, expected[, "est"], tolerance=10^-7)
  expect_equal(result$lower, expected[, "lwr.ci"], tolerance=10^-7)
  expect_equal(result$upper, expected[, "upr.ci"], tolerance=10^-7)
})




test_that("one sided intervals are correct", {
  x <- 0:5
  n <- 10
  conflevel <- 0.95
  conflevel2sided <- 1 - ((1-conflevel)*2)

  # "Less"
  result <- poissonci(x, n, conf.level=conflevel, alternative="less")
  result2sided <- poissonci(x, n, conf.level = conflevel2sided)

  expect_identical(result$lower, result2sided$lower)
  expect_identical(result$upper, rep(Inf, length(x)))

  # "Greater"
  result <- poissonci(x, n, conf.level=conflevel, alternative="greater")
  result2sided <- poissonci(x, n, conf.level = conflevel2sided)

  expect_identical(result$lower, rep(-Inf, length(x)))
  expect_identical(result$upper, result2sided$upper)
})




# ARGUMENT HANDLING #####################

#
test_that("default arguments are used correctly", {
  x <- 0:5
  n <- 10
  # Act
  result_default <- poissonci(x, n) # method="score" is default
  result_explicit <- poissonci(x, n, method="score", alternative="two.sided", conf.level = 0.95)

  # Assert
  attr(result_default, "call") <- NULL
  attr(result_explicit, "call") <- NULL
  expect_identical(result_default, result_explicit)
})


