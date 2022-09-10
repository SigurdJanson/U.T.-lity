



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
test_that("comparison with original function gives same results", {
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
  result_explicit <- poissonci(0:5, 10, method="score")

  # Assert
  expect_equal(result_default$est, expected[, "est"], tolerance=10^-14)
  expect_equal(result_default$lower, expected[, "lwr.ci"], tolerance=10^-14)
  expect_equal(result_default$upper, expected[, "upr.ci"], tolerance=10^-14)
})



#
test_that("default arguments are used correctly", {

  # Act
  result_default <- poissonci(0:5, 10) # method="score" is default
  result_explicit <- poissonci(0:5, 10, method="score", alternative="two.sided", conf.level = 0.95)

  # Assert
  attr(result_default, "call") <- NULL
  attr(result_explicit, "call") <- NULL
  expect_identical(result_default, result_explicit)
})
