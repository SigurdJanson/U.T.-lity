


test_that("sample cases work", {
  # www.statology.org/binomial-confidence-interval-python/
  # python: proportion_confint(count=56, nobs=100)
  result <- binomci(56, 100)
  expect_equal(result$lower[1], 0.4622810465167698)
  expect_equal(result$upper[1], 0.6532797336983921)

  # python: proportion_confint(count=56, nobs=100, alpha=0.10)
  result <- binomci(56, 100, conf.level=0.9)
  expect_equal(result$lower[1], 0.47783814499647415)
  expect_equal(result$upper[1], 0.6390007285095451)

  # reliabilityanalyticstoolkit.appspot.com/binomial_confidence_solution
  result <- binomci(4, 20, conf.level=0.99)
  expect_equal(result$lower[1], 0.03575611114501953125, tolerance=0.0375)
  expect_equal(result$upper[1], 0.50660800933837890625, tolerance=0.0375)

  # stattools.crab.org/Calculators/binomialConfidence.htm
  result <- binomci(758, 10003, conf.level=0.80)
  expect_equal(result$lower[1], 0.0725, tolerance=0.001)
  expect_equal(result$upper[1], 0.0792, tolerance=0.001)
})



# use samples taken from the `MKinfer` library
# using https://rdrr.io/snippets/
# This test could be improved - it is not very precise
test_that("one-sided intervals are correct for confidence=95%", {
  expected <- data.frame(
    conf.level  = c(rep(0.95, 6L)),
    x           = rep(c(42, 16, 23), 2L),
    n           = rep(c(43, 112, 256), 2L),
    alternative = factor(c(rep("less", 3L), rep("greater", 3L))),
    lower       = c(rep(0.00, 3L),
                    c(0.90225195413828107593, 0.096882964853565414765, 0.064576958381973997847)),
    upper       = c(c(0.99479459430017136423, 0.20567907712816674293, 0.1236893725354294149),
                    rep(1.00, 3L))
  )

  for (testrow in 1:6) {
    .cl  <- expected$conf.level[testrow]
    .alt <- expected$alternative[testrow]
    .x   <- expected$x[testrow]
    .n   <- expected$n[testrow]
    .exp <- c(expected$lower[testrow], expected$upper[testrow])
    # Act
    result <- binomci(x = .x, n = .n, conf.level = .cl, alternative=as.character(.alt))
    # Assert
    expect_equal(c(result$lower[1], result$upper[1]), .exp, tolerance=1E-7, label=paste(testrow, " - ", .alt))
    expect_equal(attr(result, "alternative"), as.character(.alt))
  }
})



## ARGUMENT HANDLING ###########

test_that("confidence width grows with growing 'conf.level'", {
  .levels <- c(0.60, 0.70, 0.80, 0.90, 0.99)

  # Act
  result <- data.frame(est=numeric(), lower=numeric(), upper=numeric())
  for (i in .levels)
    result <- rbind(result, binomci(5, 10, conf.level=i))
  result$width <- result$upper - result$lower

  # Assert
  for (i in 2:length(.levels))
    expect_gt(result$width[i], result$width[i-1])
})



test_that("different methods give different results", {
  x <- 3
  n <- 8

  # print(MKinfer::binomCI(3, 8, method="wilson")$conf.int, digits=20)
  expect_equal(binomci(x, n, method="wilson")$lower, 0.13684428582359736692)
  expect_equal(binomci(x, n, method="wilson")$upper, 0.69425760539737257915)

  # print(MKinfer::binomCI(3, 8, method="clopper")$conf.int, digits=20)
  expect_equal(binomci(x, n, method="exact")$lower, 0.085233414137253563081)
  expect_equal(binomci(x, n, method="exact")$upper, 0.75513678363344838296)

  # print(MKinfer::binomCI(3, 8, method="logit")$conf.int, digits=20)
  expect_equal(binomci(x, n, method="logit")$lower, 0.12540845310773368615)
  expect_equal(binomci(x, n, method="logit")$upper, 0.71515002164082153158)
})



test_that("multiple methods are not accepted", {
  # No error
  expect_error(binomci(5, 10, conf.level=0.9, method=c("asymptotic")),
               NA)
  # Error
  expect_error(binomci(5, 10, conf.level=0.9, method=c("asymptotic", "logit")),
               "binomci allows only a single method at a time")
})


test_that("changing alternatives changes the result", {
  result.twosided <- binomci(4, 12)
  result.less <- binomci(4, 12, alternative="less")
  expect_identical(result.less$lower, 0)
  expect_lt(result.less$upper, result.twosided$upper)


  result.twosided <- binomci(4, 12)
  result.greater <- binomci(4, 12, alternative="greater")
  expect_identical(result.greater$upper, 1)
  expect_gt(result.greater$lower, result.twosided$lower)
})
