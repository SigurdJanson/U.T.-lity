


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
  expect_equal(result$lower[1], 0.03575611114501953125, tolerance=0.05)
  expect_equal(result$upper[1], 0.50660800933837890625, tolerance=0.05)

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
    expect_equal(c(result$lower[1], result$upper[1]), .exp, tolerance=0.07234375, label=paste(testrow, " - ", .alt))
  }
})




test_that("... confidence width grows with growing 'conf.level'", {
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



