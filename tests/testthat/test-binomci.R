


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

  758
  # stattools.crab.org/Calculators/binomialConfidence.htm
  result <- binomci(758, 10003, conf.level=0.80)
  expect_equal(result$lower[1], 0.0708, tolerance=0.05)
  expect_equal(result$upper[1], 0.0811, tolerance=0.05)
})




test_that("one-sided intervals are correct", {
  #
  sle <- binom.test(682, 682+243, p = 0.75, alternative="less", conf.level=0.75)
  less <- c(sle$conf.int, sle$conf.int[2])

  # Act
  result <- binomci(682, 682+243, conf.level=0.75, alternative="less")

  # Assert
  expect_equal(c(result$lower[1], result$upper[1]), less)


  #
  sgr <- binom.test(682, 682+243, p = 0.75, alternative="greater", conf.level=0.75)
  greater <- c(sgr$conf.int[1], sgr$conf.int[2])

  # Act
  result <- binomci(682, 682+243, conf.level=0.75, alternative="greater")

  # Assert
  expect_equal(c(result$lower[1], result$upper[1]), greater)
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



