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
