dataset <- list(
  # source: https://de.wikipedia.org/w/index.php?title=Konfidenzintervall&oldid=222627063
  set1 = c(110, 112, 106, 90, 96, 118, 108, 114, 107, 90, 85, 84, 113, 105, 90, 104),
  # source: https://www.zippia.com/advice/how-to-calculate-confidence-interval-with-examples/
  set2 = c(40, 42, 49, 57, 61, 47, 66, 78, 90, 86, 81, 80)
)

# CI CLASS ========================
test_that("ci_new() creates a valid object", {
  # Act
  result <- ci_new(1:2, 0:1, 2:3)

  # Assert
  expect_s3_class(result, c("U.T.lity", "ci", "data.frame"))
  expect_identical(
    result,
    structure(
      structure(list(est=1:2, lower=0:1, upper=2:3),
                class="data.frame",
                row.names=1:2),
      class=c("U.T.lity", "ci", "data.frame")
    ))
})

test_that("ci_new() creates a valid object", {
  # Act
  result <- ci_new(1:2, 0:1, 2:3,
                   .alt="two-sided", .dstr="joke", .mthd="guessing", .call="none()")

  # Assert
  expect_identical(attr(result, "alternative"), "two-sided")
  expect_identical(attr(result, "distr"), "joke")
  expect_identical(attr(result, "method"), "guessing")
  expect_identical(attr(result, "call"), "none()")
})


# CONFIDENCE NUMERIC ==============

test_that("ci.numeric works with specific example, conf.level=.95", {
  dt <- dataset$set1

  # Act
  result <- ci.numeric(dt, 105, plot=FALSE)

  # Assert
  expect_equal(result$est[1], 102, tolerance = 0.005)
  expect_equal(result$lower[1], 96.07, tolerance = 0.005)
  expect_equal(result$upper[1], 107.93, tolerance = 0.005)
  #expect_equal(result$width[1], 107.93-96.07, tolerance = 0.005)
  expect_true(result$inside[1])

  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 1L)
  expect_identical(ncol(result), 4L)
})



test_that("ci.numeric works with specific example, conf.level != .95", {
  dt <- dataset$set2

  # Act
  result <- ci.numeric(dt, 105, conf.level = 0.98, plot=FALSE)

  # Assert
  expect_equal(result$est[1],  64.75, tolerance = 0.005)
  expect_equal(result$lower[1], 50.69, tolerance = 0.005)
  expect_equal(result$upper[1], 78.81, tolerance = 0.005)
  #expect_equal(result$width[1], 78.81-50.69, tolerance = 0.005)
  expect_false(result$inside[1])

  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 1L)
  expect_identical(ncol(result), 4L)
})



test_that("ci.numeric works with two vectors and varying 'mu'", {
  # Act
  result <- ci.numeric(dataset, 105, conf.level = 0.95, plot=FALSE)
  # Assert
  expect_true(result$inside[1])
  expect_false(result$inside[2])

  # Act
  result <- ci.numeric(dataset, c(110, 60), conf.level = 0.95, plot=FALSE)
  # Assert
  expect_false(result$inside[1])
  expect_true(result$inside[2])
})




