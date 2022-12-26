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
                   .alt="two.sided", .dstr="joke", .mthd="guessing", .call="none()")

  # Assert
  expect_identical(attr(result, "alternative"), "two.sided")
  expect_identical(attr(result, "distr"), "joke")
  expect_identical(attr(result, "method"), "guessing")
  expect_identical(attr(result, "call"), "none()")
})


# CONFIDENCE NUMERIC ==============

test_that("ci.numeric works with specific example, conf.level=.95", {
  dt <- dataset$set1

  # Act
  result <- ci.numeric(dt, 105)

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
  result <- ci.numeric(dt, 105, conf.level = 0.98)

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
  result <- ci.numeric(dataset, 105, conf.level = 0.95)
  # Assert
  expect_true(result$inside[1])
  expect_false(result$inside[2])

  # Act
  result <- ci.numeric(dataset, c(110, 60), conf.level = 0.95)
  # Assert
  expect_false(result$inside[1])
  expect_true(result$inside[2])
})




# is.ci( ) ================

test_that("`is.ci()` works on `new_ci()` objects", {
  # Act
  result <- ci_new(1:2, 0:1, 2:3)

  # Assert
  expect_s3_class(result, c("U.T.lity", "ci", "data.frame"))
  expect_true(is.ci(result))
})

test_that("`is.ci()` works on `new_ci()` objects", {
  # Act
  result <- structure(
    structure(list(est=1:2, lower=0:1, upper=2:3),
              class="data.frame",
              row.names=1:2),
    class=c("U.T.lity", "ci", "data.frame")
  )

  # Assert
  expect_true(is.ci(result))
})


test_that("`is.ci()` fails if either `ci` or `U.T.lity` is removed", {
  # Act
  result <- ci_new(1:2, 0:1, 2:3)
  # ... remove class "ci"
  class(result) <- class(result)[class(result) != "ci"]
  # Assert
  expect_false(is.ci(result))

  # Act
  result <- ci_new(1:2, 0:1, 2:3)
  # ... remove class "U.T.lity"
  class(result) <- class(result)[class(result) != "U.T.lity"]
  # Assert
  expect_false(is.ci(result))
})



# print.ci() ==================

test_that("function call is the first element", {
  x <- ci_new(1:2, 0:1, 2:3,
              .alt="two.sided", .dstr="joke", .mthd="guessing", .call="bigfatfish()", .lvl=0.95)
  expect_output(print(x), r"{^Call: bigfatfish\()}")
})


test_that("headers fit the alternative hypothesis", {
  # conf.level = 0.95 -> [0.25, 0.975]
  x <- ci_new(c(0.01, 0.02), c(1.01, 1.02), c(3.01, 3.02),
              .dstr="joke", .mthd="guessing", .call="bigfatfish()", .lvl=0.95)

  attr(x, "alternative") <- .alternative["two.sided"]
  expect_output(print(x, dropInf=TRUE),  r"{\s+2\.5%\s+est\s+97\.5%}")
  expect_output(print(x, dropInf=FALSE), r"{\s+2\.5%\s+est\s+97\.5%}")

  attr(x, "alternative") <- .alternative["less"]
  expect_output(print(x, dropInf=TRUE),  r"{\s+est\s+95%}")
  expect_output(print(x, dropInf=FALSE), r"{\s+0%\s+est\s+95%}")

  attr(x, "alternative") <- .alternative["greater"]
  expect_output(print(x, dropInf=TRUE),  r"{\s+95%\s+est\s*}")
  expect_output(print(x, dropInf=FALSE), r"{\s+95%\s+est\s+100%}")

  # variation: conf.level = 0.94 -> [0.3, 0.97]
  x <- ci_new(c(0.01, 0.02), c(1.01, 1.02), c(3.01, 3.02),
              .dstr="joke", .mthd="guessing", .call="bigfatfish()", .lvl=0.94)

  attr(x, "alternative") <- .alternative["two.sided"]
  expect_output(print(x, dropInf=TRUE),  r"{\s+3%\s+est\s+97%}")
  expect_output(print(x, dropInf=FALSE), r"{\s+3%\s+est\s+97%}")

  attr(x, "alternative") <- .alternative["less"]
  expect_output(print(x, dropInf=TRUE),  r"{\s+est\s+94%}")
  expect_output(print(x, dropInf=FALSE), r"{\s+0%\s+est\s+94%}")

  attr(x, "alternative") <- .alternative["greater"]
  expect_output(print(x, dropInf=TRUE),  r"{\s+94%\s+est\s*}")
  expect_output(print(x, dropInf=FALSE), r"{\s+94%\s+est\s+100%}")

})


# run `snapshot_accept("default arguments are used")`
# run `snapshot_review()` to inspect the differences
test_that("default arguments are used", {
  x <- ci_new(c(0.01, 0.02), c(1.01, 1.02), c(3.01, 3.02),
              .alt="two.sided", .dstr="joke", .mthd="guessing", .call="bigfatfish()", .lvl=0.95)

  result.defaults <- capture.output(print(x))
  result.explicit <- capture.output(print(x, dropInf=TRUE))
  expect_identical(result.defaults, result.explicit)
})

