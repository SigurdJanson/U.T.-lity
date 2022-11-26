

test_that("if correct object is returned", {
  clevel <- 0.949
  result <- ci.numeric(1:10, 2, conf.level=clevel)


  expect_s3_class(result, c("U.T.lity", "ci", "data.frame"))
  # `ci.numeric` provides an extra element `inside`
  expect_named(result, c("est", "lower", "upper", "inside"))
  # Check attribute conf.level and it's value
  expect_identical(attr(result, "conf.level"), clevel)
  # Further attributes
  expect_identical(attr(result, "alternative"), unname(.alternative["two.sided"]))
  expect_identical(attr(result, "distr"), "t distribution")
  expect_match(attr(result, "method"), "[Oo]ne.*[Ss]ample.*t-[Tt]est")
  expect_match(attr(result, "call"), ".*ci\\.numeric.*")
})

# ================
test_that("confidence intervals fit t.test", {
  for (i in 1:3) {
    x <- runif(10*i, 0, 100)
    mu <- runif(1, 0, 100)
    expected <- t.test(x, mu=mu)
    result <- ci.numeric(x, mu)
    expect_identical(result$lower, expected$conf.int[1L])
    expect_identical(result$upper, expected$conf.int[2L])
    expect_identical(result$est, unname(expected$estimate))
  }
})


# mu = default ============
test_that("descr", {
  result <- ci.numeric(-5:5, conf.level=0.95)
  expect_identical(result$inside, TRUE)
  expect_identical(result$est, 0)
})



# test with lists ============

# - two x and 2 mu
test_that("with 2 samples and 2 µ", {
  x <- list( runif(24, 0, 100), runif(32, 0, 100) )
  mu <- runif(2L, 0, 100)

  expected <- list()
  for (i in seq_along(x)) {
    expected <- c(expected, list(t.test(x[[i]], mu=mu[i])))
  }

  # Act
  result <- ci.numeric(x, mu)

  # Assert
  expect_identical(result$lower, sapply(expected, \(y) y$conf.int[1L]))
  expect_identical(result$upper, sapply(expected, \(y) y$conf.int[2L]))
  expect_identical(result$est, sapply(expected, \(y) unname(y$estimate)))
})


# - n x and fewer mu
test_that("3 samples and 2 µ the function replicates the µ", {
  x <- list( runif(24, 0, 100), runif(32, 0, 100), runif(32, 0, 100) )
  mu <- c(50.55, 49)

  expected <- list()
  for (i in seq_along(x)) {
    myndex <- (i %% length(mu)) + 1L
    expected <- c(expected, list(t.test(x[[i]], mu=mu[myndex])))
  }

  # Act
  result <- ci.numeric(x, mu)

  # Assert
  expect_identical(result$lower, sapply(expected, \(y) y$conf.int[1L]))
  expect_identical(result$upper, sapply(expected, \(y) y$conf.int[2L]))
  expect_identical(result$est, sapply(expected, \(y) unname(y$estimate)))
})
