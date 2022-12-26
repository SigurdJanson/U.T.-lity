
# Constructor of Sample Size Object =======
test_that("contructor returns correct object with class and structure", {
  result <- new_samplesim(c(0.9, 0.925, 0.95, 0.975, 0.99),
                          conf.level = 0.95,
                          desired.events = 0.801,
                          srange = c(1L, 999L),
                          .call = "TESTRUN")
  expect_type(result, "integer")
  expect_s3_class(result, "samplesize")
  expect_s3_class(result, "simulation")
  expect_identical(as.vector(result), 3L)
  expect_identical(attr(result, "result"), c(0.9, 0.925, 0.95, 0.975, 0.99))
  expect_identical(attr(result, "conf.level"), 0.95)
  expect_identical(attr(result, "desired"), 0.801)
  expect_true(attr(result, "converged"))
  expect_identical(attr(result, "search.range"), c(1L, 999L))
  expect_identical(attr(result, "call"), "TESTRUN")
})

test_that("mismatch in range throws an error", {
  input <- c(0.9, 0.925)
  expect_error(
    new_samplesim(input, conf.level = 0.95,
                  desired.events = 0.8,
                  srange = c(1, length(input)-1L),
                  .call = "TESTRUN")
  )
})

test_that("'srange' is correctly used to determine the result", {
  input <- c(0.9, 0.925, 0.95, 0.975, 0.99)

  # Position = 3 + 0
  result <- new_samplesim(input, conf.level = 0.95, desired.events = 0.801,
                          srange = c(1L, 5L), .call = "TESTRUN")
  expect_identical(as.vector(result), 3L)
  expect_true(attr(result, "converged"))

  # Position = 3 + 6
  result <- new_samplesim(input, conf.level = 0.95, desired.events = 0.801,
                          srange = c(1L, 5L) + 6L, .call = "TESTRUN")
  expect_identical(as.vector(result), 9L)
  expect_true(attr(result, "converged"))


  input <- c(0.95, 0.96, 0.97, 0.98, 0.99)
  # Position = 1 + 0
  result <- new_samplesim(input, conf.level = 0.95, desired.events = 0.801,
                          srange = c(1L, 5L), .call = "TESTRUN")
  expect_identical(as.vector(result), 1L)
  expect_true(attr(result, "converged"))

  # Position = 1 + 5
  result <- new_samplesim(input, conf.level = 0.95, desired.events = 0.801,
                          srange = c(1L, 5L)+5L, .call = "TESTRUN")
  expect_identical(as.vector(result), 6L)
  expect_true(attr(result, "converged"))

  input <- c(0.90, 0.90, 0.90, 0.90, 0.95)
  # Position = 9
  result <- new_samplesim(input, conf.level = 0.95, desired.events = 0.801,
                          srange = c(5L, 99L), .call = "TESTRUN")
  expect_identical(as.vector(result), 9L)
  expect_true(attr(result, "converged"))
})

test_that("non-convergion is identified", {
  input <- c(0.95, 0.96, 0.97, 0.98, 0.99) - 0.1

  result <- new_samplesim(input, conf.level = 0.95, desired.events = 0.801,
                          srange = c(1L, 5L), .call = "TESTRUN")
  expect_identical(as.vector(result), Inf)
  expect_false(attr(result, "converged"))
})




# Compute Sample Size =======

test_that("sample works", {
  # Taken from Sauro & Lewis (), p. 145 and table 7.1
  expect_equal(nSample_binom(0.15, 0.8), 9.9, tolerance=0.1)

  # See table 7.1
  SauroNumbers <- matrix(c(69L, 189L, 459L,
                           5L,  12L,  29L,
                           1L,   1L,   2L),
                         nrow=3L, byrow=TRUE,
                         dimnames=list(c(0.01, 0.15, 0.9), c(0.5, 0.85, 0.99)))

  for(.r in rownames(SauroNumbers))
    for (.c in colnames(SauroNumbers)) {
      result <- nSample_binom(as.numeric(.r), as.numeric(.c))
      expect_equal(result, SauroNumbers[.r, .c])
    }
})


test_that("exact gives smaller(or equal at max) results", {
  nTests <- 10L
  occ <- runif(nTests)
  obs <- runif(nTests)

  for(i in seq_along(occ)) {
    result <- nSample_binom(occ[i], obs[i])
    rexact <- nSample_binom(occ[i], obs[i], exact=TRUE)
    expect_gte(result, rexact)
  }
})
