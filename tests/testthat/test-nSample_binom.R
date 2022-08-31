

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
