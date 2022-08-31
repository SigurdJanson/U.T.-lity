


#
# Numbers by Sauro & Lewis, quant. the UX
test_that("", {
  expect_equal(ndefects(0.5, 1, 5), 0.969, tolerance = 0.001)
})


#
# Numbers by Sauro & Lewis, quant. the UX
# This test is rough because the sample sizes are rounded up to the next integer.
# Because of that, for each given value P must be `P >= ndefects(...)`.
#
test_that("sample cases from Jeff Sauro yield roughly the same result", {
  # See table 7.1
  SauroNumbers <- matrix(c(69L, 189L, 459L,
                     5L,  12L,  29L,
                     1L,   1L,   2L),
                  nrow=3L, byrow=TRUE,
                  dimnames=list(c(0.01, 0.15, 0.9), c(0.5, 0.85, 0.99)))

  for(.r in rownames(SauroNumbers))
    for (.c in colnames(SauroNumbers)) {
      result <- ndefects(as.numeric(.r), 1, SauroNumbers[.r, .c])
      expect_gte(result, as.numeric(.c)) #, tolerance=0.01, info=paste("p =", .r, "n =", SauroNumbers[.r, .c]))

      lowerresult <- ndefects(as.numeric(.r), 1, SauroNumbers[.r, .c]-1L)
      expect_lt(lowerresult, as.numeric(.c))
    }
})


#
# Numbers by Sauro & Lewis, quant. the UX
#
test_that("sample cases from Jeff Sauro yield the same result", {
  # See table 7.2
  SauroNumbers <- matrix(c(0.01, 0.05, 0.1, 0.18,
                           0.15, 0.56, 0.8, 0.96,
                           0.9 , 1, 1, 1),
                         nrow=3L, byrow=TRUE,
                         dimnames=list(c(0.01, 0.15, 0.9), c(1, 5, 10, 20)))

  for(.r in rownames(SauroNumbers))
    for (.c in colnames(SauroNumbers)) {
      result <- ndefects(as.numeric(.r), 1, as.numeric(.c))
      result <- round(result, 2) # numbers in the book are rounded to 2 digits
      expect_equal(result, SauroNumbers[.r, .c], info=paste("p =", .r, "n =", as.numeric(.c)))
    }
})



test_that("the percentage (d.total==1) is multiplied by d.total", {
  totals <- c(10, 20, 50, 1000, 5000)

  for (size in runif(5)*10+1) # different sample sizes
    for (p in runif(10L)) { # different p's
      percentage <- ndefects(p.occ=0, d.total = rep(1L, length(totals)), size)
      count <- ndefects(p.occ=0, d.total = totals, size)
      # Assert
      expect_equal(count, percentage*totals, info=paste("p =", p, "n =", size))
    }
})
