test_that("NULL is dead", {
  expect_false(.isAlive(NULL))
})

test_that("missing arguments are dead", {
  test <- function(x, ...) {
    if (missing(x)) .isAlive(x) else TRUE
  }
  expect_false(test())
})

test_that("NAs are dead", {
  expect_false(.isAlive(NA))
  expect_false(.isAlive(NA_integer_))
  expect_false(.isAlive(NA_real_))
  expect_false(.isAlive(NA_complex_))
  expect_false(.isAlive(NA_character_))

  expect_false(.isAlive(c(NA, NA)))

  expect_false(.isAlive(list(NA_character_, NA_integer_, NA)))
})



# LENGTH = 0 ===========

test_that("empty vectors are dead", {
  expect_false(.isAlive(character()))
  expect_false(.isAlive(logical(0L)))
  expect_false(.isAlive(numeric()))
  expect_false(.isAlive(integer()))
  expect_false(.isAlive(complex(0)))
  expect_false(.isAlive(raw(0)))
  expect_false(.isAlive(matrix()))
})

test_that("empty lists are dead", {
  expect_false(.isAlive(list()))
})

test_that("lists of empty lists are dead", {
  expect_false(.isAlive(list(integer(), character())))
})



# STRINGS ===========

test_that("character vectors with empty strings are dead", {
  expect_false(.isAlive(c("", "")))
  expect_false(.isAlive(c("", NA, "")))
})

test_that("empty lists are dead", {
  expect_false(.isAlive(list("", "")))
  expect_false(.isAlive(list("", NA, "")))
})



# ERRORS =======

test_that("exceptions are diagnosed as dead", {
  expect_false(.isAlive(try(stop("boom"), silent = TRUE)))
})



# ALIVE ==============


# These are all truthy
test_that("alive is alive", {
  expect_true(.isAlive(0))
  expect_true(.isAlive(1:10))
  expect_true(.isAlive(LETTERS))
  expect_true(.isAlive("NA"))
  expect_true(.isAlive(TRUE))
  expect_true(.isAlive(c(NA, NA, TRUE)))
  expect_true(.isAlive(c(FALSE, TRUE)))
  expect_true(.isAlive(c(FALSE)))
})

test_that("non-vectors are alive", {
  expect_true(.isAlive(expression(a+b)))
})
