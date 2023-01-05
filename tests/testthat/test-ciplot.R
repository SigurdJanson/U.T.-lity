
# INHERITANCE =======================
test_that("ggplot is generated and is printable", {
  ci_input <- t.test(1:10, y = c(7:20))
  result <- ciplot(ci_input, lib = "ggplot")
  #--expect_s3_class(result, c("gg", "ggplot"))
  expect_true(ggplot2::is.ggplot(result))
  #expect_error(print(result), NA)
})


# PLOT CONFIDENCE ===================

test_that("Base CI plot works for lib=graphics", {
  lwr <- seq(0, 10, 10/3)
  upr <- (lwr + 1) * 2

  # Assert
  vdiffr::expect_doppelganger("ciplot_gr",
                              ciplot_default(lwr, upr, names=LETTERS[1:4], lib="graphics"))
})


test_that("Base CI plot works for lib=ggplot", {
  lwr <- seq(0, 10, 10/3)
  upr <- (lwr + 1) * 2

  # Assert
  vdiffr::expect_doppelganger("ciplot_gg",
                              ciplot_default(lwr, upr, names=LETTERS[1:4], lib="ggplot"))
})

