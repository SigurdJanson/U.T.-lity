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

