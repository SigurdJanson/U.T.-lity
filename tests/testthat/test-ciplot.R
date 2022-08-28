# PLOT CONFIDENCE ===================

test_that("Base CI plot works", {
  lwr <- seq(0, 10, 10/3)
  upr <- (seq(0, 10, 10/3)+1)*2

  # Assert
  vdiffr::expect_doppelganger("ciplot", ciplot_default(lwr, upr, names=LETTERS[1:4]))
})
