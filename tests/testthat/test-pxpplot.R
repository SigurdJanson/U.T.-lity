
Labels <- list(
  x = "Defect", y = "Participant", title = "Matrix Participant 🞪 Defect", fill = "d"
)

#
# pxpplot_gg ===========
test_that("ggplot is generated and is has correct metadata", {
  # Act
  dg <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  result <- pxpplot_gg(dg, percentage=TRUE)

  expect_true(ggplot2::is.ggplot(result))
  expect_identical(nrow(result$data), length(dg))
  expect_equal(result$labels, Labels)

  #-oldopt <- pdf.options(encoding='ISOLatin2.enc')
  #expect_error(print(result), NA) # currently throwing errors
  #-do.call(pdf.options, oldopt)
})



#
# pxpplot_gr ===========

test_that("4 regular columns work", {

  # Act
  dg <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  result <- pxpplot_gr(dg, percentage=TRUE, plot=TRUE)

  # Assert
  expect_identical(
    result,
    matrix(c(21,1,21, 21,21,1, 12,1,1, 1,12,1),
           nrow=3L, dimnames = list(NULL, c(1, 4, 2, 3)))
  )
})



test_that("...adding 3 columns of darkfigures works", {

  # Act
  dg <- defectgrid_new(matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4))
  result <- pxpplot_gr( dg, darkfigure=3, percentage=TRUE )

  # Assert
  expect_identical(
    result,
    matrix(c(21,1,21, 21,21,1, 11,1,1, 1,11,1, 1,1,1, 1,1,1, 1,1,1),
           nrow=3L, dimnames = list(NULL, c(1, 4, 2, 3, 5, 6, 7)))
  )
})
