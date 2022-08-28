test_that("4 regular columns work", {

  # Act
  result <- defectgrid( matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4), percentage=TRUE )

  # Assert
  expect_identical(
    result,
    matrix(c(21,1,21, 21,21,1, 12,1,1, 1,12,1),
           nrow=3L, dimnames = list(NULL, c(1, 4, 2, 3)))
  )
})


test_that("...adding 3 columns of darkfigures works", {

  # Act
  result <- defectgrid( matrix(c(1,0,1,0, 0,1,0,1, 0,0,1,1), 3, 4), darkfigure=3, percentage=TRUE )

  # Assert
  expect_identical(
    result,
    matrix(c(21,1,21, 21,21,1, 11,1,1, 1,11,1, 1,1,1, 1,1,1, 1,1,1),
           nrow=3L, dimnames = list(NULL, c(1, 4, 2, 3, 5, 6, 7)))
  )
})
