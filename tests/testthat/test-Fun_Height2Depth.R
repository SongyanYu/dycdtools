
test_that('Height2Depth works', {
  height <- c(0, 1, 2, 3, 4, 5, 6, 7)
  expect_equal(hgt_to_dpt(height), c(7, 6, 5, 4, 3, 2, 1, 0))
})
