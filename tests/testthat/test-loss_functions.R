test_that("smoothed check loss works", {
  x <- c(4,-2)
  loss <- smooth_check_loss(x,tau = 0.5,h = 0.1, kernel = NULL)
  expect_equal(loss, 1.5)
})
