test_cases <- expand.grid(
  h = c(0.05,0.1,0.5),
  tau = c(0.25,0.5,0.75),
  kernel =  c("Gaussian","uniform","parabolic","triangular"),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for the smoothed check loss",
  .cases = test_cases,{
    x <- seq(-2,2,0.1)
    loss <- smooth_check_loss(x,tau = tau,h = h, kernel = kernel)
    expect_match(class(loss), "numeric")
    expect_identical(length(loss), 1L)
  }
)


test_that("non-smoothed check loss works", {
  x <- c(4,-2)
  loss <- smooth_check_loss(x,tau = 0.5, kernel = NULL)
  expect_equal(loss, 1.5)
})


test_cases <- expand.grid(
  h = c(0.05,0.1,0.5),
  tau = c(0.25,0.5,0.75),
  kernel =  c("Gaussian","uniform","parabolic","triangular"),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for the gradient of the smoothed loss",
  .cases = test_cases,{
    x <- seq(-2,2,0.1)
    grad <- grad_smooth_check_loss(x,tau = tau,h = h, kernel = kernel)
    expect_match(class(grad), "numeric")
    expect_identical(length(grad), length(x))
  }
)


test_that("non-smoothed gradient check loss works", {
  x <- c(4,-2)
  grad <- grad_smooth_check_loss(x,tau = 0.5, kernel = NULL)
  expect_equal(grad, c(0.5,-0.5))
})



