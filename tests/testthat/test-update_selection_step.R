test_cases <- expand.grid(
  h = c(0.05,0.1,0.5),
  tau = c(0.25,0.5,0.75),
  kernel =  c("Gaussian","uniform","parabolic","triangular"),
  stringsAsFactors = FALSE)

test_cases["test_name"] = apply(test_cases, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for the selection step",
  .cases = test_cases,{
    X <- cbind(c(1,2), c(2,-2))
    residuals <- c(4,-4)
    res <- update_selection_step(X,residuals,tau = 0.5,kernel = NULL)
    expect_equal(res$sel_cov,1)
  }
)

test_that("selection step works without smoothing", {
  X <- cbind(c(1,2), c(2,-2))
  residuals <- c(4,-4)
  res <- update_selection_step(X,residuals,tau = 0.5,kernel = NULL)
  expect_equal(res$sel_cov,1)
  expect_equal(res$cor,1)
})

test_that("selection step works with degenerate gradient", {
  X <- cbind(c(1,2), c(2,-2))
  residuals <- c(4,4)
  res <- update_selection_step(X,residuals,tau = 0.5,kernel = NULL)
  expect_identical(res$sel_cov,NA)
  expect_equal(res$cor,-0.5)
})
