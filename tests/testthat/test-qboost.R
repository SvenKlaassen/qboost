test_that("qboost works for simple example", {
  n <- 100;
  p <- 500; s <- 5
  X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
  Y <- 2*X[,1] + rnorm(n,0,1)
  model <- qboost(X,
                  Y,
                  tau = .5,
                  m_stop = 10,
                  stepsize = NULL,
                  h = 0.1,
                  kernel = "Gaussian")
  expect_length(model$selection_path, 10)
  expect_equal(dim(model$coeff_path),c(p+1,11))
})
