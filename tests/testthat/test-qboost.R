test_cases_WCGA <- expand.grid(
  tau = c(0.25,0.5,0.75),
  kernel =  c("Gaussian","uniform","parabolic","triangular"),
  stringsAsFactors = FALSE)

test_cases_WCGA["test_name"] = apply(test_cases_WCGA, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for WCGA",
  .cases = test_cases_WCGA,
  {
    n <- 100;p <- 500; s <- 5
    steps <- 10
    X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
    Y <- 2*X[,1] + rnorm(n,0,1)
    model <- qboost(X,
                    Y,
                    tau = tau,
                    m_stop = steps,
                    stepsize = NULL,
                    h = 0.1,
                    kernel = kernel)
    testthat::expect_length(model$selection_path, steps)
    testthat::expect_equal(dim(model$coeff_path),c(p+1,steps+1))

    #test methods
    predictions <- predict(model,
                           newdata = X,
                           steps = 1:steps)
    testthat::expect_equal(dim(predictions),c(n,steps))
    predictions_last <- predict(model,
                                newdata = X)
    testthat::expect_equal(predictions_last,predictions[,steps,drop = FALSE])
    testthat::expect_equal(dim(coef(model)),c(p + 1,1))
    testthat::expect_equal(dim(coef(model,step = c(1,10))),c(p + 1,2))
    testthat::expect_equal(coef(model,step = 0),model$coeff_path[,1,drop = FALSE])
    testthat::expect_identical(class(autoplot(model, Y, X)), c("gg","ggplot"))
  }
)

test_cases_WRGA <- expand.grid(
  stepsize = c(0.05,0.1),
  tau = c(0.25,0.5,0.75),
  kernel =  c("Gaussian","uniform","parabolic","triangular"),
  stringsAsFactors = FALSE)

test_cases_WRGA["test_name"] = apply(test_cases_WRGA, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for WRGA",
  .cases = test_cases_WRGA,
  {
    n <- 100;p <- 500; s <- 5
    steps <- 10
    X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
    Y <- 2*X[,1] + rnorm(n,0,1)
    model <- qboost(X,
                    Y,
                    tau = tau,
                    m_stop = steps,
                    stepsize = NULL,
                    h = 0.1,
                    kernel = kernel)
    testthat::expect_length(model$selection_path, steps)
    testthat::expect_equal(dim(model$coeff_path),c(p+1,steps+1))

    #test methods
    predictions <- predict(model,
                           newdata = X,
                           steps = 1:steps)
    predictions_last <- predict(model,
                                newdata = X)
    testthat::expect_equal(predictions_last,predictions[,steps,drop = FALSE])
    testthat::expect_equal(dim(predictions),c(n,steps))
    testthat::expect_identical(class(autoplot(model, Y, X)), c("gg","ggplot"))
  }
)

test_cases_nonsmooth <- expand.grid(
  stepsize = c(0.05,0.1),
  tau = c(0.25,0.5,0.75),
  stringsAsFactors = FALSE)

test_cases_nonsmooth["test_name"] = apply(test_cases_nonsmooth, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for the nonsmooth Variant",
  .cases = test_cases_nonsmooth,
  {
    n <- 100;p <- 500; s <- 5
    steps <- 10
    X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
    Y <- 2*X[,1] + rnorm(n,0,1)
    model <- qboost(X,
                    Y,
                    tau = tau,
                    m_stop = steps,
                    stepsize = stepsize,
                    h = 0.1,
                    kernel = NULL)
    testthat::expect_length(model$selection_path, steps)
    testthat::expect_equal(dim(model$coeff_path),c(p+1,steps+1))

    #test methods
    predictions <- predict(model,
                           newdata = X,
                           steps = 1:steps)
    testthat::expect_equal(dim(predictions),c(n,steps))
    predictions_last <- predict(model,
                                newdata = X)
    testthat::expect_equal(predictions_last,predictions[,steps,drop = FALSE])
    testthat::expect_identical(class(autoplot(model, Y, X)), c("gg","ggplot"))
  }
)


testthat::test_that("WRGA Degenerate", {
  n <- 10;p <- 2
  X <- matrix(rep(1,2*n),ncol = 2)
  steps <- 10
  X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
  Y <- rep(2,n)

  model <- qboost(X,
                  Y,
                  tau = 0.5,
                  m_stop = steps,
                  stepsize = 0.1,
                  h = 0.1,
                  kernel = "Gaussian")
  testthat::expect_length(model$selection_path, steps)
  testthat::expect_equal(dim(model$coeff_path),c(p+1,steps+1))
})



