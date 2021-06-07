test_cases_WCGA <- expand.grid(
  n = c(100,104),
  tau = c(0.25,0.5,0.75),
  kernel =  c("Gaussian","uniform","parabolic","triangular"),
  stringsAsFactors = FALSE)

test_cases_WCGA["test_name"] = apply(test_cases_WCGA, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for WCGA",
                                   .cases = test_cases_WCGA,
                                   {
                                     p <- 500; s <- 5
                                     steps <- 10
                                     X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
                                     Y <- 2*X[,1] + rnorm(n,0,1)
                                     model <- cv_qboost(X,
                                                        Y,
                                                        tau = tau,
                                                        m_stop = steps,
                                                        stepsize = NULL,
                                                        h = 0.1,
                                                        kernel = kernel,
                                                        n_folds = 5)
                                     testthat::expect_equal(dim(model$coeff_path),c(p+1,steps+1))

                                     #test methods
                                     predictions <- predict(model,
                                                            newdata = X,
                                                            steps = 1:steps)
                                     testthat::expect_equal(dim(predictions),c(n,steps))
                                     predictions_cv <- predict(model,
                                                               newdata = X)
                                     testthat::expect_equal(predictions_cv,predictions[,model$cv_m_stop,drop = FALSE])
                                     testthat::expect_equal(dim(coef(model)),c(p + 1,1))
                                     testthat::expect_equal(dim(coef(model,step = c(1,10))),c(p + 1,2))
                                     testthat::expect_equal(coef(model,step = c(0)),model$coeff_path[,1,drop = FALSE])
                                     testthat::expect_identical(class(autoplot(model, Y, X)), c("gg","ggplot"))
                                   }
)

test_cases_WRGA <- expand.grid(
  n = c(100,104),
  stepsize = c(0.05,0.1),
  tau = c(0.25,0.5,0.75),
  kernel =  c("Gaussian","uniform","parabolic","triangular"),
  stringsAsFactors = FALSE)

test_cases_WRGA["test_name"] = apply(test_cases_WRGA, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for WRGA",
                                   .cases = test_cases_WRGA,
                                   {
                                     p <- 500; s <- 5
                                     steps <- 10
                                     X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
                                     Y <- 2*X[,1] + rnorm(n,0,1)
                                     model <- cv_qboost(X,
                                                        Y,
                                                        tau = tau,
                                                        m_stop = steps,
                                                        stepsize = NULL,
                                                        h = 0.1,
                                                        kernel = kernel,
                                                        n_folds = 5)
                                     testthat::expect_equal(dim(model$coeff_path),c(p+1,steps+1))

                                     #test methods
                                     predictions <- predict(model,
                                                            newdata = X,
                                                            steps = 1:steps)
                                     testthat::expect_equal(dim(predictions),c(n,steps))
                                     predictions_cv <- predict(model,
                                                               newdata = X)
                                     testthat::expect_equal(predictions_cv,predictions[,model$cv_m_stop,drop = FALSE])
                                     testthat::expect_identical(class(autoplot(model, Y, X)), c("gg","ggplot"))
                                   }
)

test_cases_nonsmooth <- expand.grid(
  n = c(100,104),
  stepsize = c(0.05,0.1),
  tau = c(0.25,0.5,0.75),
  stringsAsFactors = FALSE)

test_cases_nonsmooth["test_name"] = apply(test_cases_nonsmooth, 1, paste, collapse = "_")

patrick::with_parameters_test_that("Unit tests for the nonsmooth Variant",
                                   .cases = test_cases_nonsmooth,
                                   {
                                     p <- 500; s <- 5
                                     steps <- 10
                                     X <- matrix(runif(n*p,0,1),nrow = n,ncol = p)
                                     Y <- 2*X[,1] + rnorm(n,0,1)
                                     model <- cv_qboost(X,
                                                        Y,
                                                        tau = tau,
                                                        m_stop = steps,
                                                        stepsize = stepsize,
                                                        h = 0.1,
                                                        kernel = NULL,
                                                        n_folds = 5)
                                     testthat::expect_equal(dim(model$coeff_path),c(p+1,steps+1))

                                     #test methods
                                     predictions <- predict(model,
                                                            newdata = X,
                                                            steps = 1:steps)
                                     testthat::expect_equal(dim(predictions),c(n,steps))
                                     predictions_cv <- predict(model,
                                                            newdata = X)
                                     testthat::expect_equal(predictions_cv,predictions[,model$cv_m_stop,drop = FALSE])
                                     testthat::expect_identical(class(autoplot(model, Y, X)), c("gg","ggplot"))
                                   }
)


