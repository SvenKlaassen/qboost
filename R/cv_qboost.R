#' Cross-validated Quantile Boosting for high-dimensional data
#'
#' @description
#' Applying a greedy algorithm to minimize the expected smoothed check loss.
#' Employs the Weak Restricted Greedy Algorithm. For the orthogonal variant, set `stepsize` to `NULL`.
#'
#' @param X (`matrix`) \cr
#' A matrix of covariates.
#' @param Y (`numeric()`) \cr
#' A vector of responses.
#' @param tau (`numeric(1L)`) \cr
#' A quantile.
#' @param m_stop (`integer(1L)`) \cr
#' The number of boosting steps.
#' @param h (`numeric(1L)`) \cr
#' The bandwith for smoothing.
#' @param kernel (`character(1L)`) \cr
#' The kernel for smoothing.
#' @param stepsize (`numeric(1L)`) \cr
#' The stepsize of the boosting procedure. Set to `NULL` for WCGA.
#' @param n_folds (`integer(1L)`) \cr
#' Number folds used.
#'
#' @return An object with S3 class `qboost`
#' * `coeff_path` The cofficients along the boosting steps as a matrix (starts with a zero vector).
#' * `params` Additional parameters.
#'
#' @seealso `predict.qboost`
#'
#' @export
cv_qboost <- function(X,
                      Y,
                      tau = 0.5,
                      m_stop = 1,
                      h = 0.1,
                      kernel = "Gaussian",
                      stepsize = NULL,
                      n_folds = 5){
  #Argument checks ####
  checkmate::assertMatrix(X)
  checkmate::assertNumeric(Y, len = dim(X)[1])
  checkmate::assertNumber(tau, lower = 0, upper = 1)
  checkmate::assertIntegerish(m_stop, lower = 1)
  checkmate::assertNumber(h, lower = 0, upper = 1)
  checkmate::assertNumber(stepsize, null.ok = TRUE, lower = 0, upper = 1)
  checkmate::assertChoice(kernel, null.ok = TRUE, c("Gaussian","uniform","parabolic","triangular"))
  checkmate::assertIntegerish(n_folds, lower = 2, upper = dim(X)[1])

  # K-fold partition ####
  n <- dim(X)[1]
  num_drop <- n %% n_folds # number of indices to be added seperately
  if (num_drop > 0){
    indices_drop <- sample(n, num_drop, replace = FALSE) #indices to be added seperately
    indices_remain <- (1:n)[-indices_drop]
    folds <- split(sample(indices_remain,n - num_drop, replace = FALSE), as.factor(1:n_folds))
    #add the dropped indices
    for (i in 1:num_drop){
      folds[[i]] <- c(folds[[i]], indices_drop[i])
    }
  } else {
    folds <- split(sample(n, n, replace = FALSE), as.factor(1:n_folds))
  }
  # Fitting the algorithm ####
  fold_results <- lapply(1:n_folds, function(num_fold){
    sample_test <- folds[[num_fold]]
    sample_train <- (1:n)[-sample_test]

    model <- qboost(X[sample_train,],
                    Y[sample_train],
                    tau = tau,
                    m_stop = m_stop,
                    h = h,
                    kernel = kernel,
                    stepsize = stepsize)
    residuals <- Y[sample_test] - predict.qboost(model, newdata = X[sample_test,], steps = 1:m_stop)
    loss <- apply(residuals, 2, function(x) smooth_check_loss(x,tau = tau, h = h, kernel = kernel))
    results <- list("coeff_path" = model$coeff_path, "loss" = loss)
    return(results)
  })
  # Combining the results ####
  loss <- Reduce("+",lapply(fold_results, function(x) {x$loss}))/n_folds
  coeff_path <- Reduce("+", lapply(fold_results, function(x) {x$coeff_path}))/n_folds

  params <- list("tau" = tau, "h" = h, "kernel" = kernel, "folds" = folds)
  results <- list("coeff_path" = coeff_path,
                  "cv_m_stop" = which.min(loss),
                  "loss" = loss,
                  "params" = params)
  class(results) <- "qboost"
  return(results)
}
