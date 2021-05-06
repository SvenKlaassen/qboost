#' Greedy procedure to determine the selection covariate
#'
#' @description
#' Calculates the gradient of the smoothed check-loss for the residuals
#' and selects the column of X which has the highest absolute correlation.
#'
#' @param X (`matrix`) \cr
#' A matrix of covariates.
#' @param residuals (`numeric()`) \cr
#' A vector of residuals.
#' @param tau (`numeric(1L)`) \cr
#' A quantile.
#' @param h (`numeric(1L)`) \cr
#' The bandwith for smoothing.
#' @param kernel (`character(1L)`) \cr
#' The kernel for smoothing.
#'
#' @return A list with components
#' * `sel_cov` The index of the selected covariates.
#' * `cor` The corresponding correlation.
#'
#' @export
update_selection_step <- function(X,
                                  residuals,
                                  tau,
                                  h = 0.1,
                                  kernel = "Gaussian"){
  #check arguments ####
  checkmate::assertMatrix(X)
  checkmate::assertNumeric(residuals,len = dim(X)[1])
  checkmate::assertNumber(tau,lower = 0, upper = 1)
  checkmate::assertNumber(h,lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assertChoice(kernel,null.ok = TRUE, c("Gaussian","uniform","parabolic","triangular"))

  #calculate the gradient
  grad <- grad_smooth_check_loss(-residuals, tau, h, kernel)
  sel_cov <- which.max(abs(stats::cor(grad,X)))
  results <- list("sel_cov" = sel_cov, "cor" = stats::cor(grad,X)[sel_cov])
  return(results)
}
