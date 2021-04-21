#' Greedy procedure to determine the selection covariate
#'
#' Calculates the gradient of the smoothed check-loss for the residuals
#' and selects the column of X which has the highest absolute correlation.
#'
#' @param X A matrix of covariates.
#' @param residuals A vector of residuals.
#' @param tau A quantile.
#' @param h The bandwith for smoothing.
#' @param kernel The kernel.
#'
#' @return A list with components
#' \item{sel_cov}{The index of the selected covariates.}
#' \item{cor}{The corresponding correlation.}
#' @export
#'
update_selection_step <- function(X,
                                  residuals,
                                  tau,
                                  h = 0.1,
                                  kernel = "Gaussian"){
  #calculate the gradient
  grad <- grad_smooth_check_loss(-residuals, tau, h, kernel)
  sel_cov <- which.max(abs(stats::cor(grad,X)))
  results <- list("sel_cov" = sel_cov, "cor" = stats::cor(grad,X)[sel_cov])
  return(results)
}
