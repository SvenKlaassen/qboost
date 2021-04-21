#' Title
#'
#' @param X A matrix of covariates.
#' @param Y A vector of responses.
#' @param tau A quantile.
#' @param m_stop The number of boosting steps.
#' @param h The bandwith for smoothing.
#' @param kernel The kernel for smoothing.
#' @param stepsize The stepsize of the boosting procedure.
#'
#' @return A list with components
#' \item{coeff_path}{The cofficients along the boosting steps as a matrix (starts with a zero vector).}
#' \item{selection_path}{The selected covariates as a vector.}
#' @export
#'
qboost <- function(X,
                   Y,
                   tau = 0.5,
                   m_stop = 50,
                   h = 0.1,
                   kernel = "Gaussian",
                   stepsize = NULL){
  #initial start
  residuals <- Y
  selection_path <- rep(NA,m_stop)
  coeff_path <- matrix(0,dim(X)[2]+1,m_stop+1)
  for (m in 1:m_stop){
    greedy_step <- update_selection_step(X, residuals, tau, h = h, kernel = kernel)
    selection_path[m] <- greedy_step$sel_cov
    if (is.null(stepsize)){ #WCGA
      conquer_model <- conquer::conquer(X[,selection_path[1:m], drop = F], Y, tau = tau, kernel = kernel)
      coeff_path[c(1,selection_path[1:m]+1),m+1] <- conquer_model$coeff
      residuals <- conquer_model$residual
    } else { #WRGA
      coeff_path[,m+1] <- coeff_path[,m]
      coeff_path[selection_path[m]+1,m+1] <- coeff_path[selection_path[m]+1,m] - stepsize*greedy_step$cor
      coeff_path[1,m+1] <- stats::quantile(Y-X%*%coeff_path[-1,m+1],tau)
      residuals <- Y-cbind(1,X)%*%coeff_path[,m+1]
    }
  }
  results <- list("coeff_path" = coeff_path,
                  "selection_path" = selection_path)
  return(results)
}
