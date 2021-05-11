#' Quantile Boosting for high-dimensional data
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
#'
#' @return An object with S3 class `qboost`
#' * `coeff_path` The cofficients along the boosting steps as a matrix (starts with a zero vector).
#' * `selection_path` The selected covariates as a vector.
#'
#' @seealso `predict.qboost`
#'
#' @export
qboost <- function(X,
                   Y,
                   tau = 0.5,
                   m_stop = 50,
                   h = 0.1,
                   kernel = "Gaussian",
                   stepsize = NULL){
  #check arguments ####
  checkmate::assertMatrix(X)
  checkmate::assertNumeric(Y,len = dim(X)[1])
  checkmate::assertNumber(tau,lower = 0, upper = 1)
  checkmate::assertIntegerish(m_stop, lower = 1)
  checkmate::assertNumber(h,lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assertNumber(stepsize,null.ok = TRUE, lower = 0, upper = 1)
  checkmate::assertChoice(kernel,null.ok = TRUE, c("Gaussian","uniform","parabolic","triangular"))

  #initial start
  selection_path <- rep(NA,m_stop)
  coeff_path <- Matrix::Matrix(0,dim(X)[2]+1,m_stop+1)
  #start at the quantile
  coeff_path[1,1] <- stats::quantile(Y,tau)
  residuals <- Y - coeff_path[1,1]
  #center the covariates (not for WCGA)
  if (!is.null(stepsize)){
    cm <- colMeans(X, na.rm = TRUE)
    X <- scale(X, center = cm, scale = FALSE)
  }
  for (m in 1:m_stop){
    greedy_step <- update_selection_step(X, residuals, tau, h = h, kernel = kernel)
    selection_path[m] <- greedy_step$sel_cov
    if (is.null(stepsize)){ #WCGA
      conquer_model <- conquer::conquer(X[,selection_path[1:m], drop = F], Y, tau = tau, h = h, kernel = kernel)
      coeff_path[c(1,selection_path[1:m]+1),m+1] <- conquer_model$coeff
      residuals <- conquer_model$residual
    } else { #WRGA
      coeff_path[,m+1] <- coeff_path[,m]
      if (is.na(greedy_step$sel_cov)){
        coeff_path[1,m+1] <- coeff_path[1,m+1] - stepsize*greedy_step$cor
      } else {
        coeff_path[selection_path[m]+1,m+1] <- coeff_path[selection_path[m]+1,m] - stepsize*greedy_step$cor
        coeff_path[1,m+1] <- coeff_path[1,m+1] + stepsize*greedy_step$cor*cm[selection_path[m]]
      }
      residuals <- Y - (coeff_path[1,m+1] + X %*% coeff_path[-1,m+1])
    }
  }
  results <- list("coeff_path" = coeff_path,
                  "selection_path" = selection_path)
  class(results) <- "qboost"
  return(results)
}



################# Methods for Quantile Boosting

#' Methods for S3 object \code{qboost}
#'
#' Objects of class \code{qboost} are constructed by \code{qboost}.
#' \code{predict.qboost} predicts values based on a \code{qboost} object.
#'
#' @param object (`qboost`) \cr
#' A `qboost` object.
#' @param newdata (`matrix`) \cr
#' A `matrix` of covariates for predicitons.
#' @param steps (`integer()`) \cr
#' A `vector` of integers containing the corresponding steps.
#' @param ... arguments passed to the print function and other methods
#' @rdname methods.qboost
#' @aliases methods.qboost predict.qboost
#' @export
predict.qboost <- function(object, newdata, steps = 0, ...){
  #checking arguments ####
  checkmate::assertClass(object,"qboost")
  checkmate::assertMatrix(newdata,ncols = dim(object$coeff_path)[1] - 1)
  checkmate::assertIntegerish(steps,lower = 0, upper = length(object$selection_path))

  predictions <- Matrix::t(object$coeff_path[1,steps + 1] + Matrix::t(newdata %*% object$coeff_path[-1,steps + 1,drop = FALSE]))
  colnames(predictions) <- paste0("Step_",steps)
  return(predictions)
}

#' @rdname methods.qboost
#' @export

coef.qboost <- function(object, steps = NULL, ...){
  #checking arguments ####
  checkmate::assertClass(object,"qboost")
  checkmate::assertIntegerish(steps,lower = 0, upper = length(object$selection_path), null.ok = TRUE)

  if (is.null(steps)){
    steps <- dim(object$coeff_path)[2] - 1
  }
  results <- object$coeff_path[,steps + 1,drop = FALSE]
  return(results)
}
