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
#' * `params` Additional parameters.
#'
#' @seealso `predict.qboost`
#'
#' @export
qboost <- function(X,
                   Y,
                   tau = 0.5,
                   m_stop = 1,
                   h = 0.1,
                   kernel = "Gaussian",
                   stepsize = NULL){
  #check arguments ####
  checkmate::assertMatrix(X)
  checkmate::assertNumeric(Y, len = dim(X)[1])
  checkmate::assertNumber(tau, lower = 0, upper = 1)
  checkmate::assertIntegerish(m_stop, lower = 1)
  checkmate::assertNumber(h, lower = 0, upper = 1)
  checkmate::assertNumber(stepsize, null.ok = TRUE, lower = 0, upper = 1)
  checkmate::assertChoice(kernel, null.ok = TRUE, c("Gaussian","uniform","parabolic","triangular"))

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
    greedy_step <- update_selection_step(X = X,
                                         residuals = as.numeric(residuals),
                                         tau = tau,
                                         h = h,
                                         kernel = kernel)
    selection_path[m] <- greedy_step$sel_cov
    if (is.null(stepsize)){ #WCGA
      conquer_model <- conquer::conquer(X[,selection_path[1:m], drop = F], Y, tau = tau, h = h, kernel = kernel)
      coeff_path[c(1,selection_path[1:m]+1),m+1] <- conquer_model$coeff
      residuals <- conquer_model$residual
    } else { #WRGA
      coeff_path[,m+1] <- coeff_path[,m]
      if (is.na(greedy_step$sel_cov)){
        coeff_path[1,m+1] <- coeff_path[1,m+1] - stepsize*greedy_step$coef
      } else {
        coeff_path[selection_path[m]+1,m+1] <- coeff_path[selection_path[m]+1,m] - stepsize*greedy_step$coef
        coeff_path[1,m+1] <- coeff_path[1,m+1] + stepsize*greedy_step$coef*cm[selection_path[m]]
      }
      residuals <- Y - (coeff_path[1,m+1] + X %*% coeff_path[-1,m+1,drop = F])
    }
  }
  params <- list("tau" = tau, "h" = h, "kernel" = kernel)
  results <- list("coeff_path" = coeff_path,
                  "selection_path" = selection_path,
                  "params" = params)
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
#' @param new_Y (`numeric()`)\cr
#' A vector of responses (only for evaluation).
#' @param newdata (`matrix`) \cr
#' A `matrix` of covariates for predicitons.
#' @param steps (`integer()`) \cr
#' A `vector` of integers containing the corresponding steps.
#' @param ... arguments passed to the print function and other methods
#' @rdname methods.qboost
#' @aliases methods.qboost predict.qboost coef.qboost autoplot.qboost
#' @method predict qboost
#' @export
predict.qboost <- function(object, newdata, steps = NULL, ...){
  #checking arguments ####
  checkmate::assertClass(object,"qboost")
  checkmate::assertMatrix(newdata,ncols = dim(object$coeff_path)[1] - 1)
  checkmate::assertIntegerish(steps,lower = 0, upper = dim(object$coeff_path)[2]-1, null.ok = TRUE)
  if (is.null(steps)){
    if (is.null(object$cv_m_stop)){
      steps <- dim(object$coeff_path)[2] - 1
    } else {
      steps <- object$cv_m_stop
    }
  }
  predictions <- Matrix::t(object$coeff_path[1,steps + 1] + Matrix::t(newdata %*% object$coeff_path[-1,steps + 1,drop = FALSE]))
  colnames(predictions) <- paste0("Step_",steps)
  return(predictions)
}

#' @rdname methods.qboost
#' @export
coef.qboost <- function(object, steps = NULL, ...){
  #checking arguments ####
  checkmate::assertClass(object,"qboost")
  checkmate::assertIntegerish(steps,lower = 0, upper = dim(object$coeff_path)[2]-1, null.ok = TRUE)

  if (is.null(steps)){
    steps <- dim(object$coeff_path)[2] - 1
  }
  results <- object$coeff_path[,steps + 1,drop = FALSE]
  return(results)
}

#' @rdname methods.qboost
#' @importFrom ggplot2 autoplot
#' @method autoplot qboost
#' @export
autoplot.qboost <- function(object, new_Y, newdata, steps = NULL, ...){
  #checking arguments ####
  checkmate::assertClass(object,"qboost")
  checkmate::assertMatrix(newdata,ncols = dim(object$coeff_path)[1] - 1)
  checkmate::assertIntegerish(steps,lower = 0, upper = dim(object$coeff_path)[2]-1, null.ok = TRUE)

  if (is.null(steps)){
    steps <- 1:dim(object$coeff_path)[2] - 1
  }

  residuals <- new_Y - Matrix::t(object$coeff_path[1,steps + 1] + Matrix::t(newdata %*% object$coeff_path[-1,steps + 1,drop = FALSE]))
  loss_vec <- apply(residuals, 2, function(x) smooth_check_loss(x,tau = object$params$tau, kernel = NULL))
  smoothed_loss_vec <- apply(residuals, 2, function(x) smooth_check_loss(x,
                                                                         tau = object$params$tau,
                                                                         h = object$params$h,
                                                                         kernel = object$params$kernel))
  df_plot <- data.frame("Loss" = c(loss_vec,smoothed_loss_vec),
                        "Step" = rep(steps,2),
                        "Type" = rep(c("Check Loss", "Smoothed Check Loss"), each = length(steps)))
  .data <- NULL
  plot <- ggplot2::ggplot(data = df_plot, ggplot2::aes(x = .data$Step, y = .data$Loss, color = .data$Type)) +
    ggplot2::geom_line() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_colour_manual(values = c("Forestgreen","blue","red"))

  if (!is.null(object$cv_m_stop)){
    df_cv_loss <- data.frame("cv_loss" = object$loss,
                             "Step" = seq_len(length(object$loss)),
                             "Type" = "CV-Loss")
    plot <- plot +
      ggplot2::geom_vline(ggplot2::aes(xintercept = object$cv_m_stop),
                                      color = "black",
                                      linetype = "dashed") +
      ggplot2::geom_text(ggplot2::aes(x = object$cv_m_stop, label = "cv_m_stop", y = object$loss[object$cv_m_stop]),
                         colour="black",
                         angle=90,
                         vjust = 1.2,
                         hjust = -1) +
      ggplot2::geom_line(data = df_cv_loss, ggplot2::aes(x = .data$Step,
                                                         y = .data$cv_loss,
                                                         color = .data$Type))
  }
  return(plot)
}
