#' Calculate the smoothed check loss.
#'
#' @param x A vector.
#' @param tau A quantile.
#' @param h The bandwith for smoothing.
#' @param kernel The kernel.
#'
#' @return The average smoothed check loss.
#' @export
#'
smooth_check_loss <- function(x,tau,h = 0.1, kernel = "Gaussian"){
  if (is.null(kernel)){
    loss = abs(x)/2 + (tau-1/2)*x
    return(mean(loss))
  }
  if (kernel == "Gaussian"){
    l = (2/pi)^(1/2)*exp(-(x/h)^2/2)+(x/h)*(1-2*stats::pnorm(-(x/h)))
  } else if (kernel == "uniform"){
    l = ifelse(abs(x/h)<= 1,((x/h)^2/2+0.5),abs(x/h))
  } else if (kernel == "parabolic"){
    l = ifelse(abs(x/h)<= 1,3*(x/h)^2/4-(x/h)^4/8+3/8,abs(x/h))
  } else if (kernel == "triangular"){
    l = ifelse(abs(x/h)<= 1,(x/h)^2-abs((x/h))^3/3+1/3,abs(x/h))
  }
  loss = (h/2)*l+(tau-0.5)*x
  return(mean(loss))
}

#' Calculate the gradient of the smoothed check loss.
#'
#' @param x A vector.
#' @param tau A quantile.
#' @param h The bandwith for smoothing.
#' @param kernel The kernel.
#'
#' @return The gradient of the smoothed check loss.
#' @export
#'
grad_smooth_check_loss <- function(x,tau,h = 0.1, kernel = "Gaussian"){
  if (is.null(kernel)){
    integrated_kernel = ifelse(x <= 0, 0, 1)
  } else if (kernel == "Gaussian"){
    integrated_kernel <- stats::pnorm(x/h)
  } else if (kernel == "uniform"){
    integrated_kernel <- sapply(x/h,function(x){
      if (x <= -1){
        return(0)
      } else if (x >= 1){
        return(1)
      } else {
        return(0.5*(x+1))
      }
    })
  } else if (kernel == "parabolic"){
    integrated_kernel <- sapply(x/h,function(x){
      if (x <= -1){
        return(0)
      } else if (x >= 1){
        return(1)
      } else {
        return(0.5 + 3/4*x - x^3/4)
      }
    })
  } else if (kernel == "triangular"){
    integrated_kernel <- sapply(x/h,function(x){
      if (x <= -1){
        return(0)
      } else if (x >= 1){
        return(1)
      } else if (x <= 0){
        return(.5+x+1/2*x^2)
      } else {
        return(.5+x-1/2*x^2)
      }
    })
  }
  grad <- integrated_kernel-tau
  return(grad)
}

