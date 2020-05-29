
#' Computes sensitivity d' from hit rate
#' 
#' This formula assumes zero bias 
#'
#' @param d sensitivity
#' @param H hit rate
#'
#' @return
#' @export
#'
#' @examples
#' compute_FA_from_dprime(2,.8)
compute_FA_from_dprime <- function(d,H) {
  
  return(pnorm(qnorm(H) - d))
}

#' Computes d' from hits and false alarms
#' 
#' Uses traditional formula  
#'
#' @param H hit rate (pecentages)
#' @param FA false alarm rate (pecentages) 
#' @param should_correct boolean, whether we should correct perfect performance
#' @param max_d to which values we should correct perfect performance
#'
#' @return
#' @export
#' @examples
#' compute_dprime(.4,.8)
#' compute_dprime(.4, 1, should_correct = T)
compute_dprime <- function(H,FA,should_correct = F, max_d=4) {
  stopifnot(H >= 0 & H <= 1)
  stopifnot(FA >= 0 & FA <= 1)
  
  d <- qnorm(H) - qnorm(FA)
  if(should_correct & is.infinite(d)) {
    d <- max_d*sign(d)
  }
  return(d)
}

#' Computes d' from hits and false alarms
#'
#' Bias correction currently works only for C estimate of bias
#'
#' @param H hit rate (pecentages)
#' @param FA false alarm rate (pecentages) 
#' @param type type of bias estimate, possible values are "C", C_rel","lnB". Default value is C
#' @param should_correct boolean, whether we should correct perfect performance
#' @param max_bias to which values we should correct perfect performance
#'
#' @return
#' @export
#'
#' @examples
#' compute_bias(.4,.8)
#' compute_bias(.2,.7, type = "lnB")
#' compute_bias(.4, 1, should_correct = T)
compute_bias <- function(H,FA, type = "C",should_correct = F, max_bias=4) {
  stopifnot(H >= 0 & H <= 1)
  stopifnot(FA >= 0 & FA <= 1)
  
  zH <- qnorm(H)
  zF <- qnorm(FA)
  
  if(type == "C") {
    bias <- -0.5 * (zH + zF)
  } else if(type == "C_rel") {
    bias <- -0.5 * ((zH + zF)/(zH - zF))
  } else if(type == "lnB") {
    bias <- -0.5 * (zH^2 - zF^2)
  } else {
    stop("unknown bias measure")
  }
  
  if(should_correct & is.infinite(bias)) {
    bias <- max_bias*sign(bias)
  }
  return(bias)
  
}