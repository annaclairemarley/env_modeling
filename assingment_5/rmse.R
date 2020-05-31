#' rmse 
#'
#' room mean squared error
#'
#' @param m modeled data
#' @param o observed data
#'
rmse = function(m,o){
  
  error = (m - o)^2
  
  av = mean(error)
  
  root_sq_error = sqrt(av)
  
  return(list(root_sq_error))
}