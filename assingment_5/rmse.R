
rmse = function(m,o){
  
  error = (m - o)^2
  
  av = mean(error)
  
  root_sq_error = sqrt(av)
  
  return(list(root_sq_error))
}