

compute_highflowmetrics = function(m,o, month, wy) {
  
  # bind together the parameters
  flow = cbind.data.frame(m, o, month, wy)
  
  # df with mean error per month 
  eval_df <- flow %>% 
    group_by(wy, month) %>% 
    summarize(maxo=max(o), maxm=max(m)) %>% 
    mutate(error = maxm - maxo) %>% 
    mutate(abs_error = abs(error)) %>% 
    ungroup() %>% 
    group_by(month) %>% 
    summarize(av_error = mean(abs_error))
  
error_list <- c(eval_df$av_error)
    
  
  return(list(error_list))
  
}