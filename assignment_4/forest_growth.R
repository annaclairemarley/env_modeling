#' forest_growth
#'
#' @param time time
#' @param C initial carbon (kg C)
#' @param parms$canopy_thresh canopy closure threshold (kg C)
#' @param parms$K carrying capacity (kg C)
#' @param parms$r initial growth rate (kg/year)
#' @param parms$g linear growth rate (kg/year)
#' @param parms$temp temperature (degrees C)
#'
#' @return growth rate of forest at any point in time
#' @export
#'
#' @examples 

forest_growth = function(time, C, parms){
  
  # rate of forest growth 
  carb_change = parms$r*C
  
  # forest growth is 0 when temperatures are below 0
  if (parms$temp < 0){
    
    carb_change = 0
    
  # forest growth is 0 when carrying capacity is reached  
  } else if (C >= parms$K) {
    
    carb_change = 0
    
  # forest growth becomes linear when carbon is above the threshold canopy closure  
  } else if (C > parms$canopy_thresh){
    
    carb_change = parms$g
    
  } else {
    
    carb_change = carb_change
  }
  
  return(list(carb_change))
}


 








