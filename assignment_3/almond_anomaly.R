#' almond_anomaly
#'
#' Calculates almond yield anomaly using temperature and precipitation data
#'
#' @param clim_data climate data dataframe
#' @param a crop paramater * temperature
#' @param b crop paramater * temperature^2
#' @param c crop paramater * precipitation
#' @param d crop paramater * precipitation^2
#' @param e intercept of the model
#' @param mean_only logic parameter to decide if the model only outputs mean yield anomaly
#'
#' @return dataframe of yield anomalies, 
#'

almond_anomaly = function(clim_data, a = -0.015, b = -0.0046, c = -0.07, d = 0.0043, e = 0.28, mean_only = FALSE) {
  #Checking the colnames of he datase are righ
    if(!all(c("month", "year", "tmax_c", "tmin_c", "precip") %in% colnames(clim_data))) {
      stop ('Please make sure that the colnames of the data frame include \'month\', \'year\', \'tmax_c\', \'tmin_c\', and \'precip\'.')
    } else if (!is.numeric(clim_data$month)) {
      stop ('Please make sure the month data is numeric')
    } else if (!is.numeric(clim_data$year)) {
      stop ('Please make sure the year data is numeric')
    } else if (!is.numeric(clim_data$tmax_c)) {
      stop ('Please make sure the tmax_c data is numeric')
    } else if (!is.numeric(clim_data$tmin_c)) {
      stop ('Please make sure the tmin_c data is numeric')
    } else if (!is.numeric(clim_data$precip)) {
      stop ('Please make sure the precip data is numeric')
    }
  
  for (i in clim_data$precip) {
    if (i < 0) {
      stop ('Please make sure all the precip data is not negative')
    }
  }
  
  for (i in clim_data$tmax_c) {
    if (i > 100) {
      stop ('Please make sure all the tmax_c data is reasonable')
    }
  }
  
  for (i in clim_data$tmin_c) {
    if (i < -100) {
      stop ('Please make sure all the tmin_c data is reasonable')
    }
  }
    
  # Data wrangling after all the error checking
  climate_df <- clim_data %>%
      group_by(month, year) %>% 
      summarise(tmax = mean(tmax_c),
                tmin = mean(tmin_c), 
                precip=sum(precip))
  
  # Creating a data frame storing necessary information for almond
  climate_tn2 <- climate_df %>% 
    filter(month == 2)
  climate_p1 <- climate_df %>% 
    filter(month == 1)
  
  climate_almond <- data.frame(climate_tn2$year, climate_tn2$tmin, climate_p1$precip)
  colnames(climate_almond) <- c("year", "tn2", "p1")
  
  # Mutate a new column to store all the results
  climate_almond <- climate_almond %>% 
    mutate(anomaly = a*tn2 + b*tn2^2 + c*p1 + d*p1^2 + e)
  
  mean_anomaly <- mean(climate_almond$anomaly)
  
  if (mean_only == FALSE) {
    return(list(almond_df = climate_almond, 
                almond_max = max(climate_almond$anomaly),
                almond_min = min(climate_almond$anomaly)
                )
           )
  } else {
    return(mean_anomaly)
  }
  
 
  
}