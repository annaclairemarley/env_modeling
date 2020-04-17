#' calc_crop_yield
#'
#' Calculates almond yield anomaly using temperature and precipitation data
#'
#' @param df Climate data dataframe
#' @param a_param crop paramater * temperature
#' @param b_param crop paramater * temperature^2
#' @param c_param crop paramater * precipitation
#' @param d_param crop paramater * precipitation^2
#' @param intercept 
#' @param month_temp month of temperature data needed 
#' @param month_precip month of precipitation data needed 
#'
#' @return dataframe of yield anomalies, 
#'

calc_crop_yield = function(df, a_param = -0.015, b_param = -0.0046, 
                           c_param = -0.07, d_param = 0.0043, intercept = 0.28,
                           month_temp = 2, month_precip = 1) {
  
  # new dataframe of mean monthly min temp and monthly precipitation
  clim_month = df %>% 
    group_by(year, month) %>% 
    summarize(meantmin = mean(tmin_c),
              precip=sum(precip)) %>% 
    ungroup()
  
  # make an empty dataframe to put results in
  number_years = length(unique(clim_month$year))
  crop = as.data.frame(matrix(nrow=number_years, ncol=2))
  colnames(crop)=c("year","yield_anom")
  
  for (i in 1:length(unique(clim_month$year))){
    success = TRUE
    # fill in the year column 
    crop$year[i] = unique(clim_month$year)[i]
    
    # choose specific month each year to extract min temp
    tmin_df <- clim_month %>% 
      filter(year == unique(clim_month$year)[i]) %>% 
      filter(month == month_temp) %>% 
      select(meantmin) 
    
    # error checking that there is data for tmin for that year
    if (nrow(tmin_df) != 1){
      cat("Warning: Missing data for tmin in", unique(clim_month$year)[i], "\n")
      success = FALSE
    }
    
    #choose specific month each year to precipitation
    precip_df <- clim_month %>% 
      filter(year == unique(clim_month$year)[i]) %>% 
      filter(month == month_precip) %>% 
      select(precip) 
    
    #error checking that there is data for precip for that year
    if(nrow(precip_df) !=1){
      cat("Warning: Missing data for precip in", unique(clim_month$year)[i], "\n")
      success = FALSE
    } else if(precip_df[[1]] < 0 ){ #error checking that precipitation is >= to 0
      cat("Warning: Precip is less than 0 in", unique(clim_month$year)[i], "\n")
      success = FALSE
    }
    
    # calculate yield anomaly
    # error check that it only calculates anomaly if both precip and temp data have values
    if (success == TRUE) {
      crop$yield_anom[i] = a_param*tmin_df[[1]] +
        b_param*tmin_df[[1]]^2 + 
        c_param*precip_df[[1]] +
        d_param*precip_df[[1]]^2 + 
        intercept
    }
  }
  
  # calculate max and min crop yields and return function
  return(list(data = crop, 
              max = max(na.omit(crop$yield_anom)),
              min = min(na.omit(crop$yield_anom))))
  
}