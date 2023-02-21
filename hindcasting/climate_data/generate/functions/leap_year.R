# get number of days per month with leap year condition


leap_year <- function(year){
  
  std_yr <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  lp_yr <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  if(year %% 400 == 0){
    return(lp_yr)
  }else if(year %% 100 == 0){
    return(std_yr)
  }else if(year %% 4 == 0){
    return(lp_yr)
  }else{
    return(std_yr)
  }

}