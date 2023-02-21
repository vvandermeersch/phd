# extract file specification
years_to_file <- function(years){
  if(max(years) < 2500){
    return(list(name = "0_2.5kyr", min = 0, max = -2499))
  }
  else if(max(years) < 5000){
    return(list(name = "2.5_5kyr", min = -2500, max = -4999))
  }
}

#extract months (between 1 - 30000)
year_to_months <- function(year_min, year_max, fs_yrmin){
  
  min_mn <- (year_min+fs_yrmin)*12+1 
  max_mn <- (year_max+fs_yrmin+1)*12 
  
  return(list(min = min_mn, max = max_mn))
 
}
