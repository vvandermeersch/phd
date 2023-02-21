# extract file specification
years_to_file <- function(years){
  if(max(years) < 2001){
    return(list(name = "2000_0kyr", min = 0, max = 2000))
  }
  else if(max(years) < 4001){
    return(list(name = "4000_2001kyr", min = 2001, max = 4000))
  }
  else if(max(years) < 6001){
    return(list(name = "6000_4001kyr", min = 4001, max = 6000))
  }
  else if(max(years) < 8001){
    return(list(name = "8000_6001kyr", min = 6001, max = 8000))
  }
  else if(max(years) < 10001){
    return(list(name = "10000_8001kyr", min = 8001, max = 10000))
  }
  else if(max(years) < 12001){
    return(list(name = "12000_10001kyr", min = 10001, max = 12000))
  }
  else if(max(years) < 14001){
    return(list(name = "14000_12001kyr", min = 12001, max = 14000))
  }
  else if(max(years) < 16001){
    return(list(name = "16000_14001kyr", min = 14001, max = 16000))
  }
  else if(max(years) < 18001){
    return(list(name = "18000_16001kyr", min = 16001, max = 18000))
  }
  else if(max(years) < 20001){
    return(list(name = "20000_18001kyr", min = 18001, max = 20000))
  }
  else if(max(years) < 22001){
    return(list(name = "22000_20001kyr", min = 20001, max = 22000))
  }
  else if(max(years) < 24001){
    return(list(name = "24000_22001kyr", min = 22001, max = 24000))
  }
}

#extract months (between 1 - 24000)
year_to_months <- function(yr_beg, yr_end, max_year){
  
  min_mn <- ((max_year-yr_end)+1)*12-11
  max_mn <- ((max_year-yr_beg)+1)*12
  
  return(list(min = min_mn, max = max_mn))
 
}
