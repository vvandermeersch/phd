# extract file specification
# for downscaled monthly data (500yr slice)
years_to_file <- function(years){
  yr_max <- 500 * ceiling(max(years)/500)
  if(yr_max == 500){
	return(list(name = "500_0kyr", min = 0, max = 500))
  }else{
	yr_min <- yr_max - 499
	return(list(name = paste0(yr_max, "_", yr_min, "kyr"), min = yr_min, max = yr_max))  
  }
}

#extract months (between 1 - 6000)
year_to_months <- function(yr_beg, yr_end, max_year){
  
  min_mn <- ((max_year-yr_end)+1)*12-11
  max_mn <- ((max_year-yr_beg)+1)*12
  
  return(list(min = min_mn, max = max_mn))
 
}
