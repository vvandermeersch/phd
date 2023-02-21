wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/daily_weather_generator/gwgen_f90-master"

library(lubridate)
library(ggplot2)

input <- read.table(file.path(wd, "0_2.5kyr_gwgen.csv"), sep =",", header = T)
output <- read.table(file.path(wd, "0_2.5kyr_gwgen_output.csv"), sep =",", header = T)

input_1BP <- input[input$year == 1,]
input_1BP$yday <- 15+30*(input_1BP$month-1)
output_1BP <- output[output$year == 1,]
output_1BP$date <- paste0("1849-", output_1BP$month, "-", output_1BP$day)
output_1BP$yday <- yday(output_1BP$date)+(output_1BP$year-1)*365

input$yday <- 15+30*(input$month-1)+(input$year-1950)*365
output$date <- date(paste0(output$year,"-", output$month, "-", output$day))
output$yday <- yday(output$date)+(output$year-1950)*365

for(i in 1:nrow(output)){
  print(paste0(1950-output[i,]$year,"-", output[i,]$month, "-", output[i,]$day))
  print(date(paste0(1850-output[i,]$year,"-", output[i,]$month, "-", output[i,]$day)))
  
}



ggplot() +
  geom_line(data = input, aes(x = yday, y = min.temperature, color = as.factor(station.id)), size=2, alpha = 0.5) +
  geom_line(data = output, aes(x = yday, y = tmin, color = as.factor(id))) +
  theme_minimal() +
  xlab("Day of year") +
  ylab("Min. temperature") +
  theme(legend.position = "None")

