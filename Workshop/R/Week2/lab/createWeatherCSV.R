# createweathercsv.R

#install.packages("nycflights13")
require(nycflights13)

weather <- data.frame(weather)
View(weather)

weather$date <- ISOdate(weather$year, weather$month, weather$day, weather$hour, 
          min=0, sec=0,tz="America/New_York")

weather$temperature <- round(weather$temp*(5/9)+32,1)

newarkweather <- data.frame(weather$date, weather$temperature)
View(newarkweather)
colnames(newarkweather) <- c("date","temp")
View(newarkweather)
str(newarkweather)

write.csv(newarkweather, file="temperatures.csv", row.names=FALSE)
getwd()