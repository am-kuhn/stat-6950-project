library(hms)
my_data <- read.csv("race_results.csv", header = T)
head(my_data)

## sex male = 1, female = 0
my_data$Sex <- ifelse(my_data$Sex == "Men", 1, 0)

## convert time string to time object
my_data$World.Record <- as_hms(my_data$World.Record)
my_data$Standing.record <- as_hms(my_data$Standing.record)
my_data$Finished.time..avg. <- as_hms(my_data$Finished.time..avg.)

## convert time to seconds
World.Record <- as.numeric(my_data$World.Record)
Standing.record <- as.numeric(my_data$Standing.record)
Finished.time.avg <- as.numeric(my_data$Finished.time..avg.)

## percent deviation from world record
percent_world <- (World.Record - Finished.time.avg)/World.Record * 100 

## percent deviation from standing record
percent_standing <- (Standing.Record - Finished.time.avg)/Standing.Record * 100 


## subsetting
sub_data <- cbind(my_data[,c(8,15,16,17,18,19,20,24,25,26)],percent_world)
pairs(sub_data)

sub_data <- cbind(my_data[,c(8,15,16,18,19,25)],percent_world)
pairs(sub_data)

## model deviate from weather
model1 <- lm(percent_world ~ YEAR + Air.Temperature..C. + Adjusted.wind.speed + Relative.Humidity....+Heat.Index..National.Oceanic.and.Atmospheric.Administration..Last.modified.2014......C.+Simplified.Wet.Bulb.Globe.Temperature..American.College.of.Sports.Medicine..1984......C., data = my_data)
summary(model1)
