#1. Importing the CSV file into R studio. The file should be present inside the working directory
uber <- read.csv("Uber Request Data.csv", header = TRUE)

#2. Data Cleaning and Preparation
library(lubridate)
uber$Request.timestamp <- parse_date_time(x = uber$Request.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"))
uber$Drop.timestamp <- parse_date_time(x = uber$Drop.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"))
uber$Driver.id <- as.factor(uber$Driver.id)

#3. Deriving new variables which will be useful for analysis. 
library(tidyr)
#A. Calculating the trip duration
uber$Duration <- interval(uber$Request.timestamp, uber$Drop.timestamp)/dminutes(1)

#B. Seperating 'Date and Time' of request and drop into new columns
uber <- separate(data = uber, col = "Request.timestamp", into = c("Req.Date","Req.Time"), sep = " ")
uber <- separate(data = uber, col = "Drop.timestamp", into = c("Drop.Date","Drop.Time"), sep = " ")

#C. Deriving a new column which contains the day when the request was made.
uber$RequestDay <- wday(uber$Req.Date, label = TRUE)

#D. Deriving a new column stripping the Request Time to get the 'Hour' on which request was made
uber$RequestHour <- substring(uber$Req.Time,0,2)
uber$RequestHour <- as.factor(uber$RequestHour)

#E. Creating Subsets for City and Airport requests
city <- subset(uber, uber$Pickup.point == "City")
airport <- subset(uber, uber$Pickup.point == "Airport")

#4. Univaiate Analysis
library("ggplot2")
#A. Average duration of the trip
overall_avg_duration <- mean(uber$Duration, na.rm = TRUE)
city_avg_duration <- mean(city$Duration, na.rm = TRUE)
airport_avg_duration <- mean(airport$Duration, na.rm = TRUE)

#B. Analysing on the basis of day and hour of the request for all the requests
demand_day <- ggplot(uber, aes(uber$RequestDay)) + geom_bar(position = "dodge")
demand_hour <- ggplot(uber, aes(uber$RequestHour)) + geom_bar(position = "dodge")

#C.Analysing Daywise and Hourwise for 'City' and 'Airport' requests separately
demand_city_day <- ggplot(city, aes(city$RequestDay)) + geom_bar(position = "dodge")
demand_city_hour <- ggplot(city, aes(city$RequestHour)) + geom_bar(position = "dodge")
demand_airport_day <- ggplot(airport, aes(airport$RequestDay)) + geom_bar(position = "dodge")
demand_airport_hour <- ggplot(airport, aes(airport$RequestHour)) + geom_bar(position = "dodge")

#5. Bivariate Analysis
#A. Analysing on the basis of 'hour', 'pickup point' for all the requests
#Analysing the overall demand at both the pickup points at different hours of the day
demand_point <- ggplot(uber, aes(uber$RequestHour, fill = Pickup.point)) + geom_bar(position = "stack")               #Stacking for a better comparision

#Analysing the statuses on different days of the week
demand_day <- ggplot(uber, aes(uber$RequestDay, fill = Status)) + geom_bar(position = "stack")

#Analysing Supply at different points throughout the day
status_point <- ggplot(uber, aes(uber$Pickup.point, fill = Status)) + geom_bar(position = "stack")

#Day Vs Status Analysis for City requests
city_day_status <- ggplot(city, aes(city$RequestDay, fill = Status)) + geom_bar(position = "dodge")

#Hour Vs Status Analysis for City requests
city_hour_status <- ggplot(city, aes(city$RequestHour, fill = Status)) + geom_bar(position = "dodge")

#Day Vs Status Analysis for Airport requests
airport_day_status <- ggplot(airport, aes(airport$RequestDay, fill = Status)) + geom_bar(position = "dodge")

#Hour Vs Status Analysis for City requests
airport_hour_status <- ggplot(airport, aes(airport$RequestHour, fill = Status)) + geom_bar(position = "dodge")

#6. Plots
demand_point                   #Overall demand at different points
demand_day                     #Overall demand on different days
status_point                   #Overall supply at different points
city_day_status                #City requests per day and their status
city_hour_status               #City requests every hour and their status
airport_day_status             #Airport requests per day and their status
airport_hour_status            #Airport requests every hour and their status

overall_avg_duration           #Overall average duration of a ride
city_avg_duration              #City to Airport average duration
airport_avg_duration           #Airport to City average duration

#6. Writing the file in CSV format for a better visualizaion
write.csv(uber, "UCS.csv")