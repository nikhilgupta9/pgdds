rm(list=ls())

#Loading the required libraries

library(dplyr)
library(forecast)
library(tseries)
require(graphics)
library(lubridate)
library(ggplot2)
library(tidyverse)
cols <- c("black", "green")
labels <- c("Raw", "Smoothed")

#loading the dataset

sales_data<-read.csv("Global Superstore.csv")

str(sales_data) 

head(sales_data)

dim(sales_data) #51290 observations  and 24 attributes

#Checking for missing values
sapply(sales_data, function(x) sum(is.na(x))) #all the missing values are in postal.code attribute
#not going to fix that as we are not considering  this field for any analysis

#checking for duplicated values
sum(duplicated(sales_data)) #no duplicated values

#removing unneccesary attributes 
#Since city name attribute is there, postal code column is of no use in analysis 
#As we found no duplicate rows, columns with ID's can be eliminated
#as order date is present shipping date is not necessary for this analysis
#shipping mode and order priority is based on customer priority, so those attributes can be eliminated
#category and sub-category is co-related to customer segments, so they can be eliminated
#profit is calculated including the discount and shipping cost,so discount & shipping cost can be eliminated
#based on the objective, trend needs to be identified based on market which means, city,state,country and region can be elinated

sales_data_necessary<-sales_data[,-c(1,2,4,5,6,7,9,10,11,12,14,15,16,17,18,21,23,24)] #51290 obs of 6 variables, viz.Order.Date, Segment, Market, Quantity, Sales,Profit

#converting order.date into data format

sales_data_necessary$Order.Date<-as.Date(sales_data_necessary$Order.Date, "%d-%m-%Y")


first_order_date <- min(sales_data_necessary$Order.Date) #Getting the beginning date of orders
first_order_date ##"2011-01-01"

last_order_date <- max(sales_data_necessary$Order.Date) #Getting the last date of orders
last_order_date ##"2014-12-31"

#48 months containing order information is present in the globalstore sales dataset

#Storing the difference in number of months from first month in a newly derived column - 'Months'
sales_data_necessary$Months <- sapply(sales_data_necessary$Order.Date, 
                            function(x) length(seq(from= min(sales_data_necessary$Order.Date), to=x, by='month')))

#extracting year from the date to analyse 2 most consistently profitable segments for 4 year (2011-14) - 48 months

sales_data_necessary$Year <- format(as.Date(sales_data_necessary$Order.Date), "%Y")

#extracting month and year from the date

sales_data_necessary$Order.Month.Year <- format(as.Date(sales_data_necessary$Order.Date), "%m-%y")

sales_data_necessary <- sales_data_necessary[order(sales_data_necessary$Months),] 

View(sales_data_necessary)

#Adressing case sensitivity
sales_data_necessary$Segment<-tolower(sales_data_necessary$Segment)
sales_data_necessary$Market<-toupper(sales_data_necessary$Market)


#converting segment and market into factors
sales_data_necessary$Segment<-as.factor(sales_data_necessary$Segment)
sales_data_necessary$Market<-as.factor(sales_data_necessary$Market)

head(sales_data_necessary$Segment) #factor variable with 3 levels, consumer, corporate, home office
head(sales_data_necessary$Market) #factor variable with 7 levels, US, APAC, EU, AFRICA, CANADA, EMEA, LATHAM 

sales_data_necessary <- sales_data_necessary[,-1]
#**********************************Data Preparation********************#

#Subsetting sales_data_necessary into 21 market segments by aggregating Sales, Quantity & Profit of each, Calculating COV based on Profit & 
#Analysing Qty, Sales & Profit of 21 segments over the time period of 48 months

#subsetting US market segments 

#US_consumer
US_consumer<-filter(sales_data_necessary, Market=="US" & Segment=="consumer") #5191/51290 obs = 10%
US_consumer_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,US_consumer, sum )
US_consumer_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,US_consumer, mean)

US_consumer_Profit <- sum(US_consumer$Profit) 
US_consumer_Sales <- sum(US_consumer$Sales)
US_consumer_Demand <- sum(US_consumer$Quantity)

plot(ts(US_consumer_agg$Profit))
plot(ts(US_consumer_agg$Sales))
plot(ts(US_consumer_agg$Quantity))

US_consumer_cov<-sd(US_consumer_agg$Profit)/mean(US_consumer_agg$Profit)
US_consumer_cov #1.01239

US_consumer_stats <- data.frame(US_consumer_cov,US_consumer_Profit,US_consumer_Sales,US_consumer_Demand)

US_consumer_stats <- data.frame(t(US_consumer_stats))
US_consumer_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(US_consumer_stats)<-c("US_consumer_stats", "item")
rownames(US_consumer_stats)<-c(1:4)

#US_corporate
US_corporate<-filter(sales_data_necessary, Market=="US" & Segment=="corporate") #3020 obs/51290 = 6%
US_corporate_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,US_corporate, sum )
US_corporate_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,US_corporate, mean)

US_corporate_Profit <- sum(US_corporate$Profit) 
US_corporate_Sales <- sum(US_corporate$Sales)
US_corporate_Demand <- sum(US_corporate$Quantity)

plot(ts(US_corporate_agg$Profit))
plot(ts(US_corporate_agg$Sales))
plot(ts(US_corporate_agg$Quantity))

US_corporate_cov<-sd(US_corporate_agg$Profit)/mean(US_corporate_agg$Profit)
US_corporate_cov #1.002409

US_corporate_stats <- data.frame(US_corporate_cov,US_corporate_Profit,US_corporate_Sales,US_corporate_Demand)

US_corporate_stats <- data.frame(t(US_corporate_stats))
US_corporate_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(US_corporate_stats)<-c("US_corporate_stats", "item")
rownames(US_corporate_stats)<-c(1:4)

#US_homeoffice
US_homeoffice<-filter(sales_data_necessary, Market=="US" & Segment=="home office") #1783 obs /51290 = 3.4%
US_homeoffice_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,US_homeoffice, sum )
US_homeoffice_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,US_homeoffice, mean)

US_homeoffice_Profit <- sum(US_homeoffice$Profit) 
US_homeoffice_Sales <- sum(US_homeoffice$Sales)
US_homeoffice_Demand <- sum(US_homeoffice$Quantity)

plot(ts(US_homeoffice_agg$Profit))
plot(ts(US_homeoffice_agg$Sales))
plot(ts(US_homeoffice_agg$Quantity))

US_homeoffice_cov<-sd(US_homeoffice_agg$Profit)/mean(US_homeoffice_agg$Profit)
US_homeoffice_cov #1.096147

US_homeoffice_stats <- data.frame(US_homeoffice_cov,US_homeoffice_Profit,US_homeoffice_Sales,US_homeoffice_Demand)

US_homeoffice_stats <- data.frame(t(US_homeoffice_stats))
US_homeoffice_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(US_homeoffice_stats)<-c("US_homeoffice_stats", "item")
rownames(US_homeoffice_stats)<-c(1:4)


#subsetting AFRICA market segments

#AFRICA_consumer
AFRICA_consumer<-filter(sales_data_necessary, Market=="AFRICA" & Segment=="consumer") #2381 obs/51290 = 4.6%
AFRICA_consumer_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,AFRICA_consumer, sum )
AFRICA_consumer_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,AFRICA_consumer, mean)

AFRICA_consumer_Profit <- sum(AFRICA_consumer$Profit) 
AFRICA_consumer_Sales <- sum(AFRICA_consumer$Sales)
AFRICA_consumer_Demand <- sum(AFRICA_consumer$Quantity)

plot(ts(AFRICA_consumer_agg$Profit))
plot(ts(AFRICA_consumer_agg$Sales))
plot(ts(AFRICA_consumer_agg$Quantity))

AFRICA_consumer_cov<-sd(AFRICA_consumer_agg$Profit)/mean(AFRICA_consumer_agg$Profit)
AFRICA_consumer_cov #1.319585

AFRICA_consumer_stats <- data.frame(AFRICA_consumer_cov,AFRICA_consumer_Profit,AFRICA_consumer_Sales,AFRICA_consumer_Demand)

AFRICA_consumer_stats <- data.frame(t(AFRICA_consumer_stats))
AFRICA_consumer_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(AFRICA_consumer_stats)<-c("AFRICA_consumer_stats", "item")
rownames(AFRICA_consumer_stats)<-c(1:4)


#AFRICA_corporate
AFRICA_corporate<-filter(sales_data_necessary, Market=="AFRICA" & Segment=="corporate") #1312 obs/51290 = 2.5%
AFRICA_corporate_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,AFRICA_corporate, sum )
AFRICA_corporate_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,AFRICA_corporate, mean )

AFRICA_corporate_Profit <- sum(AFRICA_corporate$Profit) 
AFRICA_corporate_Sales <- sum(AFRICA_corporate$Sales)
AFRICA_corporate_Demand <- sum(AFRICA_corporate$Quantity)

plot(ts(AFRICA_corporate_agg$Profit))
plot(ts(AFRICA_corporate_agg$Sales))
plot(ts(AFRICA_corporate_agg$Quantity))

AFRICA_corporate_cov<-sd(AFRICA_corporate_agg$Profit)/mean(AFRICA_corporate_agg$Profit)
AFRICA_corporate_cov #1.776105

AFRICA_corporate_stats <- data.frame(AFRICA_corporate_cov,AFRICA_corporate_Profit,AFRICA_corporate_Sales,AFRICA_corporate_Demand)

AFRICA_corporate_stats <- data.frame(t(AFRICA_corporate_stats))
AFRICA_corporate_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(AFRICA_corporate_stats)<-c("AFRICA_corporate_stats", "item")
rownames(AFRICA_corporate_stats)<-c(1:4)


#AFRICA_homeoffice
AFRICA_homeoffice<-filter(sales_data_necessary, Market=="AFRICA" & Segment=="home office") #894 obs/51290 = 1.7%
AFRICA_homeoffice_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,AFRICA_homeoffice, sum )
AFRICA_homeoffice_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,AFRICA_homeoffice,mean)

AFRICA_homeoffice_Profit <- sum(AFRICA_homeoffice$Profit) 
AFRICA_homeoffice_Sales <- sum(AFRICA_homeoffice$Sales)
AFRICA_homeoffice_Demand <- sum(AFRICA_homeoffice$Quantity)

plot(ts(AFRICA_homeoffice_agg$Profit))
plot(ts(AFRICA_homeoffice_agg$Sales))
plot(ts(AFRICA_homeoffice_agg$Quantity))

AFRICA_homeoffice_cov<-sd(AFRICA_homeoffice_agg$Profit)/mean(AFRICA_homeoffice_agg$Profit)
AFRICA_homeoffice_cov #1.789996

AFRICA_homeoffice_stats <- data.frame(AFRICA_homeoffice_cov,AFRICA_homeoffice_Profit,AFRICA_homeoffice_Sales,AFRICA_homeoffice_Demand)

AFRICA_homeoffice_stats <- data.frame(t(AFRICA_homeoffice_stats))
AFRICA_homeoffice_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(AFRICA_homeoffice_stats)<-c("AFRICA_homeoffice_stats", "item")
rownames(AFRICA_homeoffice_stats)<-c(1:4)


#subsetting APAC market segments 

#APAC_consumer
APAC_consumer<-filter(sales_data_necessary, Market=="APAC" & Segment=="consumer") #5699 obs / 51290 = 11%
APAC_consumer_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,APAC_consumer, sum )
APAC_consumer_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,APAC_consumer,mean)

APAC_consumer_Profit <- sum(APAC_consumer$Profit) 
APAC_consumer_Sales <- sum(APAC_consumer$Sales)
APAC_consumer_Demand <- sum(APAC_consumer$Quantity)

plot(ts(APAC_consumer_agg$Profit))
plot(ts(APAC_consumer_agg$Sales))
plot(ts(APAC_consumer_agg$Quantity))

APAC_consumer_cov<-sd(APAC_consumer_agg$Profit)/mean(APAC_consumer_agg$Profit)
APAC_consumer_cov #0.6321323

APAC_consumer_stats <- data.frame(APAC_consumer_cov,APAC_consumer_Profit,APAC_consumer_Sales,APAC_consumer_Demand)

APAC_consumer_stats <- data.frame(t(APAC_consumer_stats))
APAC_consumer_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(APAC_consumer_stats)<-c("APAC_consumer_stats", "item")
rownames(APAC_consumer_stats)<-c(1:4)

#APAC_corporate
APAC_corporate<-filter(sales_data_necessary, Market=="APAC" & Segment=="corporate") #3283 obs/51290 = 6.4%
APAC_corporate_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,APAC_corporate, sum )
APAC_corporate_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,APAC_corporate, mean )

APAC_corporate_Profit <- sum(APAC_corporate$Profit) 
APAC_corporate_Sales <- sum(APAC_corporate$Sales)
APAC_corporate_Demand <- sum(APAC_corporate$Quantity)

plot(ts(APAC_corporate_agg$Profit))
plot(ts(APAC_corporate_agg$Sales))
plot(ts(APAC_corporate_agg$Quantity))

APAC_corporate_cov<-sd(APAC_corporate_agg$Profit)/mean(APAC_corporate_agg$Profit)
APAC_corporate_cov #0.6980869

APAC_corporate_stats <- data.frame(APAC_corporate_cov,APAC_corporate_Profit,APAC_corporate_Sales,APAC_corporate_Demand)

APAC_corporate_stats <- data.frame(t(APAC_corporate_stats))
APAC_corporate_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(APAC_corporate_stats)<-c("APAC_corporate_stats", "item")
rownames(APAC_corporate_stats)<-c(1:4)


#APAC_homeoffice
APAC_homeoffice<-filter(sales_data_necessary, Market=="APAC" & Segment=="home office") #2020 obs/51290 = 4%
APAC_homeoffice_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,APAC_homeoffice, sum )
APAC_homeoffice_avg <- aggregate(cbind(Sales, Quantity, Profit)~ Months,APAC_homeoffice,mean)

APAC_homeoffice_Profit <- sum(APAC_homeoffice$Profit) 
APAC_homeoffice_Sales <- sum(APAC_homeoffice$Sales)
APAC_homeoffice_Demand <- sum(APAC_homeoffice$Quantity)


plot(ts(APAC_homeoffice_agg$Profit))
plot(ts(APAC_homeoffice_agg$Sales))
plot(ts(APAC_homeoffice_agg$Quantity))

APAC_homeoffice_cov<-sd(APAC_homeoffice_agg$Profit)/mean(APAC_homeoffice_agg$Profit)
APAC_homeoffice_cov #1.045978

APAC_homeoffice_stats <- data.frame(APAC_homeoffice_cov,APAC_homeoffice_Profit,APAC_homeoffice_Sales,APAC_homeoffice_Demand)

APAC_homeoffice_stats <- data.frame(t(APAC_homeoffice_stats))
APAC_homeoffice_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(APAC_homeoffice_stats)<-c("APAC_homeoffice_stats", "item")
rownames(APAC_homeoffice_stats)<-c(1:4)

#subsetting EU market segments 

#EU_consumer
EU_consumer<-filter(sales_data_necessary, Market=="EU" & Segment=="consumer") #5186 obs / 51290 = 10%
EU_consumer_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EU_consumer, sum )
EU_consumer_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EU_consumer, mean )

EU_consumer_Profit <- sum(EU_consumer$Profit) 
EU_consumer_Sales <- sum(EU_consumer$Sales)
EU_consumer_Demand <- sum(EU_consumer$Quantity)

plot(ts(EU_consumer_agg$Profit))
plot(ts(EU_consumer_agg$Sales))
plot(ts(EU_consumer_agg$Quantity))

EU_consumer_cov<-sd(EU_consumer_agg$Profit)/mean(EU_consumer_agg$Profit)
EU_consumer_cov #0.6243052

EU_consumer_stats <- data.frame(EU_consumer_cov,EU_consumer_Profit,EU_consumer_Sales,EU_consumer_Demand)

EU_consumer_stats <- data.frame(t(EU_consumer_stats))
EU_consumer_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(EU_consumer_stats)<-c("EU_consumer_stats", "item")
rownames(EU_consumer_stats)<-c(1:4)

#EU_corporate
EU_corporate<-filter(sales_data_necessary, Market=="EU" & Segment=="corporate") #3077 obs/51290 = 6%
EU_corporate_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EU_corporate, sum )
EU_corporate_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EU_corporate, mean )

EU_corporate_Profit <- sum(EU_corporate$Profit) 
EU_corporate_Sales <- sum(EU_corporate$Sales)
EU_corporate_Demand <- sum(EU_corporate$Quantity)

plot(ts(EU_corporate_agg$Profit))
plot(ts(EU_corporate_agg$Sales))
plot(ts(EU_corporate_agg$Quantity))

EU_corporate_cov<-sd(EU_corporate_agg$Profit)/mean(EU_corporate_agg$Profit)
EU_corporate_cov #0.7638072

EU_corporate_stats <- data.frame(EU_corporate_cov,EU_corporate_Profit,EU_corporate_Sales,EU_corporate_Demand)

EU_corporate_stats <- data.frame(t(EU_corporate_stats))
EU_corporate_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(EU_corporate_stats)<-c("EU_corporate_stats", "item")
rownames(EU_corporate_stats)<-c(1:4)

#EU_homeoffice
EU_homeoffice<-filter(sales_data_necessary, Market=="EU" & Segment=="home office") #1737 obs/51290 = 3.38%
EU_homeoffice_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EU_homeoffice, sum )
EU_homeoffice_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EU_homeoffice, mean)

EU_homeoffice_Profit <- sum(EU_homeoffice$Profit) 
EU_homeoffice_Sales <- sum(EU_homeoffice$Sales)
EU_homeoffice_Demand <- sum(EU_homeoffice$Quantity)

plot(ts(EU_homeoffice_agg$Profit))
plot(ts(EU_homeoffice_agg$Sales))
plot(ts(EU_homeoffice_agg$Quantity))

EU_homeoffice_cov<-sd(EU_homeoffice_agg$Profit)/mean(EU_homeoffice_agg$Profit)
EU_homeoffice_cov #1.116507

EU_homeoffice_stats <- data.frame(EU_homeoffice_cov,EU_homeoffice_Profit,EU_homeoffice_Sales,EU_homeoffice_Demand)

EU_homeoffice_stats <- data.frame(t(EU_homeoffice_stats))
EU_homeoffice_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(EU_homeoffice_stats)<-c("EU_homeoffice_stats", "item")
rownames(EU_homeoffice_stats)<-c(1:4)


#subsetting EMEA market segments 

#EMEA_consumer
EMEA_consumer<-filter(sales_data_necessary, Market=="EMEA" & Segment=="consumer") #2538 obs/51290 = 5%
EMEA_consumer_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EMEA_consumer, sum )
EMEA_consumer_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EMEA_consumer, mean)

EMEA_consumer_Profit <- sum(EMEA_consumer$Profit) 
EMEA_consumer_Sales <- sum(EMEA_consumer$Sales)
EMEA_consumer_Demand <- sum(EMEA_consumer$Quantity)

plot(ts(EMEA_consumer_agg$Profit))
plot(ts(EMEA_consumer_agg$Sales))
plot(ts(EMEA_consumer_agg$Quantity))

EMEA_consumer_cov<-sd(EMEA_consumer_agg$Profit)/mean(EMEA_consumer_agg$Profit)
EMEA_consumer_cov #2.188271

EMEA_consumer_stats <- data.frame(EMEA_consumer_cov,EMEA_consumer_Profit,EMEA_consumer_Sales,EMEA_consumer_Demand)

EMEA_consumer_stats <- data.frame(t(EMEA_consumer_stats))
EMEA_consumer_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(EMEA_consumer_stats)<-c("EMEA_consumer_stats", "item")
rownames(EMEA_consumer_stats)<-c(1:4)

#EMEA_corporate
EMEA_corporate<-filter(sales_data_necessary, Market=="EMEA" & Segment=="corporate") #1574 obs/ 51290 = 3%
EMEA_corporate_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EMEA_corporate, sum )
EMEA_corporate_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EMEA_corporate, mean)

EMEA_corporate_Profit <- sum(EMEA_corporate$Profit) 
EMEA_corporate_Sales <- sum(EMEA_corporate$Sales)
EMEA_corporate_Demand <- sum(EMEA_corporate$Quantity)

plot(ts(EMEA_corporate_agg$Profit))
plot(ts(EMEA_corporate_agg$Sales))
plot(ts(EMEA_corporate_agg$Quantity))

EMEA_corporate_cov<-sd(EMEA_corporate_agg$Profit)/mean(EMEA_corporate_agg$Profit)
EMEA_corporate_cov #4.467102

EMEA_corporate_stats <- data.frame(EMEA_corporate_cov,EMEA_corporate_Profit,EMEA_corporate_Sales,EMEA_corporate_Demand)

EMEA_corporate_stats <- data.frame(t(EMEA_corporate_stats))
EMEA_corporate_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(EMEA_corporate_stats)<-c("EMEA_corporate_stats", "item")
rownames(EMEA_corporate_stats)<-c(1:4)


#EMEA_homeoffice
EMEA_homeoffice<-filter(sales_data_necessary, Market=="EMEA" & Segment=="home office") #917 obs/51290 = 1.7%
EMEA_homeoffice_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EMEA_homeoffice, sum )
EMEA_homeoffice_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,EMEA_homeoffice, mean)

EMEA_homeoffice_Profit <- sum(EMEA_homeoffice$Profit) 
EMEA_homeoffice_Sales <- sum(EMEA_homeoffice$Sales)
EMEA_homeoffice_Demand <- sum(EMEA_homeoffice$Quantity)

plot(ts(EMEA_homeoffice_agg$Profit))
plot(ts(EMEA_homeoffice_agg$Sales))
plot(ts(EMEA_homeoffice_agg$Quantity))

EMEA_homeoffice_cov<-sd(EMEA_homeoffice_agg$Profit)/mean(EMEA_homeoffice_agg$Profit)
EMEA_homeoffice_cov #5.880747

EMEA_homeoffice_stats <- data.frame(EMEA_homeoffice_cov,EMEA_homeoffice_Profit,EMEA_homeoffice_Sales,EMEA_homeoffice_Demand)

EMEA_homeoffice_stats <- data.frame(t(EMEA_homeoffice_stats))
EMEA_homeoffice_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(EMEA_homeoffice_stats)<-c("EMEA_homeoffice_stats", "item")
rownames(EMEA_homeoffice_stats)<-c(1:4)


#subsetting LATAM market segments 

#LATAM_consumer
LATAM_consumer<-filter(sales_data_necessary, Market=="LATAM" & Segment=="consumer") #5321 obs/51290 = 10.37%
LATAM_consumer_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,LATAM_consumer, sum )
LATAM_consumer_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,LATAM_consumer, mean)

LATAM_consumer_Profit <- sum(LATAM_consumer$Profit) 
LATAM_consumer_Sales <- sum(LATAM_consumer$Sales)
LATAM_consumer_Demand <- sum(LATAM_consumer$Quantity)

plot(ts(LATAM_consumer_agg$Profit))
plot(ts(LATAM_consumer_agg$Sales))
plot(ts(LATAM_consumer_agg$Quantity))

LATAM_consumer_cov<-sd(LATAM_consumer_agg$Profit)/mean(LATAM_consumer_agg$Profit)
LATAM_consumer_cov #0.6614828

LATAM_consumer_stats <- data.frame(LATAM_consumer_cov,LATAM_consumer_Profit,LATAM_consumer_Sales,LATAM_consumer_Demand)

LATAM_consumer_stats <- data.frame(t(LATAM_consumer_stats))
LATAM_consumer_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(LATAM_consumer_stats)<-c("LATAM_consumer_stats", "item")
rownames(LATAM_consumer_stats)<-c(1:4)


#LATAM_corporate
LATAM_corporate<-filter(sales_data_necessary, Market=="LATAM" & Segment=="corporate") #3053 obs/51290 = 6%
LATAM_corporate_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,LATAM_corporate, sum )
LATAM_corporate_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,LATAM_corporate, mean)

LATAM_corporate_Profit <- sum(LATAM_corporate$Profit) 
LATAM_corporate_Sales <- sum(LATAM_corporate$Sales)
LATAM_corporate_Demand <- sum(LATAM_corporate$Quantity)

plot(ts(LATAM_corporate_agg$Profit))
plot(ts(LATAM_corporate_agg$Sales))
plot(ts(LATAM_corporate_agg$Quantity))

LATAM_corporate_cov<-sd(LATAM_corporate_agg$Profit)/mean(LATAM_corporate_agg$Profit)
LATAM_corporate_cov #0.8111217

LATAM_corporate_stats <- data.frame(LATAM_corporate_cov,LATAM_corporate_Profit,LATAM_corporate_Sales,LATAM_corporate_Demand)

LATAM_corporate_stats <- data.frame(t(LATAM_corporate_stats))
LATAM_corporate_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(LATAM_corporate_stats)<-c("LATAM_corporate_stats", "item")
rownames(LATAM_corporate_stats)<-c(1:4)


#LATAM_homeoffice
LATAM_homeoffice<-filter(sales_data_necessary, Market=="LATAM" & Segment=="home office") #1920 obs/51290 = 4%
LATAM_homeoffice_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,LATAM_homeoffice, sum )
LATAM_homeoffice_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,LATAM_homeoffice, mean)

LATAM_homeoffice_Profit <- sum(LATAM_homeoffice$Profit) 
LATAM_homeoffice_Sales <- sum(LATAM_homeoffice$Sales)
LATAM_homeoffice_Demand <- sum(LATAM_homeoffice$Quantity)

plot(ts(LATAM_homeoffice_agg$Profit))
plot(ts(LATAM_homeoffice_agg$Sales))
plot(ts(LATAM_homeoffice_agg$Quantity))

LATAM_homeoffice_cov<-sd(LATAM_homeoffice_agg$Profit)/mean(LATAM_homeoffice_agg$Profit)
LATAM_homeoffice_cov #1.175698

LATAM_homeoffice_stats <- data.frame(LATAM_homeoffice_cov,LATAM_homeoffice_Profit,LATAM_homeoffice_Sales,LATAM_homeoffice_Demand)

LATAM_homeoffice_stats <- data.frame(t(LATAM_homeoffice_stats))
LATAM_homeoffice_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(LATAM_homeoffice_stats)<-c("LATAM_homeoffice_stats", "item")
rownames(LATAM_homeoffice_stats)<-c(1:4)

#subsetting CANADA market segments 

#CANADA_consumer
CANADA_consumer<-filter(sales_data_necessary, Market=="CANADA" & Segment=="consumer") #202 obs/51290 = 0.4%
CANADA_consumer_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,CANADA_consumer, sum )
CANADA_consumer_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,CANADA_consumer, mean)

CANADA_consumer_Profit <- sum(CANADA_consumer$Profit) 
CANADA_consumer_Sales <- sum(CANADA_consumer$Sales)
CANADA_consumer_Demand <- sum(CANADA_consumer$Quantity)

plot(ts(CANADA_consumer_agg$Profit))
plot(ts(CANADA_consumer_agg$Sales))
plot(ts(CANADA_consumer_agg$Quantity))

CANADA_consumer_cov<-sd(CANADA_consumer_agg$Profit)/mean(CANADA_consumer_agg$Profit)
CANADA_consumer_cov #1.395312

CANADA_consumer_stats <- data.frame(CANADA_consumer_cov,CANADA_consumer_Profit,CANADA_consumer_Sales,CANADA_consumer_Demand)

CANADA_consumer_stats <- data.frame(t(CANADA_consumer_stats))
CANADA_consumer_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(CANADA_consumer_stats)<-c("CANADA_consumer_stats", "item")
rownames(CANADA_consumer_stats)<-c(1:4)


#CANADA_corporate
CANADA_corporate<-filter(sales_data_necessary, Market=="CANADA" & Segment=="corporate") #110 obs/51290 = 0.2%
CANADA_corporate_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,CANADA_corporate, sum )
CANADA_corporate_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,CANADA_corporate, mean)

CANADA_corporate_Profit <- sum(CANADA_corporate$Profit) 
CANADA_corporate_Sales <- sum(CANADA_corporate$Sales)
CANADA_corporate_Demand <- sum(CANADA_corporate$Quantity)

plot(ts(CANADA_corporate_agg$Profit))
plot(ts(CANADA_corporate_agg$Sales))
plot(ts(CANADA_corporate_agg$Quantity))

CANADA_corporate_cov<-sd(CANADA_corporate_agg$Profit)/mean(CANADA_corporate_agg$Profit)
CANADA_corporate_cov #1.552775

CANADA_corporate_stats <- data.frame(CANADA_corporate_cov,CANADA_corporate_Profit,CANADA_corporate_Sales,CANADA_corporate_Demand)

CANADA_corporate_stats <- data.frame(t(CANADA_corporate_stats))
CANADA_corporate_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(CANADA_corporate_stats)<-c("CANADA_corporate_stats", "item")
rownames(CANADA_corporate_stats)<-c(1:4)

#CANADA_homeoffice
CANADA_homeoffice<-filter(sales_data_necessary, Market=="CANADA" & Segment=="home office") #72 obs/51290 = 0.14%
CANADA_homeoffice_agg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,CANADA_homeoffice, sum )
CANADA_homeoffice_avg<-aggregate(cbind(Sales, Quantity, Profit)~ Months,CANADA_homeoffice, mean)

CANADA_homeoffice_Profit <- sum(CANADA_homeoffice$Profit) 
CANADA_homeoffice_Sales <- sum(CANADA_homeoffice$Sales)
CANADA_homeoffice_Demand <- sum(CANADA_homeoffice$Quantity)

plot(ts(CANADA_homeoffice_agg$Profit))
plot(ts(CANADA_homeoffice_agg$Sales))
plot(ts(CANADA_homeoffice_agg$Quantity))

CANADA_homeoffice_cov<-sd(CANADA_homeoffice_agg$Profit)/mean(CANADA_homeoffice_agg$Profit)
CANADA_homeoffice_cov #2.243461

CANADA_homeoffice_stats <- data.frame(CANADA_homeoffice_cov,CANADA_homeoffice_Profit,CANADA_homeoffice_Sales,CANADA_homeoffice_Demand)

CANADA_homeoffice_stats <- data.frame(t(CANADA_homeoffice_stats))
CANADA_homeoffice_stats$blankVar <- c("cov","Profit","Sales","Demand") 
colnames(CANADA_homeoffice_stats)<-c("CANADA_homeoffice_stats", "item")
rownames(CANADA_homeoffice_stats)<-c(1:4)

COV_DF<-as.data.frame(as.list.data.frame(c(US_consumer_stats, US_corporate_stats, US_homeoffice_stats,
                                           LATAM_consumer_stats, LATAM_corporate_stats, LATAM_homeoffice_stats,
                                           CANADA_consumer_stats, CANADA_corporate_stats, CANADA_homeoffice_stats,
                                           EU_consumer_stats, EU_corporate_stats, EU_homeoffice_stats,EMEA_consumer_stats,
                                           EMEA_corporate_stats, EMEA_homeoffice_stats,
                                           APAC_consumer_stats, APAC_corporate_stats, APAC_homeoffice_stats,
                                           AFRICA_consumer_stats, AFRICA_corporate_stats, AFRICA_homeoffice_stats)))

COV_DF<-data.frame(t(COV_DF))
colnames(COV_DF)<-c("monthlyCOV","montlyPROFIT","monthlySALES","mothlyDEMAND")

#Removing unnecessary rows

COV_DF<-COV_DF[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,
                  34,36,38,40,42,44),]

COV_DF$marketsegment<-(c("US_consumer", "US_corporate", "US_homeoffice",
                         "LATAM_consumer", "LATAM_corporate", "LATAM_homeoffice",
                         "CANADA_consumer", "CANADA_corporate", "CANADA_homeoffice",
                         "EU_consumer", "EU_corporate", "EU_homeoffice","EMEA_consumer",
                         "EMEA_corporate", "EMEA_homeoffice",
                         "APAC_consumer", "APAC_corporate", "APAC_homeoffice",
                         "AFRICA_consumer", "AFRICA_corporate", "AFRICA_homeoffice"))

rownames(COV_DF)<-c(1:21)
COV_DF

#subsets for the market segments have been created and coeffecient of variation of profit has been calculated 
#Lesser the coefficient of variation, profitable is the segment
#Consistency can be observed through the time series plots
#So Based on the coeffiecient of variation of profit and time series plots obtained from the 21 market segments
#EU market Consumer segment and APAC market Consumer segment are proved to be the 2 most consistently profitable 

#......................Markets/Segments/Market-Segments Proportion Analysis ...................#


#Count vs Month, time series plot for all segments
SegmentTSPlot <- ggplot(sales_data_necessary, aes(Months, fill=Segment)) + 
  geom_bar(position = "stack")
SegmentTSPlot
#Consumer is the largest segment followed by Corporate

#Count vs Month, time series plot for all markets
MarketTSPlot <- ggplot(sales_data_necessary, aes(Months, fill=Market)) + 
  geom_bar(position = "stack")
MarketTSPlot
#APAC is the largest Market followed by LATM, EU

# Time series plots of Sales, Profit and Quantity of all the 21 subsets created
#AFRICA
ggplot(AFRICA_consumer_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "Africa_consumer: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(AFRICA_corporate_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "Africa_corporate: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(AFRICA_homeoffice_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "Africa_homeoffice: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

#APAC
ggplot(APAC_consumer_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "APAC_consumer: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(APAC_corporate_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "APAC_corporate: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(APAC_homeoffice_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "APAC_homeoffice: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

#CANADA
ggplot(CANADA_consumer_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "CANADA_consumer: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(CANADA_corporate_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "CANADA_corporate: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(CANADA_homeoffice_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "CANADA_homeoffice: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")  

#EMEA
ggplot(EMEA_consumer_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "EMEA_consumer: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(EMEA_corporate_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "EMEA_corporate: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(EMEA_homeoffice_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "EMEA_homeoffice: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")
#EU
ggplot(EU_consumer_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "EU_consumer: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(EU_corporate_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "EU_corporate: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(EU_homeoffice_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "EU_homeoffice: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")  
#LATAM
ggplot(LATAM_consumer_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "LATAM_consumer: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(LATAM_corporate_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "LATAM_corporate: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(LATAM_homeoffice_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "LATAM_homeoffice: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")
#US
ggplot(US_consumer_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "US_consumer: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(US_corporate_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "US_corporate: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

ggplot(US_homeoffice_agg, aes(Months)) +
  geom_line(aes(y=Sales), colour="red") + 
  geom_line(aes(y=Profit), colour="blue") + 
  geom_line(aes(y=Quantity), colour="green") +
  labs(title = "US_homeoffice: Sales(Red), Profit(Blue), Quantity(Green) over Months",
       x="Months", y="Counts")

#......................timeseries-AUTOARIMA/ Classical Decomposition...................#


#Model on first 42 rows(months) as remaining 6 rows(months) will be used for evaluation

nrow(EU_consumer_agg) #has all 48 months - Sales,Quantity & Profit of EU - consumer
nrow(APAC_consumer_agg) #has all 48 months - Sales,Quantity & Profit of APAC_consumer

EU_consumer_agg_S <- EU_consumer_agg[,c(1,2)] #has all 48 months - Sales
EU_consumer_agg_Q <- EU_consumer_agg[,c(1,3)] #has all 48 months - Quantity

APAC_consumer_agg_S <- APAC_consumer_agg[,c(1,2)] #has all 48 months - Sales
APAC_consumer_agg_Q <- APAC_consumer_agg[,c(1,3)] #has all 48 months - Quantity

EU_consumer_Total <- EU_consumer_agg[1:42, ]
APAC_consumer_Total<- APAC_consumer_agg[1:42, ]

#Sales
EU_consumer_Sale <- ts(EU_consumer_Total$Sales,frequency = 12)
APAC_consumer_Sale <- ts(APAC_consumer_Total$Sales,frequency = 12)

#Quantity - Demand
EU_consumer_Quantity <- ts(EU_consumer_Total$Quantity,frequency = 12)
APAC_consumer_Quantity <- ts(APAC_consumer_Total$Quantity,frequency = 12)

#Profit
EU_consumer_Revenue <- ts(EU_consumer_Total$Profit,frequency = 12)
APAC_consumer_Revenue <- ts(APAC_consumer_Total$Profit,frequency = 12)

#Plotting the above 6 time series

#Plotting EU_consumer  Sale,Demand & Revenue
plot(EU_consumer_Sale, main = "EU-Consumer Market Segment - Sales")
plot(EU_consumer_Quantity, main = "EU-Consumer Market Segment - Demand")
plot(EU_consumer_Revenue, main = "EU-Consumer Market Segment - Revenue")


#Plotting APAC_consumer  Sale,Demand & Revenue
plot(APAC_consumer_Sale, main = "APAC-Consumer Market Segment - Sales")
plot(APAC_consumer_Quantity, main = "APAC-Consumer Market Segment - Demand")
plot(APAC_consumer_Revenue, main = "APAC-Consumer Market Segment - Revenue")

##-------------#####---------------#####----------

#Smoothing the series using Moving Average Smoothing Method (two-sided) 
#for all 6 time series to isolate any trend/seasonality from noise


#Smoothing EU_consumer SALE

plot(EU_consumer_Sale, main = "EU-Consumer Market Segment - Sales")

w <-1
EU_consumer_Sale_smoothed <- stats::filter(EU_consumer_Sale, 
                                          filter=rep(1/(2*w+1),(2*w+1)), 
                                          method='convolution', sides=2)

#Smoothing left end of the time series
diff <- EU_consumer_Sale_smoothed[w+2] - EU_consumer_Sale_smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_consumer_Sale_smoothed[i] <- EU_consumer_Sale_smoothed[i+1] - diff
}

#Smoothing right end of the time series
n <- length(EU_consumer_Sale)
diff <- EU_consumer_Sale_smoothed[n-w] - EU_consumer_Sale_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_consumer_Sale_smoothed[i] <- EU_consumer_Sale_smoothed[i-1] + diff
}

lines(EU_consumer_Sale_smoothed, col = "green")
legend("bottomright", labels, col=cols, lwd=2)


#Converting EU_consumer_Sale_smoothed into a dataframe
EU_consumer_Sale_smoothed_df <- as.data.frame(cbind(EU_consumer_Total$Months, as.vector(EU_consumer_Sale_smoothed)))
colnames(EU_consumer_Sale_smoothed_df) <- c('Months', 'Sales')


#Smoothing EU_consumer DEMAND

plot(EU_consumer_Quantity, main = "EU-Consumer Market Segment - Demand")

w <-1
EU_consumer_Qty_smoothed <- stats::filter(EU_consumer_Quantity, 
                                           filter=rep(1/(2*w+1),(2*w+1)), 
                                           method='convolution', sides=2)

#Smoothing left end of the time series
diff <- EU_consumer_Qty_smoothed[w+2] - EU_consumer_Qty_smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_consumer_Qty_smoothed[i] <- EU_consumer_Qty_smoothed[i+1] - diff
}

#Smoothing right end of the time series
n <- length(EU_consumer_Quantity)
diff <- EU_consumer_Qty_smoothed[n-w] - EU_consumer_Qty_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_consumer_Qty_smoothed[i] <- EU_consumer_Qty_smoothed[i-1] + diff
}

lines(EU_consumer_Qty_smoothed, col = "green")
legend("bottomright", labels, col=cols, lwd=2)


#Converting EU_consumer_Qty_smoothed into a dataframe
EU_consumer_Qty_smoothed_df <- as.data.frame(cbind(EU_consumer_Total$Months, as.vector(EU_consumer_Qty_smoothed)))
colnames(EU_consumer_Qty_smoothed_df) <- c('Months', 'Quantity')


#Smoothing EU_consumer REVENUE

plot(EU_consumer_Revenue, main = "EU-Consumer Market Segment - Revenue")


w <-1
EU_consumer_Revenue_smoothed <- stats::filter(EU_consumer_Revenue, 
                                           filter=rep(1/(2*w+1),(2*w+1)), 
                                           method='convolution', sides=2)

#Smoothing left end of the time series
diff <- EU_consumer_Revenue_smoothed[w+2] - EU_consumer_Revenue_smoothed[w+1]
for (i in seq(w,1,-1)) {
  EU_consumer_Revenue_smoothed[i] <- EU_consumer_Revenue_smoothed[i+1] - diff
}

#Smoothing right end of the time series
n <- length(EU_consumer_Revenue)
diff <- EU_consumer_Revenue_smoothed[n-w] - EU_consumer_Revenue_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_consumer_Revenue_smoothed[i] <- EU_consumer_Revenue_smoothed[i-1] + diff
}

lines(EU_consumer_Revenue_smoothed, col = "green")
legend("bottomright", labels, col=cols, lwd=2)


#Smoothing APAC_consumer_Sale

plot(APAC_consumer_Sale, main = "APAC-Consumer Market Segment - Sales")

w <-1
APAC_consumer_Sale_smoothed <- stats::filter(APAC_consumer_Sale, 
                                           filter=rep(1/(2*w+1),(2*w+1)), 
                                           method='convolution', sides=2)

#Smoothing left end of the time series
diff <- APAC_consumer_Sale_smoothed[w+2] - APAC_consumer_Sale_smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_consumer_Sale_smoothed[i] <- APAC_consumer_Sale_smoothed[i+1] - diff
}

#Smoothing right end of the time series
n <- length(APAC_consumer_Sale)
diff <- APAC_consumer_Sale_smoothed[n-w] - APAC_consumer_Sale_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_consumer_Sale_smoothed[i] <- APAC_consumer_Sale_smoothed[i-1] + diff
}

lines(APAC_consumer_Sale_smoothed, col = "green")
legend("bottomright", labels, col=cols, lwd=2)


#Converting APAC_consumer_Sale_smoothed into a dataframe
APAC_consumer_Sale_smoothed_df <- as.data.frame(cbind(APAC_consumer_Total$Months, as.vector(APAC_consumer_Sale_smoothed)))
colnames(APAC_consumer_Sale_smoothed_df) <- c('Months', 'Sales')


#Smoothing APAC_consumer DEMAND

plot(APAC_consumer_Quantity, main = "APAC-Consumer Market Segment - Demand")

w <-1
APAC_consumer_Qty_smoothed <- stats::filter(APAC_consumer_Quantity, 
                                          filter=rep(1/(2*w+1),(2*w+1)), 
                                          method='convolution', sides=2)

#Smoothing left end of the time series
diff <- APAC_consumer_Qty_smoothed[w+2] - APAC_consumer_Qty_smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_consumer_Qty_smoothed[i] <- APAC_consumer_Qty_smoothed[i+1] - diff
}

#Smoothing right end of the time series
n <- length(APAC_consumer_Quantity)
diff <- APAC_consumer_Qty_smoothed[n-w] - APAC_consumer_Qty_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_consumer_Qty_smoothed[i] <- APAC_consumer_Qty_smoothed[i-1] + diff
}

lines(APAC_consumer_Qty_smoothed, col = "green")
legend("bottomright", labels, col=cols, lwd=2)


#Converting APAC_consumer_Qty_smoothed into a dataframe
APAC_consumer_Qty_smoothed_df <- as.data.frame(cbind(APAC_consumer_Total$Months, as.vector(APAC_consumer_Qty_smoothed)))
colnames(APAC_consumer_Qty_smoothed_df) <- c('Months', 'Quantity')


#Smoothing APAC_consumer REVENUE

plot(APAC_consumer_Revenue, main = "APAC-Consumer Market Segment - Revenue")


w <-1
APAC_consumer_Revenue_smoothed <- stats::filter(APAC_consumer_Revenue, 
                                              filter=rep(1/(2*w+1),(2*w+1)), 
                                              method='convolution', sides=2)

#Smoothing left end of the time series
diff <- APAC_consumer_Revenue_smoothed[w+2] - APAC_consumer_Revenue_smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_consumer_Revenue_smoothed[i] <- APAC_consumer_Revenue_smoothed[i+1] - diff
}

#Smoothing right end of the time series
n <- length(APAC_consumer_Revenue)
diff <- APAC_consumer_Revenue_smoothed[n-w] - APAC_consumer_Revenue_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_consumer_Revenue_smoothed[i] <- APAC_consumer_Revenue_smoothed[i-1] + diff
}

lines(APAC_consumer_Revenue_smoothed, col = "green")
legend("bottomright", labels, col=cols, lwd=2)



##Forecast of Sales & Demand for Consumer Segment of APAC and EU markets using CLASSICAL DECOMPOSITION METHOD & AUTO.ARIMA METHOD
##----------------------------------------------------------------------------------

#EU Consumer - Sales Forecast - Classical Decomposition
#-----------------
plot(EU_consumer_Sale, main = "EU Consumer Sales - Smooth time Series")
lines(EU_consumer_Sale_smoothed, col="green", lwd=2)

#There is an upward trend plus some seasonal highs visible in the smoothed time series data 
#Since, it follows a curvy pattern and seasonal variation seems to be expanding over the months, 
#we can use the multiplicative model for forecasting, using classical decomposition method
#Seasonality being modeled using sinusoid function

lmfit_EU_consumer_Sale_smoothed <- lm(Sales ~ sin(0.5*Months) * poly(Months,1) + cos(0.5*Months) * poly(Months,1) + Months, 
                                      data=EU_consumer_Sale_smoothed_df)

global_pred_EU_consumer_Sale_smoothed <- predict(lmfit_EU_consumer_Sale_smoothed, Month=EU_consumer_Total$Months)
summary(global_pred_EU_consumer_Sale_smoothed)

lines(EU_consumer_Total$Months, global_pred_EU_consumer_Sale_smoothed, col='red', lwd=2)
labels <- c("Global Prediction")
cols <- c("Red")
legend("bottomright", labels, col=cols, lwd=2)

#Now, let's look at the locally predictable series to see if the above results are good enough or not
#We will model it as an ARMA series

local_pred_EU_consumer_Sale_smoothed  <- EU_consumer_Sale_smoothed - global_pred_EU_consumer_Sale_smoothed
plot(local_pred_EU_consumer_Sale_smoothed, col='green', type = "l", lwd = 2)
legend("bottomright", c("Local Prediction"), col= c("Green"), lwd=2) #it does'nt look flat. But does'nt have a pattern either

acf(local_pred_EU_consumer_Sale_smoothed, main = "ACF for EU_Consumer Sales") #There is 1 peak at 0.1. Rest look acceptable 
acf(local_pred_EU_consumer_Sale_smoothed, type="partial", main = "PACF for EU_Consumer Sales") #After 0.1 lag it is stationary

armafit_EU_consumer_Sale_smoothed <- auto.arima(local_pred_EU_consumer_Sale_smoothed)
tsdiag(armafit_EU_consumer_Sale_smoothed)
armafit_EU_consumer_Sale_smoothed 
# ARIMA(0,0,2)(1,0,0)[12] with zero mean 
# 
# Coefficients:
#         ma1     ma2    sar1
#         0.917  0.8187  0.3125
# s.e.    0.145  0.1612  0.1771
# 
# sigma^2 estimated as 10285216:  log likelihood=-398.79
# AIC=805.57   AICc=806.65   BIC=812.52

#Will have to compare this model with auto.arima model to see best fit
#We'll check if the residual series is white noise
## We will reconfirm and do some test
resi_EU_sales <- local_pred_EU_consumer_Sale_smoothed-fitted(armafit_EU_consumer_Sale_smoothed)

adf.test(resi_EU_sales,alternative = "stationary") #According to this test, we do not reject the null hypothesis as p-value = 0.1065 (p-value>0.05)
#It is not stationary
kpss.test(resi_EU_sales) #p-value = 0.1
#Here, we reject the null hypothesis as it is not stationary (p-value>0.05)

## Both KPSS and adf says that it is not white noise 

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata1 <- EU_consumer_agg[43:48, 1:2]
timevals_out1 <- outdata1$Months

global_pred_EU_consumer_Sale_smoothed_out <- predict(lmfit_EU_consumer_Sale_smoothed,data.frame(Months =timevals_out1))

fcast_EU_consumer_Sales <- global_pred_EU_consumer_Sale_smoothed_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- accuracy(fcast_EU_consumer_Sales,outdata1[,2])[5]
MAPE_class_dec1 #29.09711

#Let's also plot the predictions along with original values (48 months), to
#get a visual feel of the fit

class_dec_pred1 <- c(ts(global_pred_EU_consumer_Sale_smoothed),ts(global_pred_EU_consumer_Sale_smoothed_out))
plot(EU_consumer_agg_S , col = "black", main = "Original and Forecasted Values - EU Consumer Sale")
lines(class_dec_pred1, col = "magenta")
#model is predicting


#Consumer EU Sales using AUTO.ARIMA 
autoarima1 <- auto.arima(EU_consumer_Sale)
autoarima1
tsdiag(autoarima1)
plot(autoarima1$x, col="black", main= "EU Consumer - Sales")
lines(fitted(autoarima1), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col = c("Black", "red"), lwd =2)
#It is an auto-regressive model with one differencing done (drift)
#AIC & BIC VALUES are much lower than ARMA model, S.E. looks minimal - showing confidence in this model
#Auto Arima prediction hugs the original curve

#ARIMA(0,0,0)(1,1,0)[12] with drift 

#Coefficients:
#     sar1        drift
#    -0.5542     414.6662
#s.e. 0.1733     95.6272

#sigma^2 estimated as 72705399:  log likelihood=-315.26
#AIC=636.53   AICc=637.45   BIC=640.73


#Again, let's check if the residual series is white noise
resi_auto_arima1 <- EU_consumer_Sale - fitted(autoarima1)

adf.test(resi_auto_arima1,alternative = "stationary") #adf test says that the residual series is not stationary (p-value = 0.09775)
kpss.test(resi_auto_arima1) #KPSS test says that the residual series is not stationary (p-value = 0.1)

#Also, let's evaluate the model using MAPE
fcast_auto_arima1 <- forecast(autoarima1, n.ahead = 6) #Forecasting for next 6 month to evaluate

MAPE_auto_arima1 <- accuracy(fcast_auto_arima1$mean,outdata1[,2])[5]
MAPE_auto_arima1 #15.75223 - Very good  compared to classical decomposition model

#Lastly, let's plot the predictions along with original values (48 months), to get a visual feel of the fit
auto_arima_pred1 <- c(fitted(autoarima1),ts(fcast_auto_arima1$mean))
plot(EU_consumer_agg_S, col = "black", main = "Original and Forecasted - EU_consumer Sale")
lines(auto_arima_pred1, col = "blue")

#---------------------------------------------------------------------------------------------

#EU Consumer - Quantity Forecast - Classical Decomposition
#-----------------
plot(EU_consumer_Quantity, main = "EU Consumer Quantity - Smooth time Series")
lines(EU_consumer_Qty_smoothed, col="green", lwd=2)

#There is an upward trend plus some seasonal highs visible in the smoothed time series data 
#Since, it follows a curvy pattern and seasonal variation seems to be expanding over the months, 
#we can use the multiplicative model for forecasting, using classical decomposition methodSeasonality being modeled using sinusoid function

lmfit_EU_consumer_Qty_smoothed <- lm(Quantity ~ sin(0.5*Months) * poly(Months,1) + cos(0.5*Months) * poly(Months,1) + Months, 
                                      data=EU_consumer_Qty_smoothed_df)

global_pred_EU_consumer_Qty_smoothed <- predict(lmfit_EU_consumer_Qty_smoothed, Month=EU_consumer_Total$Months)
summary(global_pred_EU_consumer_Qty_smoothed)

lines(EU_consumer_Total$Months, global_pred_EU_consumer_Qty_smoothed, col='red', lwd=2)
labels <- c("Global Prediction")
cols <- c("Red")
legend("bottomright", labels, col=cols, lwd=2)

#Now, let's look at the locally predictable series to see if the above results are good enough or not
#We will model it as an ARMA series

local_pred_EU_consumer_Qty_smoothed  <- EU_consumer_Qty_smoothed - global_pred_EU_consumer_Qty_smoothed
plot(local_pred_EU_consumer_Qty_smoothed, col='green', type = "l", lwd = 2)
legend("bottomright", c("Local Prediction"), col= c("Green"), lwd=2) #it looks flat. Looks like white noise
#cannot draw any pattern or trend from this

acf(local_pred_EU_consumer_Qty_smoothed, main = "ACF for EU_Consumer Quantity") #There is 1 peak at 0. Rest look acceptable 
acf(local_pred_EU_consumer_Qty_smoothed, type="partial", main = "PACF for EU_Consumer Quantity") 

armafit_EU_consumer_Qty_smoothed <- auto.arima(local_pred_EU_consumer_Qty_smoothed)
tsdiag(armafit_EU_consumer_Qty_smoothed)
armafit_EU_consumer_Qty_smoothed 
# ARIMA(1,0,0)(1,0,0)[12] with zero mean 

# Coefficients:
#      ar1    sar1
#      0.4559  0.3169
# s.e.  0.1511  0.1669
# 
# sigma^2 estimated as 1207:  log likelihood=-208.33
# AIC=422.67   AICc=423.3   BIC=427.88

#Will have to compare this model with auto.arima model to see best fit
#We'll check if the residual series is white noise
## We will reconfirm and do some test
resi_EU_Quantity <- local_pred_EU_consumer_Qty_smoothed-fitted(armafit_EU_consumer_Qty_smoothed)

adf.test(resi_EU_Quantity,alternative = "stationary") #According to this test, we do not reject the null hypothesis as  p-value = 0.2571 (p-value>0.05)
#It is not stationary
kpss.test(resi_EU_Quantity) #p-value = 0.1
#Here, we reject the null hypothesis as it is not stationary (p-value>0.05)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata2 <- EU_consumer_agg[43:48, c(1,3)]
timevals_out2 <- outdata2$Months

global_pred_EU_consumer_Qty_smoothed_out <- predict(lmfit_EU_consumer_Qty_smoothed,data.frame(Months =timevals_out2))

fcast_EU_consumer_Quantity <- global_pred_EU_consumer_Qty_smoothed_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec2 <- accuracy(fcast_EU_consumer_Quantity,outdata2[,2])[5]
MAPE_class_dec2 #29.72397

#Let's also plot the predictions along with original values (48 months), to
#get a visual feel of the fit

class_dec_pred2 <- c(ts(global_pred_EU_consumer_Qty_smoothed),ts(global_pred_EU_consumer_Qty_smoothed_out))
plot(EU_consumer_agg_Q , col = "black", main = "Original and Forecasted Values - EU consumer Demand")
lines(class_dec_pred2, col = "magenta")
#model not able to predict the last months as forecast shows a downward trend while original values show upward


#Consumer EU Quantity using AUTO.ARIMA 
autoarima2 <- auto.arima(EU_consumer_Quantity)
autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black", main= "EU Consumer - Quantity")
lines(fitted(autoarima2), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col = c("Black", "red"), lwd =2)
#It is an auto-regressive model with one differencing done (drift)
#AIC & BIC VALUES are much lower than ARMA model, S.E. looks minimal - showing confidence in this model
#Log likehood has increased compared to Arma model
#Auto Arima prediction hugs the original curve

# #ARIMA(0,0,0)(1,1,0)[12] with drift 
# 
# Coefficients:
#        sar1   drift
#      -0.5756  5.8296
#s.e.   0.1559  0.9601
# 
# sigma^2 estimated as 7469:  log likelihood=-177.72
# AIC=361.45   AICc=362.37   BIC=365.65


#Again, let's check if the residual series is white noise
resi_auto_arima2 <- EU_consumer_Quantity - fitted(autoarima2)

adf.test(resi_auto_arima2,alternative = "stationary") #adf test says that the residual series is not stationary (p-value = 0.1801)
kpss.test(resi_auto_arima2) #KPSS test says that the residual series is stationary (p-value = 0.1)

#Also, let's evaluate the model using MAPE
fcast_auto_arima2 <- forecast(autoarima2, n.ahead = 6) #Forecasting for next 6 month to evaluate

MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$mean,outdata2[,2])[5]
MAPE_auto_arima2 #17.61852 - Very good  compared to classical decomposition model

#Lastly, let's plot the predictions along with original values (48 months), to get a visual feel of the fit
auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$mean))
plot(EU_consumer_agg_Q, col = "black", main = "Original and Forecasted - EU consumer Demand")
lines(auto_arima_pred2, col = "blue")

############-------------------------------------------------------------------------#################

#APAC Consumer - Sales Forecast - Classical Decomposition
#-----------------
plot(APAC_consumer_Sale, main = "APAC Consumer Sales - Smooth time Series")
lines(APAC_consumer_Sale_smoothed, col="green", lwd=2)

#There is an upward trend plus some seasonal highs visible in the smoothed time series data 
#Since, it follows a curvy pattern and seasonal variation seems to be expanding over the months, 
#we can use the multiplicative model for forecasting, using classical decomposition method
#Seasonality being modeled using sinusoid function

lmfit_APAC_consumer_Sale_smoothed <- lm(Sales ~ sin(0.5*Months) * poly(Months,1) + cos(0.5*Months) * poly(Months,1) + Months, 
                                      data=APAC_consumer_Sale_smoothed_df)

global_pred_APAC_consumer_Sale_smoothed <- predict(lmfit_APAC_consumer_Sale_smoothed, Month=APAC_consumer_Total$Months)
summary(global_pred_APAC_consumer_Sale_smoothed)

lines(APAC_consumer_Total$Months, global_pred_APAC_consumer_Sale_smoothed, col='red', lwd=2)
labels <- c("Global Prediction")
cols <- c("Red")
legend("bottomright", labels, col=cols, lwd=2)

#Now, let's look at the locally predictable series to see if the above results are good enough or not
#We will model it as an ARMA series

local_pred_APAC_consumer_Sale_smoothed  <- APAC_consumer_Sale_smoothed - global_pred_APAC_consumer_Sale_smoothed
plot(local_pred_APAC_consumer_Sale_smoothed, col='green', type = "l", lwd = 2)
legend("bottomright", c("Local Prediction"), col= c("Green"), lwd=2) #it does'nt look flat. But may have a pattern

acf(local_pred_APAC_consumer_Sale_smoothed, main = "ACF for APAC_Consumer Sales") #There is 1 peak at 0. Rest look acceptable 
acf(local_pred_APAC_consumer_Sale_smoothed, type="partial", main = "PACF for APAC_Consumer Sales") #After 0.2 lag it is stationary

armafit_APAC_consumer_Sale_smoothed <- auto.arima(local_pred_APAC_consumer_Sale_smoothed)
tsdiag(armafit_APAC_consumer_Sale_smoothed)
armafit_APAC_consumer_Sale_smoothed 
# ARIMA(3,0,0) with zero mean 

# Coefficients:
#               ar1      ar2      ar3
#             0.3166  -0.0794  -0.6079
# s.e.        0.1189   0.1264   0.1162
# 
# sigma^2 estimated as 10163751:  log likelihood=-397.85
# AIC=803.69   AICc=804.78   BIC=810.64

#Will have to compare this model with auto.arima model to see best fit
#We'll check if the residual series is white noise
## We will reconfirm and do some test
resi_APAC_sales <- local_pred_APAC_consumer_Sale_smoothed-fitted(armafit_APAC_consumer_Sale_smoothed)

adf.test(resi_APAC_sales,alternative = "stationary") #According to this test, we do not reject the null hypothesis as p-value = 0.1539 (p-value>0.05)
#It is not stationary
kpss.test(resi_APAC_sales) #p-value = 0.1
#Here, we reject the null hypothesis as it is not stationary (p-value>0.05)

## Both KPSS and adf says that it is not white noise 

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata3 <- APAC_consumer_agg[43:48, 1:2]
timevals_out3 <- outdata3$Months

global_pred_APAC_consumer_Sale_smoothed_out <- predict(lmfit_APAC_consumer_Sale_smoothed,data.frame(Months =timevals_out3))

fcast_APAC_consumer_Sales <- global_pred_APAC_consumer_Sale_smoothed_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec3 <- accuracy(fcast_APAC_consumer_Sales,outdata3[,2])[5]
MAPE_class_dec3 #26.89676

#Let's also plot the predictions along with original values (48 months), to
#get a visual feel of the fit

class_dec_pred3 <- c(ts(global_pred_APAC_consumer_Sale_smoothed),ts(global_pred_APAC_consumer_Sale_smoothed_out))
plot(APAC_consumer_agg_S , col = "black", main = "Original and Forecasted Values - APAC consumer Sales")
lines(class_dec_pred3, col = "magenta")
#model not able to predict as towards the end it forecasts a downward trend


#Consumer APAC Sales using AUTO.ARIMA 
autoarima3 <- auto.arima(APAC_consumer_Sale)
autoarima3
tsdiag(autoarima3)
plot(autoarima3$x, col="black", main= "APAC Consumer - Sales")
lines(fitted(autoarima3), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col = c("Black", "red"), lwd =2)

#It is an auto-regressive model with one differencing done (drift)
#AIC & BIC VALUES are much lower than ARMA model showing confidence in this model
#Auto Arima prediction hugs the original curve better
#log likelihood is higher compared to Arma model

#ARIMA(0,0,0)(1,1,0)[12] with drift 

# Coefficients:
#         sar1     drift
#        -0.6038  649.9106
# s.e.    0.1537   104.3881
# 
# sigma^2 estimated as 90556712:  log likelihood=-319.08
# AIC=644.15   AICc=645.08   BIC=648.36


#Again, let's check if the residual series is white noise
resi_auto_arima3 <- APAC_consumer_Sale - fitted(autoarima3)

adf.test(resi_auto_arima3,alternative = "stationary") #adf test says that the residual series is not stationary (p-value = 0.0571)
kpss.test(resi_auto_arima3) #KPSS test says that the residual series is not stationary (p-value = 0.1)

#Also, let's evaluate the model using MAPE
fcast_auto_arima3 <- forecast(autoarima3, n.ahead = 6) #Forecasting for next 6 month to evaluate

MAPE_auto_arima3 <- accuracy(fcast_auto_arima3$mean,outdata3[,2])[5]
MAPE_auto_arima3 #11.77618 - Very good  compared to classical decomposition model

#Lastly, let's plot the predictions along with original values (48 months), to get a visual feel of the fit
auto_arima_pred3 <- c(fitted(autoarima3),ts(fcast_auto_arima3$mean))
plot(APAC_consumer_agg_S, col = "black", main = "Original and Forecasted - APAC consumer Sales")
lines(auto_arima_pred3, col = "blue")

#---------------------------------------------------------------------------------------------

#APAC Consumer - Quantity Forecast - Classical Decomposition
#-----------------
plot(APAC_consumer_Quantity, main = "APAC Consumer Quantity - Smooth time Series")
lines(APAC_consumer_Qty_smoothed, col="green", lwd=2)

#There is an upward trend plus some seasonal highs visible in the smoothed time series data 
#Since, it follows a curvy pattern and seasonal variation seems to be expanding over the months, 
#we can use the multiplicative model for forecasting, using classical decomposition method#Seasonality being modeled using sinusoid function

lmfit_APAC_consumer_Qty_smoothed <- lm(Quantity ~ sin(0.5*Months) * poly(Months,1) + cos(0.5*Months) * poly(Months,1) + Months, 
                                     data=APAC_consumer_Qty_smoothed_df)

global_pred_APAC_consumer_Qty_smoothed <- predict(lmfit_APAC_consumer_Qty_smoothed, Month=APAC_consumer_Total$Months)
summary(global_pred_APAC_consumer_Qty_smoothed)

lines(APAC_consumer_Total$Months, global_pred_APAC_consumer_Qty_smoothed, col='red', lwd=2)
labels <- c("Global Prediction")
cols <- c("Red")
legend("bottomright", labels, col=cols, lwd=2)

#Now, let's look at the locally predictable series to see if the above results are good enough or not
#We will model it as an ARMA series

local_pred_APAC_consumer_Qty_smoothed  <- APAC_consumer_Qty_smoothed - global_pred_APAC_consumer_Qty_smoothed
plot(local_pred_APAC_consumer_Qty_smoothed, col='green', type = "l", lwd = 2)
legend("bottomright", c("Local Prediction"), col= c("Green"), lwd=2) #it looks like it has a pattern
#cannot draw any pattern or trend from this

acf(local_pred_APAC_consumer_Qty_smoothed, main = "ACF for APAC_Consumer Quantity") #There is 1 peak at 0. Rest look acceptable 
acf(local_pred_APAC_consumer_Qty_smoothed, type="partial", main = "PACF for APAC_Consumer Quantity") #After 0.3 lag it is stationary

armafit_APAC_consumer_Qty_smoothed <- auto.arima(local_pred_APAC_consumer_Qty_smoothed)
tsdiag(armafit_APAC_consumer_Qty_smoothed)
armafit_APAC_consumer_Qty_smoothed 
#with 1 drift
# Coefficients:
#      ARIMA(1,0,0)(1,1,0)[12] 

# Coefficients:
#       ar1     sar1
#      0.3840  -0.5802
#     s.e.  0.1658   0.1588
# 
# sigma^2 estimated as 1416:  log likelihood=-152.91
# AIC=311.83   AICc=312.75   BIC=316.03

#Will have to compare this model with auto.arima model to see best fit
#We'll check if the residual series is white noise
## We will reconfirm and do some test
resi_APAC_Quantity <- local_pred_APAC_consumer_Qty_smoothed-fitted(armafit_APAC_consumer_Qty_smoothed)

adf.test(resi_APAC_Quantity,alternative = "stationary") #According to this test, we do not reject the null hypothesis as  p-value = 0.08836 (p-value>0.05)
#It is not stationary
kpss.test(resi_APAC_Quantity) #p-value = 0.1
#Here, we reject the null hypothesis as it is not stationary (p-value>0.05)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata4 <- APAC_consumer_agg[43:48, c(1,3)]
timevals_out4 <- outdata4$Months

global_pred_APAC_consumer_Qty_smoothed_out <- predict(lmfit_APAC_consumer_Qty_smoothed,data.frame(Months =timevals_out4))

fcast_APAC_consumer_Quantity <- global_pred_APAC_consumer_Qty_smoothed_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec4 <- accuracy(fcast_APAC_consumer_Quantity,outdata4[,2])[5]
MAPE_class_dec4 #29.56586

#Let's also plot the predictions along with original values (48 months), to
#get a visual feel of the fit

class_dec_pred4 <- c(ts(global_pred_APAC_consumer_Qty_smoothed),ts(global_pred_APAC_consumer_Qty_smoothed_out))
plot(APAC_consumer_agg_Q , col = "black", main = "Original and Forecasted Values - APAC consumer Demand")
lines(class_dec_pred4, col = "magenta")
#model not able to predict as there is a downward trend towards the last few months


#Consumer APAC Quantity using AUTO.ARIMA 
autoarima4 <- auto.arima(APAC_consumer_Quantity)
autoarima4
tsdiag(autoarima4)
plot(autoarima4$x, col="black", main= "APAC Consumer - Quantity")
lines(fitted(autoarima4), col="red")
legend("bottomright", c("Original", "ARIMA Fit"), col = c("Black", "red"), lwd =2)
#It is an auto-regressive model with one differencing done (drift)
#AIC & BIC VALUES are higher than ARMA model, showing less confidence in this model
#Log likehood has decreased compared to Arma model
#Auto Arima prediction hugs the original curve

# #ARIMA(0,0,0)(1,1,0)[12] with drift 

# Coefficients:
#      sar1   drift
#     -0.5319  7.1287
#s.e.   0.1781  1.0580
# 
# sigma^2 estimated as 8726:  log likelihood=-179.64
# AIC=365.28   AICc=366.2   BIC=369.48


#Again, let's check if the residual series is white noise
resi_auto_arima4 <- APAC_consumer_Quantity - fitted(autoarima4)

adf.test(resi_auto_arima4,alternative = "stationary") #adf test says that the residual series is not stationary (p-value = 0.1573)
kpss.test(resi_auto_arima4) #KPSS test says that the residual series is not stationary (p-value = 0.1)

#Also, let's evaluate the model using MAPE
fcast_auto_arima4 <- forecast(autoarima4, n.ahead = 6) #Forecasting for next 6 month to evaluate

MAPE_auto_arima4 <- accuracy(fcast_auto_arima4$mean,outdata4[,2])[5]
MAPE_auto_arima4 #13.36779 - Very good  compared to classical decomposition model

#Lastly, let's plot the predictions along with original values (48 months), to get a visual feel of the fit
auto_arima_pred4 <- c(fitted(autoarima4),ts(fcast_auto_arima4$mean))
plot(APAC_consumer_agg_Q, col = "black", main = "Original and Forecasted - APAC consumer Demand")
lines(auto_arima_pred4, col = "blue")




