#############################################################################################################################################################
#                                                     :::::::: Data Understanding ::::::::
#############################################################################################################################################################

library(cowplot)
library(car)
library(dplyr)
library(DataCombine)
library(ggplot2)
library(readxl)
library(imputeTS)
library(ggthemes)
library(zoo)
library(MASS)
library(lubridate)
library(DAAG)
library(scales)


### Importing Data
salesData <- read.csv("ConsumerElectronics.csv", stringsAsFactors = F)
mediaInvestmentData <- read_excel("Media data and other information.xlsx", sheet = "Media Investment", skip = 2)
nps <- read_excel("Media data and other information.xlsx", sheet = "Monthly NPS Score", skip =2, col_names = FALSE)

str(salesData)   #1648824 obs. of  20 variables





################################################################################################################################################################
#                                                     :::::::: Data Preparation ::::::::
################################################################################################################################################################


### Converting "mediaInvestmentData" dataframe into weekly level spends on advertising channels (originally it hold monthly investment)

## computing Month, week, and no.of days per week (month, week)
days <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=days, Month = month(days), week = week(days),nweek = rep(1,length(days)))
weekdays <- data.frame(weekdays %>% group_by(Month,week) %>% summarise(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7

## Replacing NA values
mediaInvestmentData[is.na(mediaInvestmentData)] <- 0

## converting montly spend to weekly
mediaInvestmentData <- cbind(Month=mediaInvestmentData[,c(2)], mediaInvestmentData[,-c(1,2)]/4.30)

## Add weekly information
mediaInvestmentDataPerWeek <- merge(weekdays,mediaInvestmentData, by='Month', all.x = TRUE)


## Converting media Investment at weekly granularity
# pro-rate weekly investment as per ratio of its days span over adjacent months
mediaInvestmentDataPerWeekLatest <- data.frame(mediaInvestmentDataPerWeek %>% group_by(week) %>%
                                                 summarise(Total_Investment = sum(`Total Investment`*fracDays),
                                                           TV = sum(TV*fracDays), Digital=sum(Digital*fracDays),Sponsorship = sum(Sponsorship*fracDays),
                                                           Content_Marketing = sum(`Content Marketing`*fracDays),Online_Marketing = sum(`Online marketing`*fracDays),
                                                           Affiliates = sum(Affiliates*fracDays), SEM = sum(SEM*fracDays), Radio = sum(Radio*fracDays),
                                                           Other = sum(Other*fracDays)))


## Converting media investement into crores
mediaInvestmentDataPerWeekLatest[,2:11] <- mediaInvestmentDataPerWeekLatest[,2:11]*10000000



### Converting 'nps' dataframe into per week level
## Re-naming nps dataframe column name
colnames(nps) <- c(0:12)

## adding two rows in nps dataframe to add 'Year' and 'Month'
nps <- rbind(nps, c(0,2015,2015,2015,2015,2015, 2015,2016,2016,2016,2016,2016,2016), c(0,7,8,9,10,11,12,1,2,3,4,5,6))

## Setting first row of first column to 0
nps[1,1] <- 0

## Taking transpose of nps dataframe (i.e. converting rows to column and vice-versa)
nps = setNames(data.frame(t(nps[,-1])), nps[,1])

## Re-naming header of nps dataframe
colnames(nps) <- c("NPS Score","Year","Month")

## Rounding off numerical value to one place of decimal
nps[,'NPS Score']=round(nps[,'NPS Score'],1)



### Check whether data is from "July 2015 to June 2016"
table(salesData$Year, salesData$Month)
# Data is present for 2015- May & June and 2016- July


### Fiilerting out data
salesData_cleaned <- subset(salesData, !(Month ==5 & Year==2015 | Month ==6 & Year==2015 | Month ==7 & Year==2016))
table(salesData_cleaned$Year, salesData_cleaned$Month)

### Filtering out duplicate records
salesData_cleaned <- salesData_cleaned[!duplicated(salesData_cleaned[c(1,5,6,8)]),]


### Filtering out where Product_MRP is '0'
salesData_cleaned <- subset(salesData_cleaned, product_mrp != 0)


### Replacing "gmv" with product_mrp*units wherever gmv=0 (assuming products were sold without any discount)
row_no <- which(salesData_cleaned$gmv==0)

for(i in 1:length(row_no)){
  salesData_cleaned$gmv[row_no[i]] <- (salesData_cleaned$product_mrp[row_no[i]])*(salesData_cleaned$units[row_no[i]])
}


### Filtering out records where "gmv" is greater than 'product_mrp*units' (as we can't charge more than MRP)
salesData_cleaned <- subset(salesData_cleaned, (product_mrp*units) >= gmv)

### Converting order_date to "Date" format
salesData_cleaned$order_date <- as.Date(salesData_cleaned$order_date)

### Converted order_date in to Date format and creted as new column in order to perform analysis based on Year and Month
salesData_cleaned$month_date <- as.Date(cut(salesData_cleaned$order_date,breaks = "month"))

### Created a column "wday" from order_date to find day of a week
salesData_cleaned$wday <- weekdays(salesData_cleaned$order_date)

### Created a column "week" from order_date (it gives week number i.e. 1, 2 etc.)
salesData_cleaned$week <- as.numeric(strftime(salesData_cleaned$order_date, format = "%V"))

### Created a column "week_date" from order_date (it gives starting date of week)
salesData_cleaned$Week_date <- as.Date(cut(salesData_cleaned$order_date,breaks = "week", start.on.monday = FALSE))  # changes weekly break point to Sunday


###  Declaring a vector "date" to store dates of Special Sale Calender as provided in 'Media data and other information.xlsx'
date <- as.Date(c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17",
                  "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26","2015-12-27",
                  "2015-12-28","2015-12-29","2015-12-30","2015-12-31","2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01",
                  "2016-02-02","2016-02-14","2016-02-15","2016-02-20","2016-02-21","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27"))


##  Declaring a column whether a order is placed on special sale day or not
salesData_cleaned$isASaleDayOrNot <- ifelse(salesData_cleaned$order_date %in% date, "Y", "N")

##  Declaring one more column "SpecialSaleDay" which stores which special day it was (like Diwali, Eid etc.)
salesData_cleaned$SpecialSaleDay='Regular Day'

salesData_cleaned <- within(salesData_cleaned, {
  SpecialSaleDay[order_date  %in% (date[1:2])]='Eid & Rathayatra'
  SpecialSaleDay[order_date  %in% (date[3:5])]='Independence Day'
  SpecialSaleDay[order_date  %in% (date[6:8])]='Rakshabandhan'
  SpecialSaleDay[order_date  %in% (date[9:11])]='Daussera'
  SpecialSaleDay[order_date  %in% (date[12:19])]='Diwali'
  SpecialSaleDay[order_date  %in% (date[20:29])]='Christmas & New Year'
  SpecialSaleDay[order_date  %in% (date[30:32])]='Republic Day'
  SpecialSaleDay[order_date  %in% (date[33:34])]='BED'
  SpecialSaleDay[order_date  %in% (date[35:36])]='Valentine Day'
  SpecialSaleDay[order_date  %in% (date[37:38])]='FHSD'
  SpecialSaleDay[order_date  %in% (date[39:41])]='BSD'
  SpecialSaleDay[order_date  %in% (date[42:44])]='Pacman'
})


##  Declaring a dataframe which holds number of holidays per week

holidayData <- date   #Coverting date vector into Date format
week <- strftime(holidayData, format = "%V")   #Extracting weeks out of date
Year <- format(as.POSIXct(holidayData, format="%Y-%m-%d"),"%Y")  #Extracting Year out of date

holidayDataInfo <- data.frame(cbind(Year,week))   # Declaring a dataframe to hold holiday details
holidayDataInfo$holidayData <- holidayData
holidayDataInfo$holidayCountInfo <- 1
holidayDataInfo <- aggregate(holidayCountInfo~Year+week, holidayDataInfo, sum)   #Aggregating holidays couns based on week


### Check for "units" column in case of any outlier
quantile(salesData_cleaned$units, seq(0,1,.001))  #99.9% of orders/records have <=4 units
table(salesData_cleaned$units)

# Capping maximum order to 4 and filtering out records having units>4 as outlier
salesData_cleaned <- subset(salesData_cleaned, units <= 4)


### Check of NA or any invalid valid values in "deliverybdays" column
table(salesData_cleaned$deliverybdays)  #so many \\N values and some negative values also

# Assining 0 to values where deliverycdays are "\\N" and negative values
salesData_cleaned$deliverybdays[salesData_cleaned$deliverybdays == "\\N" | salesData_cleaned$deliverybdays < 0] <- 0


### Check of NA or any invalid valid values in "deliverycdays" column
table(salesData_cleaned$deliverycdays)  #so many \\N values and some negative values also

# Assining 0 to values where deliverycdays are "\\N" and negative values
salesData_cleaned$deliverycdays[salesData_cleaned$deliverycdays == "\\N" | salesData_cleaned$deliverycdays < 0] <- 0


### Analyzing "product_procurement_sla" column
table(salesData_cleaned$product_procurement_sla) #67976 negative values

# Assining 0 to values where product_procurement_sla having negative values
salesData_cleaned$product_procurement_sla[salesData_cleaned$product_procurement_sla < 0] <- 0

### Check for range of "sla" column
quantile(salesData_cleaned$sla, seq(0,1,.001)) ##99.9% of data has max sla of 17, so we'll cap as 17 max
salesData_cleaned$sla[salesData_cleaned$sla > 17] <- 17  ##Capping max sla as 17

### Check for missing('NA') value
sapply(salesData_cleaned, function(x) sum(is.na(x)))  #No NA values
#fsn_id                      order_date                            Year
#                              0                               0                               0
#                          Month                        order_id                   order_item_id
#                              0                               0                               0
#                            gmv                           units                   deliverybdays
#                              0                               0                               0
#                  deliverycdays      s1_fact.order_payment_type                             sla
#                              0                               0                               0
#                        cust_id                         pincode product_analytic_super_category
#                              0                               0                               0
#      product_analytic_category   product_analytic_sub_category       product_analytic_vertical
#                              0                               0                               0
#                    product_mrp         product_procurement_sla                      month_date
#                              0                               0                               0
#                           wday                            week                       Week_date
#                              0                               0                               0
#                isASaleDayOrNot                SpecialSaleDay
#                              0                               0


### Discarding columns, which is not important for analysis
salesData_cleaned <- salesData_cleaned[,-c(1,2,5,6,13:16)]

### Merging Media investment and nps data frame with salesData
salesData_cleaned <- merge(salesData_cleaned, mediaInvestmentDataPerWeekLatest, by = c("week"), all.x = TRUE)
salesData_cleaned <- merge(salesData_cleaned, nps, by = c("Year", "Month"), all.x = TRUE)
salesData_cleaned <- merge(salesData_cleaned, holidayDataInfo, by = c("Year", "week"), all.x = TRUE)

# Setting holiday count to where NA's are present (means no holidays for those weeks)
salesData_cleaned$holidayCountInfo[which(is.na(salesData_cleaned$holidayCountInfo))] <- 0


### Further filtering data for product sub-categories- camera accessory, home audio and gaming accessory.

salesData_CameraAccessory <- subset(salesData_cleaned, product_analytic_sub_category == "CameraAccessory")
salesData_HomeAudio <- subset(salesData_cleaned, product_analytic_sub_category == "HomeAudio")
salesData_GamingAccessory <- subset(salesData_cleaned, product_analytic_sub_category == "GamingAccessory")






################################################################################################################################################################
#                                             :::::::: EDA
#                                             Exploratory Data Analysis::::::::
################################################################################################################################################################

### Sum of Monthly orders
perMonthSoldItems <- aggregate(units~month_date, salesData_cleaned, sum, na.rm=TRUE)

# Plotting Bar graph showing Monthly orders
ggplot(perMonthSoldItems, aes(x=month_date, y=units)) + geom_bar(stat = "identity", fill="blue", width = 15) +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_minimal() + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months")) +
  labs(x="Months",y="No. of Sold Items") + ggtitle("Monthly Units Sold") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))

# Plotting Line graph showing Monthly orders
ggplot(perMonthSoldItems, aes(x=month_date, y=units)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + theme_bw()

### Sum of Weekly orders
peerWeekSoldItems <- aggregate(units~Week_date, salesData_cleaned, sum, na.rm=TRUE)

# Plotting Bar graph showing weekly orders
ggplot(peerWeekSoldItems, aes(x=Week_date, y=units)) + geom_bar(stat = "identity", fill="blue") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc() + scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  labs(x="Weeks",y="No. of Sold Items") + ggtitle("Weekly Units Sold") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))

# Plotting Line graph showing weekly orders
ggplot(peerWeekSoldItems, aes(x=Week_date, y=units)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="No. of Sold Items") + ggtitle("Weekly Units Sold") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


#*********************************************************************************************************************************************************#
### Sum of orders by product sub-category
ProductSoldItems <- aggregate(units~product_analytic_sub_category, salesData_cleaned, sum, na.rm=TRUE)

ProductSoldItems <- subset(ProductSoldItems, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" |
                             product_analytic_sub_category == "GamingAccessory")

# Plotting Bar graph showing units sold for different product sub-category
ggplot(ProductSoldItems, aes(x=as.factor(product_analytic_sub_category), y=units,fill=as.factor(product_analytic_sub_category))) + geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Sub-Category",y="No. of Sold Items") +
  ggtitle("Products Units Sold") + scale_fill_manual("Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3"))
theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


#*********************************************************************************************************************************************************#
### Sum of Monthly orders by product sub-category
perMonthProductSoldItems <- aggregate(units~month_date + product_analytic_sub_category, salesData_cleaned, sum, na.rm=TRUE)

perMonthProductSoldItems <- subset(perMonthProductSoldItems, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" |
                                     product_analytic_sub_category == "GamingAccessory")

# Plotting Bar graph showing units sold for different product sub-category
ggplot(perMonthProductSoldItems, aes(x=month_date,y=units, fill=as.factor(product_analytic_sub_category))) + geom_bar(stat="identity",position = "dodge", width = 15) +
  theme_hc(base_size = 19, base_family = "sans") + labs(x="Months",y="No. of Sold Items") +
  ggtitle("Monthly Items Sold") + scale_fill_manual("Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3")) +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months"))

### Sum of weekly orders by product sub-category
perWeekProductSoldItems <- aggregate(units~Week_date + product_analytic_sub_category, salesData_cleaned, sum, na.rm=TRUE)

perWeekProductSoldItems <- subset(perWeekProductSoldItems, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" |
                                    product_analytic_sub_category == "GamingAccessory")

# Plotting Bar graph showing units sold for different product sub-category weekly
ggplot(perWeekProductSoldItems, aes(x=Week_date,y=units, fill=as.factor(product_analytic_sub_category))) + geom_bar(stat="identity",position = "stack", width = 4) +
  theme_hc(base_size = 19, base_family = "sans") + labs(x="Weeks",y="No. of Sold Items") +
  ggtitle("Weekly Units Sold by Product Sub-Categories") + scale_fill_manual("Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3")) +
  theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))


# Plotting Line graph showing weekly orders
ggplot(perWeekProductSoldItems, aes(x=Week_date,y=units, group=as.factor(product_analytic_sub_category))) + geom_line(size=1.5, aes(color = product_analytic_sub_category)) +
  scale_color_manual("Sub-Category: ", values = c("CameraAccessory" = "green4", "GamingAccessory" = "yellow", "HomeAudio" = "red3")) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) + theme_bw() + labs(x="Weeks",y="No. of Sold Items") + ggtitle("Weekly Units Sold by Product Sub-Categories") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


#*********************************************************************************************************************************************************#
### Sum of units sold by top 10 Product Vertical
productVerticalSoldItems <- subset(salesData_cleaned, product_analytic_sub_category == "CameraAccessory" | product_analytic_sub_category == "HomeAudio" |
                                     product_analytic_sub_category == "GamingAccessory")

productVerticalSoldItems <- aggregate(units~product_analytic_vertical, productVerticalSoldItems, sum, na.rm=TRUE)

tenTopProductVertical <- productVerticalSoldItems[order(productVerticalSoldItems$units, decreasing = TRUE),][1:10,]

# Plotting Bar graph showing units sold for top 10 Product Vertical
ggplot(tenTopProductVertical, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Product Vertical",y="No. of Sold Items") +
  ggtitle("Product Vertical Units Sold") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none")


#*********************************************************************************************************************************************************#
### Sum of Product units sold on weekdays/weekends
weekDaySoldUnits <- aggregate(units~wday, salesData_cleaned, sum, na.rm=TRUE)

weekDaySoldUnits$wday <- factor(weekDaySoldUnits$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plotting Bar graph showing units sold for top 10 Product Vertical
ggplot(weekDaySoldUnits, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Weekdays",y="No. of Sold Items") +
  ggtitle("Units Sold on Weekdays/Weekend") + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5),legend.position="none")


#*********************************************************************************************************************************************************#
### Sum of Monthly orders by different payment types
perMonthSoldItems_ByPaymentType <- aggregate(units~month_date + s1_fact.order_payment_type, salesData_cleaned, sum, na.rm=TRUE)

# Plotting Bar graph showing units sold through different payment type
ggplot(perMonthSoldItems_ByPaymentType, aes(x=month_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "dodge", width = 17) +
  theme_hc(base_size = 19, base_family = "sans") + labs(x="Months",y="No. of Sold Items") +
  ggtitle("Monthly Items Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
  theme(legend.justification="center", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("months"))


### Sum of Weekly orders by different payment types
peerWeekSoldItems_ByPaymentType <- aggregate(units~Week_date + s1_fact.order_payment_type, salesData_cleaned, sum, na.rm=TRUE)

# Plotting Bar graph showing units sold through different payment type
ggplot(peerWeekSoldItems_ByPaymentType, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) +
  theme_hc(base_size = 19, base_family = "sans") + labs(x="Weeks",y="No. of Sold Items") +
  ggtitle("Weekly Items Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
  theme(legend.justification="center" , axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))



#*********************************************************************************************************************************************************#

### Releasing memory for faster processing.
gc()

### Weekly spends on different marketing channels [or Media Investment]
perWeekAdSpent <- salesData_cleaned[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
perWeekAdSpent$week <- c(1:53)
perWeekAdSpent <- perWeekAdSpent[,c(11,2:10)]

# Plotting Line graph amount spent on different marketing channels
plots <- list()  # new empty list

for (i in 2:10) local({
  i <- i
  p0 <- ggplot(perWeekAdSpent,aes(x=perWeekAdSpent[,1],y=perWeekAdSpent[,i])) +
    geom_line(size=1, color = "blue") + geom_point() + theme_bw() +
    labs(x="Weeks",y= paste0("Spend on ", colnames(perWeekAdSpent[i])," Ads"))
  
  plots[[i-1]] <<- p0  # add each plot into plot list
  
})

# Plotting all graphs
# Please wait...
plot_grid(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]],
          plots[[7]],plots[[8]],plots[[9]],align ="h")






################################################################################################################################################################
#                                     :::::::: Exploratory Data Analysis based on sub-categories ::::::::
################################################################################################################################################################


#***************************************************************** CameraAccessory ****************************************************************************#

### Weekly Gross Merchandise Value
cameraAccessory_perWeek_gmv <- aggregate(gmv~Week_date, salesData_CameraAccessory, sum, na.rm=TRUE)

# Plotting Line graph showing weekly Gross Merchandise Value
ggplot(cameraAccessory_perWeek_gmv, aes(x=Week_date, y=gmv)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("CameraAccessory - Weekly Gross Merchandise Value") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Weekly units sold
cameraAccessory_peerWeekSoldItems <- aggregate(units~Week_date, salesData_CameraAccessory, sum, na.rm=TRUE)

# Plotting Line graph showing weekly orders
ggplot(cameraAccessory_peerWeekSoldItems, aes(x=Week_date, y=units)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="No. of Sold Items") + ggtitle("CameraAccessory - Weekly Units Sold") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Sum of Weekly orders by different payment types
cameraAccessory_peerWeekSoldItems_ByPaymentType <- aggregate(units~Week_date + s1_fact.order_payment_type, salesData_CameraAccessory, sum, na.rm=TRUE)

# Plotting Bar graph showing units sold through different payment types
ggplot(cameraAccessory_peerWeekSoldItems_ByPaymentType, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) +
  theme_hc(base_size = 19, base_family = "sans") + labs(x="Weeks",y="No. of Sold Items") +
  ggtitle("CameraAccessory - Weekly Items Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
  theme(legend.justification="center", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))


##############################################################################################
### Sum of units sold for top 10 CameraAccessory Products
cameraAccessory_ProductSoldItems <- aggregate(units~product_analytic_vertical, salesData_CameraAccessory, sum, na.rm=TRUE)

cameraAccessory_TopTenProducts <- cameraAccessory_ProductSoldItems[order(cameraAccessory_ProductSoldItems$units, decreasing = TRUE),][1:10,]

# Ordering top 10 products based on number of units sold for display
cameraAccessory_TopTenProducts$product_analytic_vertical <- factor(cameraAccessory_TopTenProducts$product_analytic_vertical, levels = cameraAccessory_TopTenProducts$product_analytic_vertical[order(-cameraAccessory_TopTenProducts$units)])

# Plotting Bar graph showing units sold for top 10 Products
ggplot(cameraAccessory_TopTenProducts, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Product Vertical",y="No. of Sold Items") +
  ggtitle("CameraAccessory - Items Sold") + theme(legend.position="none", axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45, vjust = 0.6),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Weekly average mrp of products sold
cameraAccessory_perWeek_avg_product_mrp <- aggregate(product_mrp~Week_date, salesData_CameraAccessory, mean, na.rm=TRUE)

# Plotting Line graph showing weekly average mrp of products sold
ggplot(cameraAccessory_perWeek_avg_product_mrp, aes(x=Week_date, y=product_mrp)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="Average Product MRP") + ggtitle("CameraAccessory - Weekly Average Product MRP") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Sum of Product units sold on weekdays/weekends
cameraAccessory_weekDaySoldUnits <- aggregate(units~wday, salesData_CameraAccessory, sum, na.rm=TRUE)

cameraAccessory_weekDaySoldUnits$wday <- factor(cameraAccessory_weekDaySoldUnits$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plotting Bar graph showing units sold on weekdays/weekends
ggplot(cameraAccessory_weekDaySoldUnits, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Weekdays",y="No. of Sold Items") +
  ggtitle("CameraAccessory - Units Sold on Weekdays/Weekend") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Units sold of special sale day [i.e. on different holidays]
cameraAccessory_SpecialDaySoldItems <- subset(salesData_CameraAccessory, SpecialSaleDay != "Regular Day")

cameraAccessory_SpecialDaySoldItems <- aggregate(units~SpecialSaleDay, cameraAccessory_SpecialDaySoldItems, sum, na.rm=TRUE)

# Ordering special sales day based on number of units sold
cameraAccessory_SpecialDaySoldItems$SpecialSaleDay <- factor(cameraAccessory_SpecialDaySoldItems$SpecialSaleDay, levels = cameraAccessory_SpecialDaySoldItems$SpecialSaleDay[order(-cameraAccessory_SpecialDaySoldItems$units)])

# Plotting Bar graph showing units sold on different holiday seaseons
ggplot(cameraAccessory_SpecialDaySoldItems, aes(x=as.factor(SpecialSaleDay), y=units, fill = as.factor(SpecialSaleDay))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Paired") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Special Sale Days",y="No. of Sold Items") +
  ggtitle("CameraAccessory - Units Sold on Special Sale Day") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Releasing memory for faster processing.
gc()

### Weekly spends on different marketing channels [or Media Investment]
cameraAccessory_perWeekAdSpent <- salesData_CameraAccessory[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
cameraAccessory_perWeekAdSpent$week <- c(1:52)
cameraAccessory_perWeekAdSpent <- cameraAccessory_perWeekAdSpent[,c(11,2:10)]

# Plotting Line graph amount spent on different marketing channels
cameraAccessory_plots <- list()  # new empty list
for (i in 2:10) local({
  i <- i
  p1 <- ggplot(cameraAccessory_perWeekAdSpent,aes(x=cameraAccessory_perWeekAdSpent[,1],y=cameraAccessory_perWeekAdSpent[,i])) +
    geom_line(size=1, color = "blue") + geom_point() + theme_bw() +
    labs(x="Weeks",y= paste0("Spend on ", colnames(cameraAccessory_perWeekAdSpent[i])," Ads"))
  
  cameraAccessory_plots[[i-1]] <<- p1  # add each plot into plot list
  
})

# Plotting all graphs
# Please wait...
plot_grid(cameraAccessory_plots[[1]],cameraAccessory_plots[[2]],cameraAccessory_plots[[3]],cameraAccessory_plots[[4]],cameraAccessory_plots[[5]],cameraAccessory_plots[[6]],
          cameraAccessory_plots[[7]],cameraAccessory_plots[[8]],cameraAccessory_plots[[9]],align ="h")




#***************************************************************** HomeAudio ****************************************************************************#

### Weekly Gross Merchandise Value
homeAudio_perWeek_gmv <- aggregate(gmv~Week_date, salesData_HomeAudio, sum, na.rm=TRUE)

# Plotting Line graph showing weekly Gross Merchandise Value
ggplot(homeAudio_perWeek_gmv, aes(x=Week_date, y=gmv)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("HomeAudio - Weekly Gross Merchandise Value") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Weekly units sold
homeAudio_peerWeekSoldItems <- aggregate(units~Week_date, salesData_HomeAudio, sum, na.rm=TRUE)

# Plotting Line graph showing weekly orders
ggplot(homeAudio_peerWeekSoldItems, aes(x=Week_date, y=units)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="No. of Sold Items") + ggtitle("HomeAudio - Weekly Units Sold") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Sum of Weekly orders by different payment types
homeAudio_peerWeekSoldItems_ByPaymentType <- aggregate(units~Week_date + s1_fact.order_payment_type, salesData_HomeAudio, sum, na.rm=TRUE)

# Plotting Bar graph showing units sold through different payment types
ggplot(homeAudio_peerWeekSoldItems_ByPaymentType, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) +
  theme_hc(base_size = 19, base_family = "sans") + labs(x="Weeks",y="No. of Sold Items") +
  ggtitle("HomeAudio - Weekly Items Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
  theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))


##############################################################################################
### Sum of units sold for top 10 CameraAccessory Products
homeAudio_ProductSoldItems <- aggregate(units~product_analytic_vertical, salesData_HomeAudio, sum, na.rm=TRUE)

homeAudio_TopTenProducts <- homeAudio_ProductSoldItems[order(homeAudio_ProductSoldItems$units, decreasing = TRUE),][1:10,]

# Ordering top 10 products based on number of units sold for display
homeAudio_TopTenProducts$product_analytic_vertical <- factor(homeAudio_TopTenProducts$product_analytic_vertical, levels = homeAudio_TopTenProducts$product_analytic_vertical[order(-homeAudio_TopTenProducts$units)])

# Plotting Bar graph showing units sold for top 10 Products
ggplot(homeAudio_TopTenProducts, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Product Vertical",y="No. of Sold Items") +
  ggtitle("HomeAudio - Items Sold") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Weekly average mrp of products sold
homeAudio_perWeek_avg_product_mrp <- aggregate(product_mrp~Week_date, salesData_HomeAudio, mean, na.rm=TRUE)

# Plotting Line graph showing weekly average mrp of products sold
ggplot(homeAudio_perWeek_avg_product_mrp, aes(x=Week_date, y=product_mrp)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="Average Product MRP") + ggtitle("HomeAudio - Weekly Average Product MRP") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Sum of Product units sold on weekdays/weekends
homeAudio_weekDaySoldUnits <- aggregate(units~wday, salesData_HomeAudio, sum, na.rm=TRUE)

homeAudio_weekDaySoldUnits$wday <- factor(homeAudio_weekDaySoldUnits$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plotting Bar graph showing units sold on weekdays/weekends
ggplot(homeAudio_weekDaySoldUnits, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Weekdays",y="No. of Sold Items") +
  ggtitle("HomeAudio - Units Sold on Weekdays/Weekend") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Units sold of special sale day [i.e. on different holidays]
homeAudio_SpecialDaySoldItems <- subset(salesData_HomeAudio, SpecialSaleDay != "Regular Day")

homeAudio_SpecialDaySoldItems <- aggregate(units~SpecialSaleDay, homeAudio_SpecialDaySoldItems, sum, na.rm=TRUE)

# Ordering special sales day based on number of units sold
homeAudio_SpecialDaySoldItems$SpecialSaleDay <- factor(homeAudio_SpecialDaySoldItems$SpecialSaleDay, levels = homeAudio_SpecialDaySoldItems$SpecialSaleDay[order(-homeAudio_SpecialDaySoldItems$units)])

# Plotting Bar graph showing units sold on different holiday seaseons
ggplot(homeAudio_SpecialDaySoldItems, aes(x=as.factor(SpecialSaleDay), y=units, fill = as.factor(SpecialSaleDay))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Paired") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Special Sale Days",y="No. of Sold Items") +
  ggtitle("HomeAudio - Units Sold on Special Sale Day") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Releasing memory for faster processing.
gc()

### Weekly spends on different marketing channels [or Media Investment]
homeAudio_perWeekAdSpent <- salesData_HomeAudio[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
homeAudio_perWeekAdSpent$week <- c(1:49)
homeAudio_perWeekAdSpent <- homeAudio_perWeekAdSpent[,c(11,2:10)]
View
# Plotting Line graph amount spent on different marketing channels
homeAudio_plots <- list()  # new empty list
for (i in 2:10) local({
  i <- i
  p2 <- ggplot(homeAudio_perWeekAdSpent,aes(x=homeAudio_perWeekAdSpent[,1],y=homeAudio_perWeekAdSpent[,i])) +
    geom_line(size=1, color = "blue") + geom_point() + theme_bw() +
    labs(x="Weeks",y= paste0("Spend on ", colnames(homeAudio_perWeekAdSpent[i])," Ads"))
  
  homeAudio_plots[[i-1]] <<- p2  # add each plot into plot list
  
})

# Plotting all graphs
# Please wait...
plot_grid(homeAudio_plots[[1]],homeAudio_plots[[2]],homeAudio_plots[[3]],homeAudio_plots[[4]],homeAudio_plots[[5]],homeAudio_plots[[6]],
          homeAudio_plots[[7]],homeAudio_plots[[8]],homeAudio_plots[[9]],align ="h")





#***************************************************************** GamingAccessory ****************************************************************************#

### Weekly Gross Merchandise Value
gamingAccessory_perWeek_gmv <- aggregate(gmv~Week_date, salesData_GamingAccessory, sum, na.rm=TRUE)

# Plotting Line graph showing weekly Gross Merchandise Value
ggplot(gamingAccessory_perWeek_gmv, aes(x=Week_date, y=gmv)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="GMV") + ggtitle("GamingAccessory - Weekly Gross Merchandise Value") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Weekly units sold
gamingAccessory_peerWeekSoldItems <- aggregate(units~Week_date, salesData_GamingAccessory, sum, na.rm=TRUE)

# Plotting Line graph showing weekly orders
ggplot(gamingAccessory_peerWeekSoldItems, aes(x=Week_date, y=units)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="No. of Sold Items") + ggtitle("GamingAccessory - Weekly Units Sold") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Sum of Weekly orders by different payment types
gamingAccessory_peerWeekSoldItems_ByPaymentType <- aggregate(units~Week_date + s1_fact.order_payment_type, salesData_GamingAccessory, sum, na.rm=TRUE)

# Plotting Bar graph showing units sold through different payment types
ggplot(gamingAccessory_peerWeekSoldItems_ByPaymentType, aes(x=Week_date,y=units, fill=as.factor(s1_fact.order_payment_type))) + geom_bar(stat="identity",position = "stack", width = 4) +
  theme_hc(base_size = 19, base_family = "sans") + labs(x="Weeks",y="No. of Sold Items") +
  ggtitle("GamingAccessory - Weekly Items Sold by Different Payment types") + scale_fill_manual("Payment Type: ", values = c("COD" = "navyblue", "Prepaid" = "goldenrod1")) +
  theme(legend.justification="center",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, size = 12,vjust = 0.4),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week"))


##############################################################################################
### Sum of units sold for top 10 CameraAccessory Products
gamingAccessory_ProductSoldItems <- aggregate(units~product_analytic_vertical, salesData_GamingAccessory, sum, na.rm=TRUE)

gamingAccessory_TopTenProducts <- gamingAccessory_ProductSoldItems[order(gamingAccessory_ProductSoldItems$units, decreasing = TRUE),][1:10,]

# Ordering top 10 products based on number of units sold for display
gamingAccessory_TopTenProducts$product_analytic_vertical <- factor(gamingAccessory_TopTenProducts$product_analytic_vertical, levels = gamingAccessory_TopTenProducts$product_analytic_vertical[order(-gamingAccessory_TopTenProducts$units)])

# Plotting Bar graph showing units sold for top 10 Products
ggplot(gamingAccessory_TopTenProducts, aes(x=as.factor(product_analytic_vertical), y=units, fill = as.factor(product_analytic_vertical))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Spectral") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Product Vertical",y="No. of Sold Items") +
  ggtitle("GamingAccessory - Items Sold") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle = 45, vjust = 0.6),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Weekly average mrp of products sold
gamingAccessory_perWeek_avg_product_mrp <- aggregate(product_mrp~Week_date, salesData_GamingAccessory, mean, na.rm=TRUE)

# Plotting Line graph showing weekly average mrp of products sold
ggplot(gamingAccessory_perWeek_avg_product_mrp, aes(x=Week_date, y=product_mrp)) + geom_line(size=1, color = "blue") + geom_point() +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 week")) +
  theme_bw() + labs(x="Weeks",y="Average Product MRP") + ggtitle("GamingAccessory - Weekly Average Product MRP") +
  theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black", angle=90, vjust = 0.5),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Sum of Product units sold on weekdays/weekends
gamingAccessory_weekDaySoldUnits <- aggregate(units~wday, salesData_GamingAccessory, sum, na.rm=TRUE)

gamingAccessory_weekDaySoldUnits$wday <- factor(gamingAccessory_weekDaySoldUnits$wday,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Plotting Bar graph showing units sold on weekdays/weekends
ggplot(gamingAccessory_weekDaySoldUnits, aes(x=as.factor(wday), y=units, fill = as.factor(wday))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Set1") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Weekdays",y="No. of Sold Items") +
  ggtitle("GamingAccessory - Units Sold on Weekdays/Weekend") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Units sold of special sale day [i.e. on different holidays]
gamingAccessory_SpecialDaySoldItems <- subset(salesData_GamingAccessory, SpecialSaleDay != "Regular Day")

gamingAccessory_SpecialDaySoldItems <- aggregate(units~SpecialSaleDay, gamingAccessory_SpecialDaySoldItems, sum, na.rm=TRUE)

# Ordering special sales day based on number of units sold
gamingAccessory_SpecialDaySoldItems$SpecialSaleDay <- factor(gamingAccessory_SpecialDaySoldItems$SpecialSaleDay, levels = gamingAccessory_SpecialDaySoldItems$SpecialSaleDay[order(-gamingAccessory_SpecialDaySoldItems$units)])

# Plotting Bar graph showing units sold on different holiday seaseons
ggplot(gamingAccessory_SpecialDaySoldItems, aes(x=as.factor(SpecialSaleDay), y=units, fill = as.factor(SpecialSaleDay))) + geom_bar(stat = "identity", width = 0.4) + scale_fill_brewer(palette="Paired") +
  geom_text(aes(label=units), vjust=-0.3, size=3.5) + theme_hc(base_size = 19, base_family = "sans") + labs(x="Special Sale Days",y="No. of Sold Items") +
  ggtitle("GamingAccessory - Units Sold on Special Sale Day") + theme(legend.position="none",axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"),axis.text.x = element_text(colour = "black"),axis.text.y = element_text(colour = "black"),title = element_text(colour = "black"),legend.title = element_text(colour = "black"),plot.title = element_text(hjust = 0.5))


##############################################################################################
### Releasing memory for faster processing.
gc()

### Weekly spends on different marketing channels [or Media Investment]
gamingAccessory_perWeekAdSpent <- salesData_GamingAccessory[,c(16,20:28)] %>% group_by(Week_date) %>% summarise_all(funs(sum)) %>% data.frame()
gamingAccessory_perWeekAdSpent$week <- c(1:53)
gamingAccessory_perWeekAdSpent <- gamingAccessory_perWeekAdSpent[,c(11,2:10)]

# Plotting Line graph amount spent on different marketing channels
gamingAccessory_plots <- list()  # new empty list
for (i in 2:10) local({
  i <- i
  p3 <- ggplot(gamingAccessory_perWeekAdSpent,aes(x=gamingAccessory_perWeekAdSpent[,1],y=gamingAccessory_perWeekAdSpent[,i])) +
    geom_line(size=1, color = "blue") + geom_point() + theme_bw() +
    labs(x="Weeks",y= paste0("Spend on ", colnames(gamingAccessory_perWeekAdSpent[i])," Ads"))
  
  gamingAccessory_plots[[i-1]] <<- p3  # add each plot into plot list
  
})

# Plotting all graphs
# Please wait...
plot_grid(gamingAccessory_plots[[1]],gamingAccessory_plots[[2]],gamingAccessory_plots[[3]],gamingAccessory_plots[[4]],gamingAccessory_plots[[5]],gamingAccessory_plots[[6]],
          gamingAccessory_plots[[7]],gamingAccessory_plots[[8]],gamingAccessory_plots[[9]],align ="h")



################################################################################################################################################################
#                                                 :::::::: Exploratory Data Analysis for KPI ::::::::
################################################################################################################################################################

###  Declaring a function for KPI's

productFeaturesFunction <- function(test){
  ##1. KPI - List Price for all products
  test$list_price <- test$gmv/test$units
  
  ##2. KPI - Promotional Offer for all Products
  test$promotional_offer <- (test$product_mrp - test$list_price)/test$product_mrp
  
  ##3. KPI - Payment Mode Indicator
  test$payment_ind <- ifelse(test$s1_fact.order_payment_type == "Prepaid",1,0)
  
  ##4. KPI - Prepaid Order Percentage
  # Total Order Placed
  total_order <- aggregate(payment_ind ~ Year+Month+week, test, FUN = NROW)
  
  # Total Online Order
  online_order <- aggregate(payment_ind ~ Year+Month+week, data = test, FUN = sum)
  
  # Merge "total_order" and "online_order"
  order_merged <- merge(total_order, online_order, by = c("Month", "Year", "week"), all.x = TRUE)
  
  # Calculating proportion of total online order from total order
  order_merged$per_order <- order_merged$payment_ind.y/order_merged$payment_ind.x
  
  # Discarding columns
  order_merged <- order_merged[,-c(4,5)]
  
  # Adding "per_order" column in dataset
  test <- merge(test, order_merged, by = c("Month", "Year", "week"), all.x = TRUE)
  
  ##5. KPI - Product Category
  cluster <- aggregate(cbind(units,list_price, product_mrp)~product_analytic_vertical, test, mean)
  
  if(nrow(cluster)>2){
    cluster$units_1 <- scale(cluster$units)
    cluster$list_price_1 <- scale(cluster$list_price)
    cluster$product_mrp_1 <- scale(cluster$product_mrp)
    
    k1 <- cluster[,-c(1:3)]
    
    # Applying clustering algorithm
    clust <- kmeans(k1, centers = 3, iter.max = 50, nstart = 50)
    cluster$price_tag <- as.factor(clust$cluster)
    cluster <- cluster[, c(1,8)]
    
    # Adding columns generated from clustering algorithm to dataset
    test <- merge(test, cluster, by=c("product_analytic_vertical"), all.x = TRUE)
    
    k2 <- count(test, price_tag)[2]
    
    levels(test$price_tag)[which(k2==max(count(test, price_tag)[2]))] <- "Mass_Product"
    levels(test$price_tag)[which(k2==min(count(test, price_tag)[2]))] <- "Premium_Product"
    levels(test$price_tag)[which(k2!=max(count(test, price_tag)[2]) & k2!=min(count(test, price_tag)[2]))] <- "Aspiring_Product"
    
  }
  
  else{
    test$price_tag <- NA
    test$price_tag$product_analytic_vertical <- factor(test$price_tag$product_analytic_vertical)
    
    if(tapply(test$product_mrp, test$product_analytic_vertical, mean)[[1]] > tapply(test$product_mrp, test$product_analytic_vertical, mean)[[2]]){
      test$price_tag[which(test$product_analytic_vertical == levels(test$product_analytic_vertical)[1])] <- "Aspiring_Product"
      test$price_tag[is.na(test$price_tag)] <- "Mass_Product"
    }
    
    else{
      test$price_tag[which(test$product_analytic_vertical == levels(test$product_analytic_vertical)[2])] <- "Aspiring_Product"
      test$price_tag[is.na(test$price_tag)] <- "Mass_Product"
    }
    
  }
  
  
  ##6. KPI - Adstock
  # Considering adstock rate as 50%
  adstock_rate = 0.50
  
  #  Declaring adstock for each media investment
  df <- data.frame(week=1:53)
  
  for(i in 3:ncol(mediaInvestmentDataPerWeekLatest)){
    
    df[[paste0(colnames(mediaInvestmentDataPerWeekLatest)[i],"_adstock")]] <- stats::filter(x=mediaInvestmentDataPerWeekLatest[i],
                                                                                            filter=adstock_rate, method="recursive")
    
  }
  
  # Merging adstock with actual dataset
  test <- merge(test, df, by = c("week"), all.x = TRUE)
  
  
  ## Converting data into weekly format
  
  # As we have data from July-2015 to June-2016, So we're considering June-105 as our base for week calculation/number
  # i.e 1st week of July-2015 as 1 (instead of 26), 2nd week of July-2015 as 2 (instead of 27) and so on till June-2016
  # Also, for Jan-2016 we'll consider subsequent week number [i.e week number after Dec-2015 last week] (instead as 1st week)
  test$week <- ifelse(test$week>=26, test$week-25, test$week+28)
  
  # Filtering out variables which are not necessary
  test <- subset(test, select = -c(Month,Year,product_analytic_sub_category,month_date,Week_date))
  
  #  Declaring two vectors which holds numeric andcategorical variables
  col_numeric <- c("week", "gmv", "units", "deliverybdays", "deliverycdays", "sla", "product_mrp", "product_procurement_sla")
  col_factor <- c("product_analytic_vertical", "s1_fact.order_payment_type","wday", "isASaleDayOrNot","SpecialSaleDay", "price_tag")
  
  # Convering continuous variables into numeric format and Categorical variables in to factors
  test[,col_numeric] <- sapply(test[,col_numeric], as.numeric)
  test[,col_factor] <- sapply(test[,col_factor], as.factor)
  
  df_dummies <- test[,col_factor]  ## Created a data frame which holds only categorical variables
  
  #  Declaring dummy variables for categorical/factor attributes
  dummies<- data.frame(sapply(df_dummies, function(x) data.frame(model.matrix(~x-1,data =df_dummies))[,-1]))
  dummies <- as.data.frame(cbind(test[1], dummies))
  
  # Aggregate dummy variables data by weeks
  dummies_aggregate <- aggregate(.~ week, dummies, sum, na.rm = TRUE)
  
  # Aggregating Actual dtaa variables by weeks
  test <- test %>% group_by(week) %>% summarise(gmv = sum(gmv), units = sum(units), deliverybdays = mean(deliverybdays), deliverycdays = mean(deliverycdays),
                                                sla = mean(sla), product_mrp = sum(product_mrp), product_procurement_sla = mean(product_procurement_sla),
                                                Total_Investment = mean(Total_Investment), TV = mean(TV), Digital = mean(Digital), Sponsorship = mean(Sponsorship),
                                                Content_Marketing = mean(Content_Marketing), Online_Marketing = mean(Online_Marketing), Affiliates = mean(Affiliates),
                                                SEM = mean(SEM), Radio = mean(Radio), Other = mean(Other), NPS_Score = mean(`NPS Score`), holidayCountInfo = mean(holidayCountInfo),
                                                list_price = sum(list_price), promotional_offer = sum(promotional_offer)/length(week), per_order = mean(per_order), TV_adstock= mean(TV_adstock),
                                                Digital_adstock = mean(Digital_adstock), Sponsorship_adstock = mean(Sponsorship_adstock), Content_Marketing_adstock = mean(Content_Marketing_adstock),
                                                Online_Marketing_adstock = mean(Online_Marketing_adstock), Affiliates_adstock = mean(Affiliates_adstock), SEM_adtock = mean(SEM_adstock),
                                                Radio_adstock = mean(Radio), Other_adstock = mean(Other_adstock))
  
  
  # Merging Dummy and actual data variables in to one data frame
  test <- merge(test, dummies_aggregate, by = c("week"), all.x = TRUE)
  
  return(test)
  
}

### Calling "productFeaturesFunction" function for 3 Product subcategories to create Engineered variables and
### Also to covet whole data into weekly format
GamingAccessory_df <- productFeaturesFunction(salesData_GamingAccessory)
HomeAudio_df <- productFeaturesFunction(salesData_HomeAudio)
CameraAccessory_df <- productFeaturesFunction(salesData_CameraAccessory)



#************************************
### Other/Advanced Engineered KPI's::

AnotherKPIFunction <- function(test){
  
  ##7. KPI - Moving average
  myfun1 = function(x) rollmean(x, k = 2, fill = NA, align = "right")
  myfun2 = function(x) rollmean(x, k = 3, fill = NA, align = "right")
  myfun3 = function(x) rollmean(x, k = 4, fill = NA, align = "right")
  
  x <- test[,c("week", "list_price", "promotional_offer")]
  x <- arrange(x, week)
  
  x1<-x %>% mutate_each(funs(myfun1),list_price,promotional_offer) %>% data.frame()
  x2<-x %>% mutate_each(funs(myfun2),list_price,promotional_offer) %>% data.frame()
  x3<-x %>% mutate_each(funs(myfun3),list_price,promotional_offer) %>% data.frame()
  
  # Imputing missing or NA values
  x1 <- imputeTS::na.ma(x1, k=2, weighting = "simple")
  x2 <- imputeTS::na.ma(x2, k=3, weighting = "simple")
  x3 <- imputeTS::na.ma(x3, k=4, weighting = "simple")
  
  x1$LP_MA1<-(x1$list_price)
  x1$PO_MA1<-(x1$promotional_offer)
  
  x2$LP_MA2<-(x2$list_price)
  x2$PO_MA2<-(x2$promotional_offer)
  
  x3$LP_MA3<-(x3$list_price)
  x3$PO_MA3<-(x3$promotional_offer)
  
  x4=cbind(x1[,-c(1:3)],x2[,-c(1:3)],x3[,-c(1:3)])
  
  test_1 <- cbind(test, x4[,c(1,3,5,2,4,6)])
  test <- test_1
  
  k9 <- test
  
  test$inc_LP_MA1<-(test$list_price - test$LP_MA1)/test$LP_MA1
  test$inc_LP_MA2<-(test$list_price - test$LP_MA2)/test$LP_MA2
  test$inc_LP_MA3<-(test$list_price - test$LP_MA3)/test$LP_MA3
  
  test$inc_PO_MA1<-(test$promotional_offer - test$PO_MA1)/test$PO_MA1
  test$inc_PO_MA2<-(test$promotional_offer - test$PO_MA2)/test$PO_MA2
  test$inc_PO_MA3<-(test$promotional_offer - test$PO_MA3)/test$PO_MA3
  
  # Deleting columns
  test$LP_MA1<-NULL
  test$LP_MA2<-NULL
  test$LP_MA3<-NULL
  
  test$PO_MA1<-NULL
  test$PO_MA2<-NULL
  test$PO_MA3<-NULL
  
  
  ##8. Lag Variables [For 'list_price', 'promotional_offer', 'gmv']
  test <- test[with(test, order(week)),]
  
  #Lag List Price (different period lags) [Lag of list price by 1st week, 2nd week, 3rd week]
  test_dum <- slide(test, Var = "list_price", slideBy = -1)
  test_dum <- slide(test_dum, Var = "list_price", slideBy = -2)
  test_dum <- slide(test_dum, Var = "list_price", slideBy = -3)
  
  #Lag Promotional Offer (different period lags) [Lag of discount(Promotional Offer) by 1st week, 2nd week, 3rd week]
  test_dum <- slide(test_dum, Var = "promotional_offer", slideBy = -1)
  test_dum <- slide(test_dum, Var = "promotional_offer", slideBy = -2)
  test_dum <- slide(test_dum, Var = "promotional_offer", slideBy = -3)
  
  #Lag gmv (different period lags) [Lag of gmv by 1st week, 2nd week, 3rd week]
  test_dum <- slide(test_dum, Var = "gmv", slideBy = -1)
  test_dum <- slide(test_dum, Var = "gmv", slideBy = -2)
  test_dum <- slide(test_dum, Var = "gmv", slideBy = -3)
  
  test <- test_dum
  
  col1 <- c("list_price-1", "promotional_offer-1", "gmv-1")
  col2 <- c("list_price-2", "promotional_offer-2", "gmv-2")
  col3 <- c("list_price-3", "promotional_offer-3", "gmv-3")
  
  test[, col1] <- imputeTS::na.ma(test[, col1], k=1, weighting = "simple")
  test[, col2] <- imputeTS::na.ma(test[, col2], k=2, weighting = "simple")
  test[, col3] <- imputeTS::na.ma(test[, col3], k=3, weighting = "simple")
  
  
  #Incremental Lags
  #Incremental Lags of List Price by 1 week, 2 week, 3 week
  test$LP_lag_1_per <- (test$list_price - test$`list_price-1`)/test$`list_price-1`
  test$LP_lag_2_per <- (test$list_price - test$`list_price-2`)/test$`list_price-2`
  test$LP_lag_3_per <- (test$list_price - test$`list_price-3`)/test$`list_price-3`
  
  test$LP_lag_1_per <- ifelse(is.na(test$LP_lag_1_per),0,test$LP_lag_1_per)
  test$LP_lag_2_per <- ifelse(is.na(test$LP_lag_2_per),0,test$LP_lag_2_per)
  test$LP_lag_3_per <- ifelse(is.na(test$LP_lag_3_per),0,test$LP_lag_3_per)
  
  #Incremental Lags of Promotional Offer by 1 week, 2 week, 3 week
  test$PO_lag_1_per <- (test$promotional_offer - test$`promotional_offer-1`)/test$`promotional_offer-1`
  test$PO_lag_2_per <- (test$promotional_offer - test$`promotional_offer-2`)/test$`promotional_offer-2`
  test$PO_lag_3_per <- (test$promotional_offer - test$`promotional_offer-3`)/test$`promotional_offer-3`
  
  test$PO_lag_1_per <- ifelse(is.na(test$PO_lag_1_per),0,test$PO_lag_1_per)
  test$PO_lag_2_per <- ifelse(is.na(test$PO_lag_2_per),0,test$PO_lag_2_per)
  test$PO_lag_3_per <- ifelse(is.na(test$PO_lag_3_per),0,test$PO_lag_3_per)
  
  #Incremental Lags of gmv by 1 week, 2 week, 3 week
  test$GMV_lag_1_per <- (test$gmv - test$`gmv-1`)/test$`gmv-1`
  test$GMV_lag_2_per <- (test$gmv - test$`gmv-2`)/test$`gmv-2`
  test$GMV_lag_3_per <- (test$gmv - test$`gmv-3`)/test$`gmv-3`
  
  test$GMV_lag_1_per <- ifelse(is.na(test$GMV_lag_1_per),0,test$GMV_lag_1_per)
  test$GMV_lag_2_per <- ifelse(is.na(test$GMV_lag_2_per),0,test$GMV_lag_2_per)
  test$GMV_lag_3_per <- ifelse(is.na(test$GMV_lag_3_per),0,test$GMV_lag_3_per)
  
  #Discarding columns
  test$`list_price-1` <- NULL
  test$`list_price-2` <- NULL
  test$`list_price-3` <- NULL
  
  test$`promotional_offer-1` <- NULL
  test$`promotional_offer-2` <- NULL
  test$`promotional_offer-3` <- NULL
  
  test$`gmv-1` <- NULL
  test$`gmv-2` <- NULL
  test$`gmv-3` <- NULL
  
  return(test)
  
}


### Calling "AnotherKPIFunction" function for 3 Product subcategories to create advanced KPI's
GamingAccessory_final <- AnotherKPIFunction(GamingAccessory_df)
HomeAudio_final <- AnotherKPIFunction(HomeAudio_df)
CameraAccessory_final <- AnotherKPIFunction(CameraAccessory_df)





#######################################################################################################################################################
#                                   :::::::: EDA for 3 Sub-categories [gmv Vs Independent variables]::::::::
######################################################################################################################################################

#***************************************************************** GamingAccessory
### Releasing memory for faster processing.
gc()

### Response Curves ['gmv' w.r.t all Independent variables]
GA <- GamingAccessory_final[,c(2:19,21:32,68:82)]

# Plotting scatter plot of all Independent variables w.r.t 'gmv'
gamingAccessory_AllPlots <- list()  # new empty list
for (i in 2:45) local({
  i <- i
  p4 <- ggplot(GA,aes(x=GA[,i],y=GA[,1])) + geom_point() + geom_smooth(method = "loess") + theme_bw() +
    labs(x= paste0("", colnames(GA[i])),y="GMV")
  
  gamingAccessory_AllPlots[[i-1]] <<- p4  # add each plot into plot list
  
})

# Plotting all graphs
# Please wait...
plot_grid(gamingAccessory_AllPlots[[1]],gamingAccessory_AllPlots[[2]],gamingAccessory_AllPlots[[3]],gamingAccessory_AllPlots[[4]],gamingAccessory_AllPlots[[5]],gamingAccessory_AllPlots[[6]],
          gamingAccessory_AllPlots[[7]],gamingAccessory_AllPlots[[8]],gamingAccessory_AllPlots[[9]],align ="h")

plot_grid(gamingAccessory_AllPlots[[10]],gamingAccessory_AllPlots[[11]],gamingAccessory_AllPlots[[12]],gamingAccessory_AllPlots[[13]],gamingAccessory_AllPlots[[14]],gamingAccessory_AllPlots[[15]],
          gamingAccessory_AllPlots[[16]],gamingAccessory_AllPlots[[17]],gamingAccessory_AllPlots[[18]],align ="h")

plot_grid(gamingAccessory_AllPlots[[19]],gamingAccessory_AllPlots[[20]],gamingAccessory_AllPlots[[21]],gamingAccessory_AllPlots[[22]],gamingAccessory_AllPlots[[23]],gamingAccessory_AllPlots[[24]],
          gamingAccessory_AllPlots[[25]],gamingAccessory_AllPlots[[26]],gamingAccessory_AllPlots[[27]],align ="h")

plot_grid(gamingAccessory_AllPlots[[28]],gamingAccessory_AllPlots[[29]],gamingAccessory_AllPlots[[30]],gamingAccessory_AllPlots[[31]],gamingAccessory_AllPlots[[32]],gamingAccessory_AllPlots[[33]],
          gamingAccessory_AllPlots[[34]],gamingAccessory_AllPlots[[35]],gamingAccessory_AllPlots[[36]],align ="h")

plot_grid(gamingAccessory_AllPlots[[37]],gamingAccessory_AllPlots[[38]],gamingAccessory_AllPlots[[39]],gamingAccessory_AllPlots[[40]],gamingAccessory_AllPlots[[41]],gamingAccessory_AllPlots[[42]],
          gamingAccessory_AllPlots[[43]],gamingAccessory_AllPlots[[44]],align ="h")




#***************************************************************** HomeAudio
### Releasing memory for faster processing.
gc()

### ### Response Curves ['gmv' w.r.t all Independent variables]
HA <- HomeAudio_final[,c(2:19,21:32,63:77)]

# Plotting scatter plot of all Independent variables w.r.t 'gmv'
homeAudio_AllPlots <- list()  # new empty list
for (i in 2:45) local({
  i <- i
  p5 <- ggplot(HA,aes(x=HA[,i],y=HA[,1])) + geom_point() + geom_smooth(method = "loess") + theme_bw() +
    labs(x= paste0("", colnames(HA[i])),y="GMV")
  
  homeAudio_AllPlots[[i-1]] <<- p5  # add each plot into plot list
  
})

# Plotting all graphs
# Please wait...
plot_grid(homeAudio_AllPlots[[1]],homeAudio_AllPlots[[2]],homeAudio_AllPlots[[3]],homeAudio_AllPlots[[4]],homeAudio_AllPlots[[5]],homeAudio_AllPlots[[6]],
          homeAudio_AllPlots[[7]],homeAudio_AllPlots[[8]],homeAudio_AllPlots[[9]],align ="h")

plot_grid(homeAudio_AllPlots[[10]],homeAudio_AllPlots[[11]],homeAudio_AllPlots[[12]],homeAudio_AllPlots[[13]],homeAudio_AllPlots[[14]],homeAudio_AllPlots[[15]],
          homeAudio_AllPlots[[16]],homeAudio_AllPlots[[17]],homeAudio_AllPlots[[18]],align ="h")

plot_grid(homeAudio_AllPlots[[19]],homeAudio_AllPlots[[20]],homeAudio_AllPlots[[21]],homeAudio_AllPlots[[22]],homeAudio_AllPlots[[23]],homeAudio_AllPlots[[24]],
          homeAudio_AllPlots[[25]],homeAudio_AllPlots[[26]],homeAudio_AllPlots[[27]],align ="h")

plot_grid(homeAudio_AllPlots[[28]],homeAudio_AllPlots[[29]],homeAudio_AllPlots[[30]],homeAudio_AllPlots[[31]],homeAudio_AllPlots[[32]],homeAudio_AllPlots[[33]],
          homeAudio_AllPlots[[34]],homeAudio_AllPlots[[35]],homeAudio_AllPlots[[36]],align ="h")

plot_grid(homeAudio_AllPlots[[37]],homeAudio_AllPlots[[38]],homeAudio_AllPlots[[39]],homeAudio_AllPlots[[40]],homeAudio_AllPlots[[41]],homeAudio_AllPlots[[42]],
          homeAudio_AllPlots[[43]],homeAudio_AllPlots[[44]],align ="h")




#***************************************************************** CameraAccessory
### Releasing memory for faster processing.
gc()

### Response Curves ['gmv' w.r.t all Independent variables]
CA <- CameraAccessory_final[,c(2:19,21:32,77:91)]

# Plotting scatter plot of all Independent variables w.r.t 'gmv'
cameraAccessory_AllPlots <- list()  # new empty list
for (i in 2:45) local({
  i <- i
  p6 <- ggplot(CA,aes(x=CA[,i],y=CA[,1])) + geom_point() + geom_smooth(method = "loess") + theme_bw() +
    labs(x= paste0("", colnames(CA[i])),y="GMV")
  
  cameraAccessory_AllPlots[[i-1]] <<- p6  # add each plot into plot list
  
})

# Plotting all graphs
# Please wait...
plot_grid(cameraAccessory_AllPlots[[1]],cameraAccessory_AllPlots[[2]],cameraAccessory_AllPlots[[3]],cameraAccessory_AllPlots[[4]],cameraAccessory_AllPlots[[5]],cameraAccessory_AllPlots[[6]],
          cameraAccessory_AllPlots[[7]],cameraAccessory_AllPlots[[8]],cameraAccessory_AllPlots[[9]],align ="h")

plot_grid(cameraAccessory_AllPlots[[10]],cameraAccessory_AllPlots[[11]],cameraAccessory_AllPlots[[12]],cameraAccessory_AllPlots[[13]],cameraAccessory_AllPlots[[14]],cameraAccessory_AllPlots[[15]],
          cameraAccessory_AllPlots[[16]],cameraAccessory_AllPlots[[17]],cameraAccessory_AllPlots[[18]],align ="h")

plot_grid(cameraAccessory_AllPlots[[19]],cameraAccessory_AllPlots[[20]],cameraAccessory_AllPlots[[21]],cameraAccessory_AllPlots[[22]],cameraAccessory_AllPlots[[23]],cameraAccessory_AllPlots[[24]],
          cameraAccessory_AllPlots[[25]],cameraAccessory_AllPlots[[26]],cameraAccessory_AllPlots[[27]],align ="h")

plot_grid(cameraAccessory_AllPlots[[28]],cameraAccessory_AllPlots[[29]],cameraAccessory_AllPlots[[30]],cameraAccessory_AllPlots[[31]],cameraAccessory_AllPlots[[32]],cameraAccessory_AllPlots[[33]],
          cameraAccessory_AllPlots[[34]],cameraAccessory_AllPlots[[35]],cameraAccessory_AllPlots[[36]],align ="h")

plot_grid(cameraAccessory_AllPlots[[37]],cameraAccessory_AllPlots[[38]],cameraAccessory_AllPlots[[39]],cameraAccessory_AllPlots[[40]],cameraAccessory_AllPlots[[41]],cameraAccessory_AllPlots[[42]],
          cameraAccessory_AllPlots[[43]],cameraAccessory_AllPlots[[44]],align ="h")

################################################################################################################################################################

#******************************************************** [Linear Regression Model CameraAccessory
################################################################################################################################################################

### Preparing dataset
## Discarding lag variables and Moving averages variables
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
LinearReg_cameraAccessory_Dataset <- CameraAccessory_final[,-c(21:22,77:91)]

## Scaling variables
LinearReg_cameraAccessory_Dataset[,2:ncol(LinearReg_cameraAccessory_Dataset)] <- scale(LinearReg_cameraAccessory_Dataset[,2:ncol(LinearReg_cameraAccessory_Dataset)])

## Check variables for linear relationship or multicollinearity
model <- lm(gmv~.,LinearReg_cameraAccessory_Dataset)
alias(model)

## Discarding variables which were showing linear relationship or multicollinearity
LinearReg_cameraAccessory_Dataset <- LinearReg_cameraAccessory_Dataset[, -c(54:72)]


### Stepwise Regression to remove insignificant and correlated variables
LinearReg_cameraAccessory_Base.Model<- lm(gmv ~ 1 , data= LinearReg_cameraAccessory_Dataset)  # base intercept only model
LinearReg_cameraAccessory_All.Model <- lm(gmv ~ . , data= LinearReg_cameraAccessory_Dataset) # full model with all predictors
LinearReg_cameraAccessory_StepModel<- step(LinearReg_cameraAccessory_Base.Model, scope = list(lower = LinearReg_cameraAccessory_Base.Model, upper = LinearReg_cameraAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
LinearReg_cameraAccessory_ShortListedVariables <- names(unlist(LinearReg_cameraAccessory_StepModel[[1]])) # get shortlisted variable.
LinearReg_cameraAccessory_ShortListedVariables <- LinearReg_cameraAccessory_ShortListedVariables[!LinearReg_cameraAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables[using LinearReg_cameraAccessory_StepModel]
LinearReg_cameraAccessoryMODEL_1 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                         product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                         Online_Marketing_adstock + product_analytic_vertical.xExtensionTube +
                                         product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock +
                                         Sponsorship + product_analytic_vertical.xTeleconverter +
                                         sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope +
                                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                         Content_Marketing + Total_Investment + product_analytic_vertical.xStrap +
                                         product_analytic_vertical.xCameraEyeCup, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_1)
vif(LinearReg_cameraAccessoryMODEL_1)


## High VIF and Non-significant p-value columns: Total_Investment
## Slightly high VIF and Non-significant p-value columns: Sponsorship, product_analytic_vertical.xCameraMount
## Non-significant p-value columns: sla, product_analytic_vertical.xCameraEyeCup
LinearReg_cameraAccessoryMODEL_2 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                         product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                         Online_Marketing_adstock + product_analytic_vertical.xExtensionTube +
                                         week + Content_Marketing_adstock + product_analytic_vertical.xTeleconverter +
                                         product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope +
                                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                         Content_Marketing + product_analytic_vertical.xStrap, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_2)
vif(LinearReg_cameraAccessoryMODEL_2)


## Slightly high VIF and Non-significant p-value columns: week
## High VIF and Non-significant p-value columns: week
## Non-significant p-value columns: product_analytic_vertical.xCameraBatteryGrip
LinearReg_cameraAccessoryMODEL_3 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                         product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                         Online_Marketing_adstock + product_analytic_vertical.xExtensionTube + Content_Marketing_adstock +
                                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope +
                                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                         product_analytic_vertical.xStrap, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_3)
vif(LinearReg_cameraAccessoryMODEL_3)


## High VIF and Non-significant p-value columns: product_analytic_vertical.xCameraBattery
## Non-significant p-value columns: Content_Marketing_adstock
LinearReg_cameraAccessoryMODEL_4 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                         product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                         Online_Marketing_adstock + product_analytic_vertical.xExtensionTube +
                                         product_analytic_vertical.xTeleconverter + product_analytic_vertical.xTelescope +
                                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xStrap, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_4)
vif(LinearReg_cameraAccessoryMODEL_4)


## Slightly high VIF and Non-significant p-value columns: product_analytic_vertical.xCameraRemoteControl
## Non-significant p-value columns: product_analytic_vertical.xTeleconverter
LinearReg_cameraAccessoryMODEL_5 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                         product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTelescope +
                                         product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xStrap, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_5)
vif(LinearReg_cameraAccessoryMODEL_5)


## Slightly High VIF and Less significant p-value columns: product_analytic_vertical.xStrap, product_analytic_vertical.xCameraFilmRolls
LinearReg_cameraAccessoryMODEL_6 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                         product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                         product_analytic_vertical.xExtensionTube + product_analytic_vertical.xTelescope, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_6)
vif(LinearReg_cameraAccessoryMODEL_6)


## Non-significant p-value columns: product_analytic_vertical.xTelescope
## less significant p-value columns: Online_Marketing_adstock, product_analytic_vertical.xExtensionTube
LinearReg_cameraAccessoryMODEL_7 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                         product_analytic_vertical.xReflectorUmbrella, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_7)
vif(LinearReg_cameraAccessoryMODEL_7)


## Less significant p-value columns: product_analytic_vertical.xReflectorUmbrella
LinearReg_cameraAccessoryMODEL_8 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_8)
vif(LinearReg_cameraAccessoryMODEL_8)


## Less significant p-value columns: product_analytic_vertical.xCameraBatteryCharger
LinearReg_cameraAccessoryMODEL_9 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                         deliverycdays + NPS_Score, data = LinearReg_cameraAccessory_Dataset)


summary(LinearReg_cameraAccessoryMODEL_9)
vif(LinearReg_cameraAccessoryMODEL_9)


## Discarding "units" variable and then check Adjusted R-squared value
LinearReg_cameraAccessoryMODEL_10 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod +
                                          deliverycdays + NPS_Score, data = LinearReg_cameraAccessory_Dataset)

summary(LinearReg_cameraAccessoryMODEL_10)  # Adjusted R-squared value is changed at 3rd place of decimal, we'good to remove that variable
vif(LinearReg_cameraAccessoryMODEL_10)




### CROSS VALIDATION
cv.lm(data = LinearReg_cameraAccessory_Dataset, form.lm = LinearReg_cameraAccessoryMODEL_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_3 <- function(var){
  LinearReg_cameraAccessory_ElasticityCoeff <-as.numeric(LinearReg_cameraAccessoryMODEL_10$coefficients[var]*mean(LinearReg_cameraAccessory_Dataset[,var])/mean(LinearReg_cameraAccessory_Dataset$gmv))
  return(LinearReg_cameraAccessory_ElasticityCoeff)
  
}

LinearReg_cameraAccessory_var_list <- list()

for(i in 2:length(LinearReg_cameraAccessoryMODEL_10$coefficients)){
  LinearReg_cameraAccessory_var_list[i-1] <-ElasticityValue_3(names(LinearReg_cameraAccessoryMODEL_10$coefficients)[i])
  
}

LinearReg_cameraAccessory_ElasticityCoeff.outputs <- data.frame(names(LinearReg_cameraAccessoryMODEL_10$coefficients[2:length(LinearReg_cameraAccessoryMODEL_10$coefficients)]))
LinearReg_cameraAccessory_ElasticityCoeff.outputs <- cbind(LinearReg_cameraAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, LinearReg_cameraAccessory_var_list))
colnames(LinearReg_cameraAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

LinearReg_cameraAccessory_ElasticityCoeff.outputs$Direction <- ifelse(LinearReg_cameraAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
LinearReg_cameraAccessory_ElasticityCoeff.outputs
#   Variable Elasticity Direction
#1                             product_mrp   -1.21641  Negative
#2 product_analytic_vertical.xCameraTripod   -0.04380  Negative
#3                           deliverycdays   -0.00662  Negative
#4                               NPS_Score    0.05792  Positive
# Plotting Elasticity
ggplot(LinearReg_cameraAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity", width = 0.9) + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
  ggtitle("CameraAccessory - Linear Regression Model") +xlab("Variables")


##                             #********************************************************[Koyck Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1 week lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
KoyckcameraAccessory_Dataset <- CameraAccessory_final[,-c(21:22,77:88,90:91)]

## Scaling variables
KoyckcameraAccessory_Dataset[,2:ncol(KoyckcameraAccessory_Dataset)] <- scale(KoyckcameraAccessory_Dataset[,2:ncol(KoyckcameraAccessory_Dataset)])

## Check variables for linear relationship or multicollinearity
KoyckcameraAccessory_model <- lm(gmv~.,KoyckcameraAccessory_Dataset)
alias(KoyckcameraAccessory_model)

## Discarding variables which were showing linear relationship or multicollinearity
KoyckcameraAccessory_Dataset <- KoyckcameraAccessory_Dataset[, -c(54:72)]


### Stepwise Regression to remove insignificant and correlated variables
KoyckcameraAccessory_Base.Model<- lm(gmv ~ 1 , data= KoyckcameraAccessory_Dataset)  # base intercept only model
KoyckcameraAccessory_All.Model <- lm(gmv ~ . , data= KoyckcameraAccessory_Dataset) # full model with all predictors
KoyckcameraAccessory_StepModel<- step(KoyckcameraAccessory_Base.Model, scope = list(lower = KoyckcameraAccessory_Base.Model, upper = KoyckcameraAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
KoyckcameraAccessory_ShortListedVariables <- names(unlist(KoyckcameraAccessory_StepModel[[1]])) # get shortlisted variable.
KoyckcameraAccessory_ShortListedVariables <- KoyckcameraAccessory_ShortListedVariables[!KoyckcameraAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables[using KoyckcameraAccessory_StepModel]
KoyckcameraAccessoryMODEL_1 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                    Online_Marketing_adstock + product_analytic_vertical.xExtensionTube +
                                    product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock +
                                    Sponsorship + product_analytic_vertical.xTeleconverter +
                                    sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope +
                                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                    Content_Marketing + Total_Investment + product_analytic_vertical.xStrap +
                                    product_analytic_vertical.xCameraEyeCup, data = KoyckcameraAccessory_Dataset)



summary(KoyckcameraAccessoryMODEL_1)
vif(KoyckcameraAccessoryMODEL_1)


## Non-significant p-value columns: product_analytic_vertical.xCameraEyeCup, product_analytic_vertical.xCameraBatteryGrip, sla
## High VIF and Non-significant p-value columns: product_analytic_vertical.xStrap
KoyckcameraAccessoryMODEL_2 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                    product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                    Online_Marketing_adstock + product_analytic_vertical.xExtensionTube +
                                    product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock +
                                    Sponsorship + product_analytic_vertical.xTeleconverter +
                                    product_analytic_vertical.xTelescope + product_analytic_vertical.xCameraFilmRolls +
                                    product_analytic_vertical.xCameraBattery + Content_Marketing + Total_Investment, data = KoyckcameraAccessory_Dataset)

summary(KoyckcameraAccessoryMODEL_2)
vif(KoyckcameraAccessoryMODEL_2)


## High VIF and Non-significant p-value columns: product_analytic_vertical.xCameraRemoteControl
## Slightly high VIF and Non-significant p-value columns: deliverycdays, product_analytic_vertical.xTelescope
KoyckcameraAccessoryMODEL_3 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                    product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount +
                                    week + Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter +
                                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                    Content_Marketing + Total_Investment, data = KoyckcameraAccessory_Dataset)


summary(KoyckcameraAccessoryMODEL_3)
vif(KoyckcameraAccessoryMODEL_3)


## Slightly High VIF and Less significant p-value columns: week
KoyckcameraAccessoryMODEL_4 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                    product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount +
                                    Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter +
                                    product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                    Content_Marketing + Total_Investment, data = KoyckcameraAccessory_Dataset)

summary(KoyckcameraAccessoryMODEL_4)
vif(KoyckcameraAccessoryMODEL_4)


## High VIF and insignificant p-value columns: product_analytic_vertical.xCameraBattery
## less significant p-value columns: product_analytic_vertical.xTeleconverter
KoyckcameraAccessoryMODEL_5 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                    product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount +
                                    Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xCameraFilmRolls +
                                    Content_Marketing + Total_Investment, data = KoyckcameraAccessory_Dataset)

summary(KoyckcameraAccessoryMODEL_5)
vif(KoyckcameraAccessoryMODEL_5)


## High VIF and Non-significant p-value columns: Total_Investment, Content_Marketing
## Less significant p-value columns: product_analytic_vertical.xCameraFilmRolls
KoyckcameraAccessoryMODEL_6 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                    product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                    product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount +
                                    Content_Marketing_adstock + Sponsorship, data = KoyckcameraAccessory_Dataset)

summary(KoyckcameraAccessoryMODEL_6)
vif(KoyckcameraAccessoryMODEL_6)


## Less significant p-value columns: product_analytic_vertical.xCameraMount, product_analytic_vertical.xReflectorUmbrella,
##                                   product_analytic_vertical.xExtensionTube
KoyckcameraAccessoryMODEL_7 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                    Online_Marketing_adstock + Content_Marketing_adstock + Sponsorship, data = KoyckcameraAccessory_Dataset)


summary(KoyckcameraAccessoryMODEL_7)
vif(KoyckcameraAccessoryMODEL_7)


## Less significant p-value columns: Online_Marketing_adstock, Content_Marketing_adstock
KoyckcameraAccessoryMODEL_8 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    NPS_Score + product_analytic_vertical.xCameraBatteryCharger + Sponsorship, data = KoyckcameraAccessory_Dataset)

summary(KoyckcameraAccessoryMODEL_8)
vif(KoyckcameraAccessoryMODEL_8)


## Discarding "Sponsorship" variable and will check change in Adjusted R-squared value
KoyckcameraAccessoryMODEL_9 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                    NPS_Score + product_analytic_vertical.xCameraBatteryCharger , data = KoyckcameraAccessory_Dataset)


summary(KoyckcameraAccessoryMODEL_9) # Slight change at 3rd place of decimal in Adjusted R-squared value
vif(KoyckcameraAccessoryMODEL_9)


## Discarding "product_analytic_vertical.xCameraBatteryCharger" variable and will check change in Adjusted R-squared value
KoyckcameraAccessoryMODEL_10 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                     NPS_Score, data = KoyckcameraAccessory_Dataset)


summary(KoyckcameraAccessoryMODEL_10) # Slight change at 3rd place of decimal in Adjusted R-squared value
vif(KoyckcameraAccessoryMODEL_10)


## Discarding "units" [because of high VIF] variable
KoyckcameraAccessoryMODEL_11 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod +
                                     NPS_Score, data = KoyckcameraAccessory_Dataset)


summary(KoyckcameraAccessoryMODEL_11)
vif(KoyckcameraAccessoryMODEL_11)
##Futher removing variable lead to decreasing in Adjusted R-squared and increased in residual error


### CROSS VALIDATION
cv.lm(data = KoyckcameraAccessory_Dataset, form.lm = KoyckcameraAccessoryMODEL_11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_9 <- function(var){
  KoyckcameraAccessory_ElasticityCoeff <- as.numeric(KoyckcameraAccessoryMODEL_11$coefficients[var]*mean(KoyckcameraAccessory_Dataset[,var])/mean(KoyckcameraAccessory_Dataset$gmv))
  return(KoyckcameraAccessory_ElasticityCoeff)
  
}

KoyckcameraAccessory_var_list <- list()

for(i in 2:length(KoyckcameraAccessoryMODEL_11$coefficients)){
  KoyckcameraAccessory_var_list[i-1] <- ElasticityValue_9(names(KoyckcameraAccessoryMODEL_11$coefficients)[i])
  
}

KoyckcameraAccessory_ElasticityCoeff.outputs <- data.frame(names(KoyckcameraAccessoryMODEL_11$coefficients[2:length(KoyckcameraAccessoryMODEL_11$coefficients)]))
KoyckcameraAccessory_ElasticityCoeff.outputs <- cbind(KoyckcameraAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, KoyckcameraAccessory_var_list))
colnames(KoyckcameraAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

KoyckcameraAccessory_ElasticityCoeff.outputs$Direction <- ifelse(KoyckcameraAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
KoyckcameraAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                             product_mrp   1.87e+00  Positive
#2 product_analytic_vertical.xCameraTripod  -1.17e-05  Negative
#3                               NPS_Score  -7.08e-07  Negative
# Plotting Elasticity
ggplot(KoyckcameraAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("CameraAccessory - Koyck Model") +xlab("Variables")


#********************************************************[Distributive Lag Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1, 2 and 3 weeks lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
DistriLagcameraAccessory_Dataset <- CameraAccessory_final[,-c(21:22,77:88)]

## Scaling variables
DistriLagcameraAccessory_Dataset[,2:ncol(DistriLagcameraAccessory_Dataset)] <- scale(DistriLagcameraAccessory_Dataset[,2:ncol(DistriLagcameraAccessory_Dataset)])

## Check variables for linear relationship or multicollinearity
DistriLagcameraAccessory_model <- lm(gmv~.,DistriLagcameraAccessory_Dataset)
alias(DistriLagcameraAccessory_model)

## Discarding variables which were showing linear relationship or multicollinearity
DistriLagcameraAccessory_Dataset <- DistriLagcameraAccessory_Dataset[, -c(54:72)]


### Stepwise Regression to remove insignificant and correlated variables
DistriLagcameraAccessory_Base.Model<- lm(gmv ~ 1 , data= DistriLagcameraAccessory_Dataset)  # base intercept only model
DistriLagcameraAccessory_All.Model <- lm(gmv ~ . , data= DistriLagcameraAccessory_Dataset) # full model with all predictors
DistriLagcameraAccessory_StepModel<- step(DistriLagcameraAccessory_Base.Model, scope = list(lower = DistriLagcameraAccessory_Base.Model, upper = DistriLagcameraAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
DistriLagcameraAccessory_ShortListedVariables <- names(unlist(DistriLagcameraAccessory_StepModel[[1]])) # get shortlisted variable.
DistriLagcameraAccessory_ShortListedVariables <- DistriLagcameraAccessory_ShortListedVariables[!DistriLagcameraAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables[using DistriLagcameraAccessory_StepModel]
DistriLagcameraAccessoryMODEL_1 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                        Online_Marketing_adstock + product_analytic_vertical.xExtensionTube +
                                        product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock +
                                        Sponsorship + product_analytic_vertical.xTeleconverter +
                                        sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope +
                                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                        Content_Marketing + Total_Investment + product_analytic_vertical.xStrap +
                                        product_analytic_vertical.xCameraEyeCup, data = DistriLagcameraAccessory_Dataset)


summary(DistriLagcameraAccessoryMODEL_1)
vif(DistriLagcameraAccessoryMODEL_1)


## Non-significant p-value columns: product_analytic_vertical.xCameraEyeCup
## High VIF and Non-significant p-value columns: product_analytic_vertical.xStrap, Total_Investment
DistriLagcameraAccessoryMODEL_2 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        product_analytic_vertical.xReflectorUmbrella + product_analytic_vertical.xCameraRemoteControl +
                                        Online_Marketing_adstock + product_analytic_vertical.xExtensionTube +
                                        product_analytic_vertical.xCameraMount + week + Content_Marketing_adstock +
                                        Sponsorship + product_analytic_vertical.xTeleconverter +
                                        sla + product_analytic_vertical.xCameraBatteryGrip + product_analytic_vertical.xTelescope +
                                        product_analytic_vertical.xCameraFilmRolls + product_analytic_vertical.xCameraBattery +
                                        Content_Marketing, data = DistriLagcameraAccessory_Dataset)


summary(DistriLagcameraAccessoryMODEL_2)
vif(DistriLagcameraAccessoryMODEL_2)


## High VIF and Non-significant p-value columns: Content_Marketing, product_analytic_vertical.xCameraBattery
## Non-significant p-value columns: product_analytic_vertical.xCameraBatteryGrip
## Slightly High VIF and Non-significant p-value columns: product_analytic_vertical.xCameraRemoteControl
DistriLagcameraAccessoryMODEL_3 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount +
                                        week + Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter +
                                        sla + product_analytic_vertical.xTelescope + product_analytic_vertical.xCameraFilmRolls, data = DistriLagcameraAccessory_Dataset)


summary(DistriLagcameraAccessoryMODEL_3)
vif(DistriLagcameraAccessoryMODEL_3)


## Non-significant p-value columns: sla
## Slightly high VIF and Less significant p-value columns: product_analytic_vertical.xTelescope
DistriLagcameraAccessoryMODEL_4 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        deliverycdays + NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount +
                                        week + Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xTeleconverter +
                                        product_analytic_vertical.xCameraFilmRolls, data = DistriLagcameraAccessory_Dataset)


summary(DistriLagcameraAccessoryMODEL_4)
vif(DistriLagcameraAccessoryMODEL_4)


## Slightly high VIF and Non-significant p-value columns: week
## Non-significant p-value columns: product_analytic_vertical.xTeleconverter
## less significant p-value columns: deliverycdays
DistriLagcameraAccessoryMODEL_5 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        product_analytic_vertical.xReflectorUmbrella + Online_Marketing_adstock +
                                        product_analytic_vertical.xExtensionTube + product_analytic_vertical.xCameraMount +
                                        Content_Marketing_adstock + Sponsorship +
                                        product_analytic_vertical.xCameraFilmRolls, data = DistriLagcameraAccessory_Dataset)


summary(DistriLagcameraAccessoryMODEL_5)
vif(DistriLagcameraAccessoryMODEL_5)


## Less significant p-value columns: product_analytic_vertical.xCameraFilmRolls, product_analytic_vertical.xCameraMount,
##                                   product_analytic_vertical.xReflectorUmbrella
DistriLagcameraAccessoryMODEL_6 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        NPS_Score + product_analytic_vertical.xCameraBatteryCharger + Online_Marketing_adstock +
                                        product_analytic_vertical.xExtensionTube + Content_Marketing_adstock +
                                        Sponsorship, data = DistriLagcameraAccessory_Dataset)


summary(DistriLagcameraAccessoryMODEL_6)
vif(DistriLagcameraAccessoryMODEL_6)


## Less significant p-value columns: product_analytic_vertical.xExtensionTube, Online_Marketing_adstock
DistriLagcameraAccessoryMODEL_7 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        Content_Marketing_adstock + Sponsorship, data = DistriLagcameraAccessory_Dataset)

summary(DistriLagcameraAccessoryMODEL_7)
vif(DistriLagcameraAccessoryMODEL_7)


## Non-significant p-value columns: Content_Marketing_adstock
DistriLagcameraAccessoryMODEL_8 <- lm(formula = gmv ~ product_mrp + units + product_analytic_vertical.xCameraTripod +
                                        NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        Sponsorship, data = DistriLagcameraAccessory_Dataset)

summary(DistriLagcameraAccessoryMODEL_8)
vif(DistriLagcameraAccessoryMODEL_8)


## High VIF value columns: units
DistriLagcameraAccessoryMODEL_9 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod +
                                        NPS_Score + product_analytic_vertical.xCameraBatteryCharger +
                                        Sponsorship, data = DistriLagcameraAccessory_Dataset)

summary(DistriLagcameraAccessoryMODEL_9)
vif(DistriLagcameraAccessoryMODEL_9)


## Non-significant p-value columns: product_analytic_vertical.xCameraBatteryCharger, Sponsorship
DistriLagcameraAccessoryMODEL_10 <- lm(formula = gmv ~ product_mrp + product_analytic_vertical.xCameraTripod +
                                         NPS_Score, data = DistriLagcameraAccessory_Dataset)

summary(DistriLagcameraAccessoryMODEL_10)
vif(DistriLagcameraAccessoryMODEL_10)
##Futher removing variable lead to decreasing in Adjusted R-squared and increased in residual error


### CROSS VALIDATION
cv.lm(data = DistriLagcameraAccessory_Dataset, form.lm = DistriLagcameraAccessoryMODEL_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_12 <- function(var){
  DistriLagcameraAccessory_ElasticityCoeff <- as.numeric(DistriLagcameraAccessoryMODEL_10$coefficients[var]*mean(DistriLagcameraAccessory_Dataset[,var])/mean(DistriLagcameraAccessory_Dataset$gmv))
  return(DistriLagcameraAccessory_ElasticityCoeff)
  
}

DistriLagcameraAccessory_var_list <- list()

for(i in 2:length(DistriLagcameraAccessoryMODEL_10$coefficients)){
  DistriLagcameraAccessory_var_list[i-1] <- ElasticityValue_12(names(DistriLagcameraAccessoryMODEL_10$coefficients)[i])
  
}

DistriLagcameraAccessory_ElasticityCoeff.outputs <- data.frame(names(DistriLagcameraAccessoryMODEL_10$coefficients[2:length(DistriLagcameraAccessoryMODEL_10$coefficients)]))
DistriLagcameraAccessory_ElasticityCoeff.outputs <- cbind(DistriLagcameraAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, DistriLagcameraAccessory_var_list))
colnames(DistriLagcameraAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

DistriLagcameraAccessory_ElasticityCoeff.outputs$Direction <- ifelse(DistriLagcameraAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
DistriLagcameraAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                             product_mrp    -1.1505  Negative
#2 product_analytic_vertical.xCameraTripod    -0.0269  Negative
#3                               NPS_Score     0.0662  Positive


# Plotting Elasticity
ggplot(DistriLagcameraAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity", width = 0.8) + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("CameraAccessory - Distributive Lag Model") +xlab("Variables")


#********************************************************[Multiplicative Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
MultiModel_cameraAccessory_Dataset <- CameraAccessory_final[,-c(21:22,77:91)]

## Replacing 0 value in column with '0.00001' as log(0) is undefined
MultiModel_cameraAccessory_Dataset[MultiModel_cameraAccessory_Dataset == 0] <- 0.00001

## Taking log of all variable to buils to Multiplicative model
MultiModel_cameraAccessory_Dataset <- log(MultiModel_cameraAccessory_Dataset)

## Check variables for linear relationship or multicollinearity
MultiModel_cameraAccessory_model <- lm(gmv~.,MultiModel_cameraAccessory_Dataset)
alias(MultiModel_cameraAccessory_model)

## Discarding variables which were showing linear relationship or multicollinearity
MultiModel_cameraAccessory_Dataset <- MultiModel_cameraAccessory_Dataset[, -c(54:72)]


### Stepwise Regression to remove insignificant and correlated variables
MultiModel_cameraAccessory_Base.Model<- lm(gmv ~ 1 , data= MultiModel_cameraAccessory_Dataset)  # base intercept only model
MultiModel_cameraAccessory_All.Model <- lm(gmv ~ . , data= MultiModel_cameraAccessory_Dataset) # full model with all predictors
MultiModel_cameraAccessory_StepModel<- step(MultiModel_cameraAccessory_Base.Model, scope = list(lower = MultiModel_cameraAccessory_Base.Model, upper = MultiModel_cameraAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MultiModel_cameraAccessory_ShortListedVariables <- names(unlist(MultiModel_cameraAccessory_StepModel[[1]])) # get shortlisted variable.
MultiModel_cameraAccessory_ShortListedVariables <- MultiModel_cameraAccessory_ShortListedVariables[!MultiModel_cameraAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables[using MultiModel_cameraAccessory_StepModel]
MultiModel_cameraAccessoryMODEL_1 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                          units + product_analytic_vertical.xCameraEyeCup + deliverycdays +
                                          product_analytic_vertical.xTelescope + week + product_analytic_vertical.xFlashShoeAdapter +
                                          SEM_adtock + Sponsorship, data = MultiModel_cameraAccessory_Dataset)


summary(MultiModel_cameraAccessoryMODEL_1)
vif(MultiModel_cameraAccessoryMODEL_1)


## Non-significant p-value columns: product_analytic_vertical.xFlashShoeAdapter, deliverycdays, week
MultiModel_cameraAccessoryMODEL_2 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                          units + product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xTelescope +
                                          SEM_adtock + Sponsorship, data = MultiModel_cameraAccessory_Dataset)


summary(MultiModel_cameraAccessoryMODEL_2)
vif(MultiModel_cameraAccessoryMODEL_2)


## High VIF and insignificant p-value columns: product_analytic_vertical.xTelescope
MultiModel_cameraAccessoryMODEL_3 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                          units + product_analytic_vertical.xCameraEyeCup + SEM_adtock + Sponsorship, data = MultiModel_cameraAccessory_Dataset)


summary(MultiModel_cameraAccessoryMODEL_3)
vif(MultiModel_cameraAccessoryMODEL_3)


## Non-significant p-value columns: SEM_adtock, Sponsorship
## Less significant p-value columns: product_analytic_vertical.xCameraEyeCup
MultiModel_cameraAccessoryMODEL_4 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                          units, data = MultiModel_cameraAccessory_Dataset)


summary(MultiModel_cameraAccessoryMODEL_4)
vif(MultiModel_cameraAccessoryMODEL_4)


## High VIF value columns: units
MultiModel_cameraAccessoryMODEL_5 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                          product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount, data = MultiModel_cameraAccessory_Dataset)

summary(MultiModel_cameraAccessoryMODEL_5)
vif(MultiModel_cameraAccessoryMODEL_5)


## High VIF value columns: product_analytic_vertical.xCameraMount, product_analytic_vertical.xCameraBatteryCharger
MultiModel_cameraAccessoryMODEL_6 <- lm(formula = gmv ~ product_mrp + product_procurement_sla +
                                          product_analytic_vertical.xCameraTripod , data = MultiModel_cameraAccessory_Dataset)


summary(MultiModel_cameraAccessoryMODEL_6)
vif(MultiModel_cameraAccessoryMODEL_6)



### CROSS VALIDATION
cv.lm(data = MultiModel_cameraAccessory_Dataset, form.lm = MultiModel_cameraAccessoryMODEL_6, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_6 <- function(var){
  MultiModel_cameraAccessory_ElasticityCoeff <- as.numeric(MultiModel_cameraAccessoryMODEL_6$coefficients[var]*mean(MultiModel_cameraAccessory_Dataset[,var])/mean(MultiModel_cameraAccessory_Dataset$gmv))
  return(MultiModel_cameraAccessory_ElasticityCoeff)
  
}

MultiModel_cameraAccessory_var_list <- list()

for(i in 2:length(MultiModel_cameraAccessoryMODEL_6$coefficients)){
  MultiModel_cameraAccessory_var_list[i-1] <- ElasticityValue_6(names(MultiModel_cameraAccessoryMODEL_6$coefficients)[i])
  
}

MultiModel_cameraAccessory_ElasticityCoeff.outputs <- data.frame(names(MultiModel_cameraAccessoryMODEL_6$coefficients[2:length(MultiModel_cameraAccessoryMODEL_6$coefficients)]))
MultiModel_cameraAccessory_ElasticityCoeff.outputs <- cbind(MultiModel_cameraAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, MultiModel_cameraAccessory_var_list))
colnames(MultiModel_cameraAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

MultiModel_cameraAccessory_ElasticityCoeff.outputs$Direction <- ifelse(MultiModel_cameraAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
MultiModel_cameraAccessory_ElasticityCoeff.outputs
##                                 Variable Elasticity Direction
##1                             product_mrp     1.1543  Positive
##2                 product_procurement_sla     0.0533  Positive
##3 product_analytic_vertical.xCameraTripod    -0.0285  Negative

# Plotting Elasticity
ggplot(MultiModel_cameraAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-1),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("CameraAccessory - Multiplicative Model") +xlab("Variables")


#********************************************************[Multiplicative + Distributive Lag Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1, 2 and 3 weeks lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
MultiLagcameraAccessory_Dataset <- CameraAccessory_final[,-c(21:22,77:88)]

## Replacing 0 value in column with '0.00001' as log(0) is undefined
MultiLagcameraAccessory_Dataset[MultiLagcameraAccessory_Dataset == 0] <- 0.00001

## Tranforming negative values
MultiLagcameraAccessory_Dataset$GMV_lag_1_per <- 1 + MultiLagcameraAccessory_Dataset$GMV_lag_1_per - min(MultiLagcameraAccessory_Dataset$GMV_lag_1_per)
MultiLagcameraAccessory_Dataset$GMV_lag_2_per <- 1 + MultiLagcameraAccessory_Dataset$GMV_lag_2_per - min(MultiLagcameraAccessory_Dataset$GMV_lag_2_per)
MultiLagcameraAccessory_Dataset$GMV_lag_3_per <- 1 + MultiLagcameraAccessory_Dataset$GMV_lag_3_per - min(MultiLagcameraAccessory_Dataset$GMV_lag_3_per)

## Taking log of all variable to buils to Multiplicative model
MultiLagcameraAccessory_Dataset <- log(MultiLagcameraAccessory_Dataset)

## Check variables for linear relationship or multicollinearity
MultiLagcameraAccessory_model <- lm(gmv~.,MultiLagcameraAccessory_Dataset)
alias(MultiLagcameraAccessory_model)

## Discarding variables which were showing linear relationship or multicollinearity
MultiLagcameraAccessory_Dataset <- MultiLagcameraAccessory_Dataset[, -c(54:72)]


### Stepwise Regression to remove insignificant and correlated variables
MultiLagcameraAccessory_Base.Model<- lm(gmv ~ 1 , data= MultiLagcameraAccessory_Dataset)  # base intercept only model
MultiLagcameraAccessory_All.Model <- lm(gmv ~ . , data= MultiLagcameraAccessory_Dataset) # full model with all predictors
MultiLagcameraAccessory_StepModel<- step(MultiLagcameraAccessory_Base.Model, scope = list(lower = MultiLagcameraAccessory_Base.Model, upper = MultiLagcameraAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MultiLagcameraAccessory_ShortListedVariables <- names(unlist(MultiLagcameraAccessory_StepModel[[1]])) # get shortlisted variable.
MultiLagcameraAccessory_ShortListedVariables <- MultiLagcameraAccessory_ShortListedVariables[!MultiLagcameraAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables[using MultiLagcameraAccessory_StepModel]
MultiLagcameraAccessoryMODEL_1 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                       units + product_analytic_vertical.xCameraEyeCup + deliverycdays +
                                       product_analytic_vertical.xTelescope + week + product_analytic_vertical.xFlashShoeAdapter +
                                       SEM_adtock + Sponsorship, data = MultiLagcameraAccessory_Dataset)


summary(MultiLagcameraAccessoryMODEL_1)
vif(MultiLagcameraAccessoryMODEL_1)


## Non-significant p-value columns: product_analytic_vertical.xFlashShoeAdapter, deliverycdays, week
MultiLagcameraAccessoryMODEL_2 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                       units + product_analytic_vertical.xCameraEyeCup + product_analytic_vertical.xTelescope +
                                       SEM_adtock + Sponsorship, data = MultiLagcameraAccessory_Dataset)


summary(MultiLagcameraAccessoryMODEL_2)
vif(MultiLagcameraAccessoryMODEL_2)


## High VIF and insignificant p-value columns: product_analytic_vertical.xTelescope
MultiLagcameraAccessoryMODEL_3 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                       units + product_analytic_vertical.xCameraEyeCup + SEM_adtock + Sponsorship, data = MultiLagcameraAccessory_Dataset)


summary(MultiLagcameraAccessoryMODEL_3)
vif(MultiLagcameraAccessoryMODEL_3)


## Non-significant p-value columns: SEM_adtock, Sponsorship
## Less significant p-value columns: product_analytic_vertical.xCameraEyeCup
MultiLagcameraAccessoryMODEL_4 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                       product_analytic_vertical.xCameraBatteryCharger + product_analytic_vertical.xCameraMount +
                                       units, data = MultiLagcameraAccessory_Dataset)


summary(MultiLagcameraAccessoryMODEL_4)
vif(MultiLagcameraAccessoryMODEL_4)


## High VIF value columns: units, product_analytic_vertical.xCameraMount
MultiLagcameraAccessoryMODEL_5 <- lm(formula = gmv ~ product_mrp + product_procurement_sla + product_analytic_vertical.xCameraTripod +
                                       product_analytic_vertical.xCameraBatteryCharger, data = MultiLagcameraAccessory_Dataset)


summary(MultiLagcameraAccessoryMODEL_5)
vif(MultiLagcameraAccessoryMODEL_5)


## High VIF value column: product_analytic_vertical.xCameraBatteryCharger
MultiLagcameraAccessoryMODEL_6 <- lm(formula = gmv ~ product_mrp + product_procurement_sla +
                                       product_analytic_vertical.xCameraTripod, data = MultiLagcameraAccessory_Dataset)

summary(MultiLagcameraAccessoryMODEL_6)
vif(MultiLagcameraAccessoryMODEL_6)



### CROSS VALIDATION
cv.lm(data = MultiLagcameraAccessory_Dataset, form.lm = MultiLagcameraAccessoryMODEL_6, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_15 <- function(var){
  MultiLagcameraAccessory_ElasticityCoeff <- as.numeric(MultiLagcameraAccessoryMODEL_6$coefficients[var]*mean(MultiLagcameraAccessory_Dataset[,var])/mean(MultiLagcameraAccessory_Dataset$gmv))
  return(MultiLagcameraAccessory_ElasticityCoeff)
}

MultiLagcameraAccessory_var_list <- list()

for(i in 2:length(MultiLagcameraAccessoryMODEL_6$coefficients)){
  MultiLagcameraAccessory_var_list[i-1] <- ElasticityValue_15(names(MultiLagcameraAccessoryMODEL_6$coefficients)[i])
  
}

MultiLagcameraAccessory_ElasticityCoeff.outputs <- data.frame(names(MultiLagcameraAccessoryMODEL_6$coefficients[2:length(MultiLagcameraAccessoryMODEL_6$coefficients)]))
MultiLagcameraAccessory_ElasticityCoeff.outputs <- cbind(MultiLagcameraAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, MultiLagcameraAccessory_var_list))
colnames(MultiLagcameraAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

MultiLagcameraAccessory_ElasticityCoeff.outputs$Direction <- ifelse(MultiLagcameraAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
MultiLagcameraAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                             product_mrp     1.1543  Positive
#2                 product_procurement_sla     0.0533  Positive
#3 product_analytic_vertical.xCameraTripod    -0.0285  Negative

# Plotting Elasticity
ggplot(MultiLagcameraAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("CameraAccessory - Multiplicative and Distributive Lag Model") +xlab("Variables")



################################################################################################################################################################
#                                           :::::::: Modeling [GamingAccessory] ::::::::
################################################################################################################################################################

#********************************************************[Linear Regression Model]
### Preparing dataset
## Discarding lag variables and Moving averages variables
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
LinearReg_gamingAccessory_Dataset <- GamingAccessory_final[,-c(21:22,68:82)]

## Scaling variables
LinearReg_gamingAccessory_Dataset[,2:ncol(LinearReg_gamingAccessory_Dataset)] <- scale(LinearReg_gamingAccessory_Dataset[,2:ncol(LinearReg_gamingAccessory_Dataset)])

### Stepwise Regression to remove insignificant and correlated variables
LinearReg_gamingAccessory_Base.Model<- lm(gmv ~ 1 , data= LinearReg_gamingAccessory_Dataset)  # base intercept only model
LinearReg_gamingAccessory_All.Model <- lm(gmv ~ . , data= LinearReg_gamingAccessory_Dataset) # full model with all predictors
LinearReg_gamingAccessory_StepModel<- step(LinearReg_gamingAccessory_Base.Model, scope = list(lower = LinearReg_gamingAccessory_Base.Model, upper = LinearReg_gamingAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
LinearReg_gamingAccessory_ShortListedVariables <- names(unlist(LinearReg_gamingAccessory_StepModel[[1]])) # get shortlisted variable.
LinearReg_gamingAccessory_ShortListedVariables <- LinearReg_gamingAccessory_ShortListedVariables[!LinearReg_gamingAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using LinearReg_gamingAccessory_StepModel]
LinearReg_gamingAccessoryMODEL_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController +
                                         product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock + Affiliates +
                                         wday.xMonday + Online_Marketing + SpecialSaleDay.xRepublic.Day +
                                         product_analytic_vertical.xGamingKeyboard + SpecialSaleDay.xFHSD +
                                         wday.xTuesday + SpecialSaleDay.xDiwali + SpecialSaleDay.xPacman +
                                         Content_Marketing_adstock + wday.xWednesday + product_procurement_sla,
                                       data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_1)
vif(LinearReg_gamingAccessoryMODEL_1)


## Non-significant p-value columns: product_analytic_vertical.xMotionController, SpecialSaleDay.xFHSD, product_procurement_sla
LinearReg_gamingAccessoryMODEL_2 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock + Affiliates +
                                         wday.xMonday + Online_Marketing + SpecialSaleDay.xRepublic.Day +
                                         product_analytic_vertical.xGamingKeyboard +
                                         wday.xTuesday + SpecialSaleDay.xDiwali + SpecialSaleDay.xPacman +
                                         Content_Marketing_adstock + wday.xWednesday, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_2)
vif(LinearReg_gamingAccessoryMODEL_2)


## Non-significant p-value columns: SpecialSaleDay.xDiwali
## Less Significant p-value columns: wday.xTuesday
LinearReg_gamingAccessoryMODEL_3 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock + Affiliates +
                                         wday.xMonday + Online_Marketing + SpecialSaleDay.xRepublic.Day +
                                         product_analytic_vertical.xGamingKeyboard + SpecialSaleDay.xPacman +
                                         Content_Marketing_adstock + wday.xWednesday, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_3)
vif(LinearReg_gamingAccessoryMODEL_3)


## Non-significant p-value columns: Content_Marketing_adstock
## Less Significant p-value columns: wday.xWednesday
LinearReg_gamingAccessoryMODEL_4 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock + Affiliates +
                                         wday.xMonday + Online_Marketing + SpecialSaleDay.xRepublic.Day +
                                         product_analytic_vertical.xGamingKeyboard + SpecialSaleDay.xPacman, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_4)
vif(LinearReg_gamingAccessoryMODEL_4)


## High VIF and Less Significant p-value columns: Online_Marketing
## Less Significant p-value columns: product_analytic_vertical.xGamingKeyboard, SpecialSaleDay.xRepublic.Day
LinearReg_gamingAccessoryMODEL_5 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock + Affiliates +
                                         wday.xMonday + SpecialSaleDay.xPacman, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_5)
vif(LinearReg_gamingAccessoryMODEL_5)


## Less Significant p-value columns: SpecialSaleDay.xPacman, product_analytic_vertical.xGamingAccessoryKit
LinearReg_gamingAccessoryMODEL_6 <- lm(formula = gmv ~ units + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock + Affiliates +
                                         wday.xMonday, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_6)
vif(LinearReg_gamingAccessoryMODEL_6)


## Less Significant p-value columns: Affiliates
LinearReg_gamingAccessoryMODEL_7 <- lm(formula = gmv ~ units + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock + wday.xMonday, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_7)
vif(LinearReg_gamingAccessoryMODEL_7)


## Less Significant p-value columns: wday.xMonday
LinearReg_gamingAccessoryMODEL_8 <- lm(formula = gmv ~ units + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         SpecialSaleDay.xEid...Rathayatra + Other_adstock, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_8)
vif(LinearReg_gamingAccessoryMODEL_8)

## Trying to remove "SpecialSaleDay.xEid...Rathayatra" variable (as it is having high p-value among variables)
## and then see variation in Adjusted R-squared
LinearReg_gamingAccessoryMODEL_9 <- lm(formula = gmv ~ units + s1_fact.order_payment_type +
                                         product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                         Other_adstock, data = LinearReg_gamingAccessory_Dataset)


summary(LinearReg_gamingAccessoryMODEL_9) # After removing variable very less change in Adjusted R-squared, we're good to go with this model
vif(LinearReg_gamingAccessoryMODEL_9)



### CROSS VALIDATION
cv.lm(data = LinearReg_gamingAccessory_Dataset, form.lm = LinearReg_gamingAccessoryMODEL_9, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_1 <- function(var){
  LinearReg_gamingAccessory_ElasticityCoeff <-as.numeric(LinearReg_gamingAccessoryMODEL_9$coefficients[var]*mean(LinearReg_gamingAccessory_Dataset[,var])/mean(LinearReg_gamingAccessory_Dataset$gmv))
  return(LinearReg_gamingAccessory_ElasticityCoeff)
  
}

LinearReg_homeAudio_var_list <- list()

for(i in 2:length(LinearReg_gamingAccessoryMODEL_9$coefficients)){
  LinearReg_homeAudio_var_list[i-1] <-ElasticityValue_1(names(LinearReg_gamingAccessoryMODEL_9$coefficients)[i])
  
}

LinearReg_gamingAccessory_ElasticityCoeff.outputs <- data.frame(names(LinearReg_gamingAccessoryMODEL_9$coefficients[2:length(LinearReg_gamingAccessoryMODEL_9$coefficients)]))
LinearReg_gamingAccessory_ElasticityCoeff.outputs <- cbind(LinearReg_gamingAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, LinearReg_homeAudio_var_list))
colnames(LinearReg_gamingAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

LinearReg_gamingAccessory_ElasticityCoeff.outputs$Direction <- ifelse(LinearReg_gamingAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
LinearReg_gamingAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                                          units      24.38  Positive
#2                     s1_fact.order_payment_type       4.68  Positive
#3             product_analytic_vertical.xGamePad     -24.06  Negative
#4 product_analytic_vertical.xJoystickGamingWheel       5.55  Positive
#5                                  Other_adstock       1.13  Positive

# Plotting Elasticity
ggplot(LinearReg_gamingAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-3),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
  ggtitle("GamingAccessory - Linear Regression Model") +xlab("Variables")


#********************************************************[Koyck Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1 week lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
KoyckgamingAccessory_Dataset <- GamingAccessory_final[,-c(21:22,68:79, 81:82)]

## Scaling variables
KoyckgamingAccessory_Dataset[,2:ncol(KoyckgamingAccessory_Dataset)] <- scale(KoyckgamingAccessory_Dataset[,2:ncol(KoyckgamingAccessory_Dataset)])

### Stepwise Regression to remove insignificant and correlated variables
KoyckgamingAccessory_Base.Model<- lm(gmv ~ 1 , data= KoyckgamingAccessory_Dataset)  # base intercept only model
KoyckgamingAccessory_All.Model <- lm(gmv ~ . , data= KoyckgamingAccessory_Dataset) # full model with all predictors
KoyckgamingAccessory_StepModel<- step(KoyckgamingAccessory_Base.Model, scope = list(lower =KoyckgamingAccessory_Base.Model, upper = KoyckgamingAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
KoyckgamingAccessory_ShortListedVariables <- names(unlist(KoyckgamingAccessory_StepModel[[1]])) # get shortlisted variable.
KoyckgamingAccessory_ShortListedVariables <- KoyckgamingAccessory_ShortListedVariables[!KoyckgamingAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using KoyckgamingAccessory_StepModel]
KoyckgamingAccessoryMODEL_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController +
                                    product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type +
                                    product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                    SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per + Other_adstock +
                                    wday.xTuesday + Affiliates + SpecialSaleDay.xDiwali + SpecialSaleDay.xRepublic.Day +
                                    SpecialSaleDay.xFHSD + wday.xMonday + Online_Marketing,
                                  data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_1)
vif(KoyckgamingAccessoryMODEL_1)


## High VIF and Non-significant p-value columns:  Online_Marketing, product_analytic_vertical.xGamingAccessoryKit
## Non-significant p-value columns: wday.xMonday
KoyckgamingAccessoryMODEL_2 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController +
                                    s1_fact.order_payment_type +
                                    product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                    SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per + Other_adstock +
                                    wday.xTuesday + Affiliates + SpecialSaleDay.xDiwali + SpecialSaleDay.xRepublic.Day +
                                    SpecialSaleDay.xFHSD, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_2)
vif(KoyckgamingAccessoryMODEL_2)


## Non-significant p-value columns: SpecialSaleDay.xRepublic.Day, SpecialSaleDay.xFHSD, SpecialSaleDay.xDiwali
## Less significant p-value columns: Affiliates
KoyckgamingAccessoryMODEL_3 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController +
                                    s1_fact.order_payment_type + product_analytic_vertical.xGamePad +
                                    product_analytic_vertical.xJoystickGamingWheel + SpecialSaleDay.xEid...Rathayatra +
                                    GMV_lag_1_per + Other_adstock + wday.xTuesday, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_3)
vif(KoyckgamingAccessoryMODEL_3)


## Non-significant p-value columns: wday.xTuesday
KoyckgamingAccessoryMODEL_4 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController +
                                    s1_fact.order_payment_type + product_analytic_vertical.xGamePad +
                                    product_analytic_vertical.xJoystickGamingWheel + SpecialSaleDay.xEid...Rathayatra +
                                    GMV_lag_1_per + Other_adstock, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_4)
vif(KoyckgamingAccessoryMODEL_4)


## Let's try to remove "s1_fact.order_payment_type" variable and then see if Adjusted R-squared vary much or not
KoyckgamingAccessoryMODEL_5 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController +
                                    product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                    SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per + Other_adstock, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_5) # Good to remove that, Adjusted R-squared decreased at third place of decimal.
vif(KoyckgamingAccessoryMODEL_5)


## Again, Let's try to remove "product_analytic_vertical.xMotionController" variable and then see if Adjusted R-squared vary much or not
KoyckgamingAccessoryMODEL_6 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                    SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per + Other_adstock, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_6) # Good to remove that, slight change in Adjusted R-squared
vif(KoyckgamingAccessoryMODEL_6)


## Again, Let's try to remove "Other_adstock" variable and then see if Adjusted R-squared vary much or not
KoyckgamingAccessoryMODEL_7 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                    SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_7) # Good to remove that, slight change in Adjusted R-squared
vif(KoyckgamingAccessoryMODEL_7)


## Discarding "SpecialSaleDay.xEid...Rathayatra" variable and will check Adjusted R-squared
KoyckgamingAccessoryMODEL_8 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                    GMV_lag_1_per, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_8) # Slight change in Adjusted R-squared value
vif(KoyckgamingAccessoryMODEL_8)


## Less significant p-value columns: GMV_lag_1_per
KoyckgamingAccessoryMODEL_9 <- lm(formula = gmv ~ units + product_analytic_vertical.xGamePad +
                                    product_analytic_vertical.xJoystickGamingWheel, data = KoyckgamingAccessory_Dataset)


summary(KoyckgamingAccessoryMODEL_9) # Slight change in Adjusted R-squared value but mean squared error also decreased
vif(KoyckgamingAccessoryMODEL_9)



### CROSS VALIDATION
cv.lm(data = KoyckgamingAccessory_Dataset, form.lm = KoyckgamingAccessoryMODEL_9, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_7 <- function(var){
  KoyckgamingAccessory_ElasticityCoeff <- as.numeric(KoyckgamingAccessoryMODEL_9$coefficients[var]*mean(KoyckgamingAccessory_Dataset[,var])/mean(KoyckgamingAccessory_Dataset$gmv))
  return(KoyckgamingAccessory_ElasticityCoeff)
  
}

KoyckgamingAccessory_var_list <- list()

for(i in 2:length(KoyckgamingAccessoryMODEL_9$coefficients)){
  KoyckgamingAccessory_var_list[i-1] <- ElasticityValue_7(names(KoyckgamingAccessoryMODEL_9$coefficients)[i])
  
}

KoyckgamingAccessory_ElasticityCoeff.outputs <- data.frame(names(KoyckgamingAccessoryMODEL_9$coefficients[2:length(KoyckgamingAccessoryMODEL_9$coefficients)]))
KoyckgamingAccessory_ElasticityCoeff.outputs <- cbind(KoyckgamingAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, KoyckgamingAccessory_var_list))
colnames(KoyckgamingAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

KoyckgamingAccessory_ElasticityCoeff.outputs$Direction <- ifelse(KoyckgamingAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
KoyckgamingAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                                          units      26.20  Positive
#2             product_analytic_vertical.xGamePad     -25.93  Negative
#3 product_analytic_vertical.xJoystickGamingWheel       5.81  Positive

# Plotting Elasticity
ggplot(KoyckgamingAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("GamingAccessory - Koyck Model") +xlab("Variables")

#********************************************************[Multiplicative Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
MultiModel_gamingAccessory_Dataset <- GamingAccessory_final[,-c(21:22,68:82)]

## Replacing 0 value in column with '0.00001' as log(0) is undefined
MultiModel_gamingAccessory_Dataset[MultiModel_gamingAccessory_Dataset == 0] <- 0.00001

## Taking log of all variable to buils to Multiplicative model
MultiModel_gamingAccessory_Dataset <- log(MultiModel_gamingAccessory_Dataset)


### Stepwise Regression to remove insignificant and correlated variables
MultiModel_gamingAccessory_Base.Model<- lm(gmv ~ 1 , data= MultiModel_gamingAccessory_Dataset)  # base intercept only model
MultiModel_gamingAccessory_All.Model <- lm(gmv ~ . , data= MultiModel_gamingAccessory_Dataset) # full model with all predictors
MultiModel_gamingAccessory_StepModel<- step(MultiModel_gamingAccessory_Base.Model, scope = list(lower = MultiModel_gamingAccessory_Base.Model, upper = MultiModel_gamingAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MultiModel_gamingAccessory_ShortListedVariables <- names(unlist(MultiModel_gamingAccessory_StepModel[[1]])) # get shortlisted variable.
MultiModel_gamingAccessory_ShortListedVariables <- MultiModel_gamingAccessory_ShortListedVariables[!MultiModel_gamingAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using MultiModel_gamingAccessory_StepModel]
MultiModel_gamingAccessoryMODEL_1 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset +
                                          SpecialSaleDay.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker +
                                          SpecialSaleDay.xChristmas...New.Year + Content_Marketing_adstock +
                                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount +
                                          product_analytic_vertical.xGamingKeyboard + SpecialSaleDay.xBSD +
                                          Other_adstock + product_analytic_vertical.xGamingMemoryCard +
                                          SpecialSaleDay.xPacman + product_analytic_vertical.xTVOutCableAccessory +
                                          Digital, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_1)
vif(MultiModel_gamingAccessoryMODEL_1)


## High VIF and insignificant p-value columns: Digital, product_analytic_vertical.xGamingMemoryCard, product_analytic_vertical.xTVOutCableAccessory
## Non-significant p-value columns: SpecialSaleDay.xPacman
MultiModel_gamingAccessoryMODEL_2 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset +
                                          SpecialSaleDay.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker +
                                          SpecialSaleDay.xChristmas...New.Year + Content_Marketing_adstock +
                                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount +
                                          product_analytic_vertical.xGamingKeyboard + SpecialSaleDay.xBSD + Other_adstock, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_2)
vif(MultiModel_gamingAccessoryMODEL_2)

## High VIF and Less significant p-value columns: Other_adstock
MultiModel_gamingAccessoryMODEL_3 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset +
                                          SpecialSaleDay.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker +
                                          SpecialSaleDay.xChristmas...New.Year + Content_Marketing_adstock +
                                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount +
                                          product_analytic_vertical.xGamingKeyboard + SpecialSaleDay.xBSD, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_3)
vif(MultiModel_gamingAccessoryMODEL_3)


## Isignificant p-value columns: SpecialSaleDay.xBSD
## High VIF and Less significant p-value columns: product_analytic_vertical.xGamingKeyboard
MultiModel_gamingAccessoryMODEL_4 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset +
                                          SpecialSaleDay.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker +
                                          SpecialSaleDay.xChristmas...New.Year + Content_Marketing_adstock +
                                          Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock + product_analytic_vertical.xGameControlMount, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_4)
vif(MultiModel_gamingAccessoryMODEL_4)


## Less significant p-vale columns: product_analytic_vertical.xGameControlMount, pecial_sale_day.xChristmas...New.Year
MultiModel_gamingAccessoryMODEL_5 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset +
                                          SpecialSaleDay.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker +
                                          Content_Marketing_adstock + Content_Marketing + deliverybdays + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_5)
vif(MultiModel_gamingAccessoryMODEL_5)


## Slightly high VIF and Non-significant p-value columns: deliverybdays
MultiModel_gamingAccessoryMODEL_6 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xThursday + wday.xMonday + product_analytic_vertical.xGamingHeadset +
                                          SpecialSaleDay.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker +
                                          Content_Marketing_adstock + Content_Marketing + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_6)
vif(MultiModel_gamingAccessoryMODEL_6)


## High VIF and Non-significant p-value columns: wday.xThursday, Content_Marketing_adstock
MultiModel_gamingAccessoryMODEL_7 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xMonday + product_analytic_vertical.xGamingHeadset +
                                          SpecialSaleDay.xValentine.Day + wday.xSaturday + product_analytic_vertical.xGamingSpeaker +
                                          Content_Marketing + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_7)
vif(MultiModel_gamingAccessoryMODEL_7)


## Slightly high VIF and insignificant p-value columns: wday.xSaturday
## Less significant p-value columns: product_analytic_vertical.xGamingSpeaker
MultiModel_gamingAccessoryMODEL_8 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          wday.xMonday + product_analytic_vertical.xGamingHeadset + SpecialSaleDay.xValentine.Day +
                                          Content_Marketing + product_analytic_vertical.xGamingAdapter +
                                          sla + Digital_adstock, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_8)
vif(MultiModel_gamingAccessoryMODEL_8)


## Less significant p-value columns: SpecialSaleDay.xValentine.Day, Digital_adstock
## High VIF and less significant p-value columns: wday.xMonday
MultiModel_gamingAccessoryMODEL_9 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                          product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                          product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                          Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                          product_analytic_vertical.xGamingHeadset +
                                          Content_Marketing + product_analytic_vertical.xGamingAdapter + sla, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_9)
vif(MultiModel_gamingAccessoryMODEL_9)


## Less significant p-value columns: Content_Marketing
MultiModel_gamingAccessoryMODEL_10 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                           Other + SpecialSaleDay.xEid...Rathayatra + product_analytic_vertical.xMotionController +
                                           product_analytic_vertical.xGamingHeadset +
                                           product_analytic_vertical.xGamingAdapter + sla, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_10)
vif(MultiModel_gamingAccessoryMODEL_10)


## Non-significant p-value columns: SpecialSaleDay.xEid...Rathayatra
MultiModel_gamingAccessoryMODEL_11 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                           Other + product_analytic_vertical.xMotionController +
                                           product_analytic_vertical.xGamingHeadset +
                                           product_analytic_vertical.xGamingAdapter + sla, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_11)
vif(MultiModel_gamingAccessoryMODEL_11)


## Non-significant p-value columns: product_analytic_vertical.xGamingHeadset
MultiModel_gamingAccessoryMODEL_12 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                           Other + product_analytic_vertical.xMotionController +
                                           product_analytic_vertical.xGamingAdapter + sla, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_12)
vif(MultiModel_gamingAccessoryMODEL_12)


## Slightly high VIF and Non-significant p-value columns: sla, Other
MultiModel_gamingAccessoryMODEL_13 <- lm(formula = gmv ~ units + per_order + SpecialSaleDay.xRakshabandhan +
                                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                           product_analytic_vertical.xMotionController + product_analytic_vertical.xGamingAdapter, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_13)
vif(MultiModel_gamingAccessoryMODEL_13)


## High VIF columns: SpecialSaleDay.xRakshabandhan
MultiModel_gamingAccessoryMODEL_14 <- lm(formula = gmv ~ units + per_order +
                                           product_analytic_vertical.xJoystickGamingWheel + s1_fact.order_payment_type +
                                           product_analytic_vertical.xGamingAccessoryKit + product_analytic_vertical.xGamePad +
                                           product_analytic_vertical.xMotionController + product_analytic_vertical.xGamingAdapter, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_14) ## Adjusted R-squared very slightly changes (at third decimal place)
vif(MultiModel_gamingAccessoryMODEL_14)


## High VIF and insignificant p-value columns: product_analytic_vertical.xGamingAdapter, product_analytic_vertical.xMotionController
##                                             product_analytic_vertical.xGamingAccessoryKit
MultiModel_gamingAccessoryMODEL_15 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xJoystickGamingWheel +
                                           s1_fact.order_payment_type + product_analytic_vertical.xGamePad, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_15)
vif(MultiModel_gamingAccessoryMODEL_15)


## High VIF columns: product_analytic_vertical.xGamePad, product_analytic_vertical.xJoystickGamingWheel, s1_fact.order_payment_type
MultiModel_gamingAccessoryMODEL_16 <- lm(formula = gmv ~ units + per_order, data = MultiModel_gamingAccessory_Dataset)

summary(MultiModel_gamingAccessoryMODEL_16)
vif(MultiModel_gamingAccessoryMODEL_16)



### CROSS VALIDATION
cv.lm(data = MultiModel_gamingAccessory_Dataset, form.lm = MultiModel_gamingAccessoryMODEL_16, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_4 <- function(var){
  MultiModel_gamingAccessory_ElasticityCoeff <-as.numeric(MultiModel_gamingAccessoryMODEL_16$coefficients[var]*mean(MultiModel_gamingAccessory_Dataset[,var])/mean(MultiModel_gamingAccessory_Dataset$gmv))
  return(MultiModel_gamingAccessory_ElasticityCoeff)
  
}

MultiModel_gamingAccessory_var_list <- list()

for(i in 2:length(MultiModel_gamingAccessoryMODEL_16$coefficients)){
  MultiModel_gamingAccessory_var_list[i-1] <- ElasticityValue_4(names(MultiModel_gamingAccessoryMODEL_16$coefficients)[i])
  
}

MultiModel_gamingAccessory_ElasticityCoeff.outputs <- data.frame(names(MultiModel_gamingAccessoryMODEL_16$coefficients[2:length(MultiModel_gamingAccessoryMODEL_16$coefficients)]))
MultiModel_gamingAccessory_ElasticityCoeff.outputs <- cbind(MultiModel_gamingAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, MultiModel_gamingAccessory_var_list))
colnames(MultiModel_gamingAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

MultiModel_gamingAccessory_ElasticityCoeff.outputs$Direction <- ifelse(MultiModel_gamingAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
MultiModel_gamingAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1     units     0.5009  Positive
#2 per_order    -0.0189  Negative

# Plotting Elasticity
ggplot(MultiModel_gamingAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5), color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
  ggtitle("GamingAccessory - Multiplicative Model") +xlab("Variables")


#********************************************************[Distributive Lag Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1, 2 and 3 weeks lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
DistriLaggamingAccessory_Dataset <- GamingAccessory_final[,-c(21:22,68:79)]

## Scaling variables
DistriLaggamingAccessory_Dataset[,2:ncol(DistriLaggamingAccessory_Dataset)] <- scale(DistriLaggamingAccessory_Dataset[,2:ncol(DistriLaggamingAccessory_Dataset)])

### Stepwise Regression to remove insignificant and correlated variables
DistriLaggamingAccessory_Base.Model<- lm(gmv ~ 1 , data= DistriLaggamingAccessory_Dataset)  # base intercept only model
DistriLaggamingAccessory_All.Model <- lm(gmv ~ . , data= DistriLaggamingAccessory_Dataset) # full model with all predictors
DistriLaggamingAccessory_StepModel<- step(DistriLaggamingAccessory_Base.Model, scope = list(lower =DistriLaggamingAccessory_Base.Model, upper = DistriLaggamingAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
DistriLaggamingAccessory_ShortListedVariables <- names(unlist(DistriLaggamingAccessory_StepModel[[1]])) # get shortlisted variable.
DistriLaggamingAccessory_ShortListedVariables <- DistriLaggamingAccessory_ShortListedVariables[!DistriLaggamingAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using DistriLaggamingAccessory_StepModel]
DistriLaggamingAccessoryMODEL_1 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController +
                                        product_analytic_vertical.xGamingAccessoryKit + s1_fact.order_payment_type +
                                        product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                        SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per + Other_adstock +
                                        wday.xTuesday + Affiliates + SpecialSaleDay.xDiwali + SpecialSaleDay.xRepublic.Day +
                                        SpecialSaleDay.xFHSD + wday.xMonday + Online_Marketing,
                                      data = DistriLaggamingAccessory_Dataset)

summary(DistriLaggamingAccessoryMODEL_1)
vif(DistriLaggamingAccessoryMODEL_1)


## High VIF and Non-significant p-value columns: Online_Marketing, product_analytic_vertical.xGamingAccessoryKit
## Non-significant p-value columns: wday.xMonday, SpecialSaleDay.xRepublic.Day, SpecialSaleDay.xFHSD, SpecialSaleDay.xDiwali
DistriLaggamingAccessoryMODEL_2 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type +
                                        product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                        SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per + Other_adstock +
                                        wday.xTuesday + Affiliates, data = DistriLaggamingAccessory_Dataset)

summary(DistriLaggamingAccessoryMODEL_2)
vif(DistriLaggamingAccessoryMODEL_2)


## Less significant p-value columns: Affiliates, wday.xTuesday
DistriLaggamingAccessoryMODEL_3 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type +
                                        product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                        SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per + Other_adstock, data = DistriLaggamingAccessory_Dataset)

summary(DistriLaggamingAccessoryMODEL_3)
vif(DistriLaggamingAccessoryMODEL_3)


## Discarding 'Other_adstock' column and then check whether Adjusted R-squared value is chanf=ging or not
DistriLaggamingAccessoryMODEL_4 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type +
                                        product_analytic_vertical.xGamePad + product_analytic_vertical.xJoystickGamingWheel +
                                        SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per, data = DistriLaggamingAccessory_Dataset)

summary(DistriLaggamingAccessoryMODEL_4) # Adjusted R-squared is changes at third place of decimal i.e. very less change
vif(DistriLaggamingAccessoryMODEL_4)


## Discarding 'product_analytic_vertical.xJoystickGamingWheel' variable
DistriLaggamingAccessoryMODEL_5 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type +
                                        product_analytic_vertical.xGamePad +
                                        SpecialSaleDay.xEid...Rathayatra + GMV_lag_1_per, data = DistriLaggamingAccessory_Dataset)

summary(DistriLaggamingAccessoryMODEL_5)
vif(DistriLaggamingAccessoryMODEL_5)


## Trying to remove "SpecialSaleDay.xEid...Rathayatra" variable and will check Adjusted R-squared value
DistriLaggamingAccessoryMODEL_6 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type +
                                        product_analytic_vertical.xGamePad + GMV_lag_1_per, data = DistriLaggamingAccessory_Dataset)

summary(DistriLaggamingAccessoryMODEL_6)  # Slight change in Adjusted R-squared value
vif(DistriLaggamingAccessoryMODEL_6)


## Slightly less significant p-value columns: GMV_lag_1_per
DistriLaggamingAccessoryMODEL_7 <- lm(formula = gmv ~ units + product_analytic_vertical.xMotionController + s1_fact.order_payment_type +
                                        product_analytic_vertical.xGamePad, data = DistriLaggamingAccessory_Dataset)

summary(DistriLaggamingAccessoryMODEL_7)
vif(DistriLaggamingAccessoryMODEL_7)



### CROSS VALIDATION
cv.lm(data = DistriLaggamingAccessory_Dataset, form.lm = DistriLaggamingAccessoryMODEL_7, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_10 <- function(var){
  DistriLaggamingAccessory_ElasticityCoeff <- as.numeric(DistriLaggamingAccessoryMODEL_7$coefficients[var]*mean(DistriLaggamingAccessory_Dataset[,var])/mean(DistriLaggamingAccessory_Dataset$gmv))
  return(DistriLaggamingAccessory_ElasticityCoeff)
  
}

DistriLaggamingAccessory_var_list <- list()

for(i in 2:length(DistriLaggamingAccessoryMODEL_7$coefficients)){
  DistriLaggamingAccessory_var_list[i-1] <- ElasticityValue_10(names(DistriLaggamingAccessoryMODEL_7$coefficients)[i])
  
}

DistriLaggamingAccessory_ElasticityCoeff.outputs <- data.frame(names(DistriLaggamingAccessoryMODEL_7$coefficients[2:length(DistriLaggamingAccessoryMODEL_7$coefficients)]))
DistriLaggamingAccessory_ElasticityCoeff.outputs <- cbind(DistriLaggamingAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, DistriLaggamingAccessory_var_list))
colnames(DistriLaggamingAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

DistriLaggamingAccessory_ElasticityCoeff.outputs$Direction <- ifelse(DistriLaggamingAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
DistriLaggamingAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                                       units     25.476  Positive
#2 product_analytic_vertical.xMotionController     -0.941  Negative
#3                  s1_fact.order_payment_type      4.946  Positive
#4          product_analytic_vertical.xGamePad    -17.999  Negative

# Plotting Elasticity
ggplot(DistriLaggamingAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("GamingAccessory - Distributive Lag Model") +xlab("Variables")





#********************************************************[Multiplicative + DIstributive Lag Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1, 2 and 3 weeks lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
MultiLaggamingAccessory_Dataset <- GamingAccessory_final[,-c(21:22,68:79)]

## Replacing 0 value in column with '0.00001' as log(0) is undefined
MultiLaggamingAccessory_Dataset[MultiLaggamingAccessory_Dataset == 0] <- 0.00001

## Tranforming negative values
MultiLaggamingAccessory_Dataset$GMV_lag_1_per <- 1 + MultiLaggamingAccessory_Dataset$GMV_lag_1_per - min(MultiLaggamingAccessory_Dataset$GMV_lag_1_per)
MultiLaggamingAccessory_Dataset$GMV_lag_2_per <- 1 + MultiLaggamingAccessory_Dataset$GMV_lag_2_per - min(MultiLaggamingAccessory_Dataset$GMV_lag_2_per)
MultiLaggamingAccessory_Dataset$GMV_lag_3_per <- 1 + MultiLaggamingAccessory_Dataset$GMV_lag_3_per - min(MultiLaggamingAccessory_Dataset$GMV_lag_3_per)

## Taking log of all variable to buils to Multiplicative model
MultiLaggamingAccessory_Dataset <- log(MultiLaggamingAccessory_Dataset)

## Check variables for linear relationship or multicollinearity
MultiLaggamingAccessory_model <- lm(gmv~.,MultiLaggamingAccessory_Dataset)
alias(MultiLaggamingAccessory_model)

## Discarding variables which were showing linear relationship or multicollinearity
MultiLaggamingAccessory_Dataset <- MultiLaggamingAccessory_Dataset[, -c(55:68)]


### Stepwise Regression to remove insignificant and correlated variables
MultiLaggamingAccessory_Base.Model<- lm(gmv ~ 1 , data= MultiLaggamingAccessory_Dataset)  # base intercept only model
MultiLaggamingAccessory_All.Model <- lm(gmv ~ . , data= MultiLaggamingAccessory_Dataset) # full model with all predictors
MultiLaggamingAccessory_StepModel<- step(MultiLaggamingAccessory_Base.Model, scope = list(lower = MultiLaggamingAccessory_Base.Model, upper = MultiLaggamingAccessory_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MultiLaggamingAccessory_ShortListedVariables <- names(unlist(MultiLaggamingAccessory_StepModel[[1]])) # get shortlisted variable.
MultiLaggamingAccessory_ShortListedVariables <- MultiLaggamingAccessory_ShortListedVariables[!MultiLaggamingAccessory_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using MultiLaggamingAccessory_StepModel]
MultiLaggamingAccessoryMODEL_1 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xMotionController +
                                       SpecialSaleDay.xBSD + product_analytic_vertical.xGamingAccessoryKit +
                                       TV_adstock + product_analytic_vertical.xGamingMousePad +
                                       product_analytic_vertical.xGamingAdapter + sla + SEM_adtock +
                                       Other + SEM + week + product_analytic_vertical.xGamingMemoryCard +
                                       Total_Investment + holidayCountInfo + isASaleDayOrNot +
                                       Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_1)
vif(MultiLaggamingAccessoryMODEL_1)


## High VIF and Non-significant p-value columns: product_analytic_vertical.xGamingMemoryCard
## Non-significant p-value columns: week
MultiLaggamingAccessoryMODEL_2 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xMotionController +
                                       SpecialSaleDay.xBSD + product_analytic_vertical.xGamingAccessoryKit +
                                       TV_adstock + product_analytic_vertical.xGamingMousePad +
                                       product_analytic_vertical.xGamingAdapter + sla + SEM_adtock +
                                       Other + SEM + Total_Investment + holidayCountInfo + isASaleDayOrNot +
                                       Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_2)
vif(MultiLaggamingAccessoryMODEL_2)


## Non-significant p-value columns: SpecialSaleDay.xBSD
MultiLaggamingAccessoryMODEL_3 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xMotionController +
                                       product_analytic_vertical.xGamingAccessoryKit +
                                       TV_adstock + product_analytic_vertical.xGamingMousePad +
                                       product_analytic_vertical.xGamingAdapter + sla + SEM_adtock +
                                       Other + SEM + Total_Investment + holidayCountInfo + isASaleDayOrNot +
                                       Content_Marketing, data = MultiLaggamingAccessory_Dataset)

summary(MultiLaggamingAccessoryMODEL_3)
vif(MultiLaggamingAccessoryMODEL_3)


## High VIF and Non-significant p-value columns: product_analytic_vertical.xGamingMousePad
## Non-significant p-value columns: holidayCountInfo
MultiLaggamingAccessoryMODEL_4 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xMotionController +
                                       product_analytic_vertical.xGamingAccessoryKit + TV_adstock +
                                       product_analytic_vertical.xGamingAdapter + sla + SEM_adtock +
                                       Other + SEM + Total_Investment + isASaleDayOrNot +
                                       Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_4)
vif(MultiLaggamingAccessoryMODEL_4)


## Non-significant p-value columns: isASaleDayOrNot
MultiLaggamingAccessoryMODEL_5 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xMotionController +
                                       product_analytic_vertical.xGamingAccessoryKit + TV_adstock +
                                       product_analytic_vertical.xGamingAdapter + sla + SEM_adtock +
                                       Other + SEM + Total_Investment + Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_5)
vif(MultiLaggamingAccessoryMODEL_5)


## Non-significant p-value columns: sla
## High VIF and less significant p-value columns: Total_Investment
MultiLaggamingAccessoryMODEL_6 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xMotionController +
                                       product_analytic_vertical.xGamingAccessoryKit + TV_adstock +
                                       product_analytic_vertical.xGamingAdapter + SEM_adtock +
                                       Other + SEM + Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_6)
vif(MultiLaggamingAccessoryMODEL_6)


## Slightly High VIF and Non-significant p-value columns: SEM, TV_adstock
MultiLaggamingAccessoryMODEL_7 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xMotionController +
                                       product_analytic_vertical.xGamingAccessoryKit +
                                       product_analytic_vertical.xGamingAdapter + SEM_adtock +
                                       Other + Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_7)
vif(MultiLaggamingAccessoryMODEL_7)


## High VIF value columns: product_analytic_vertical.xGamingAdapter, product_analytic_vertical.xMotionController
MultiLaggamingAccessoryMODEL_8 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       wday.xSunday + product_analytic_vertical.xGamingAccessoryKit +
                                       SEM_adtock + Other + Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_8)
vif(MultiLaggamingAccessoryMODEL_8)


## Non-significant p-value columns: SEM_adtock
## Less significant p-value columns: Other, wday.xSunday
MultiLaggamingAccessoryMODEL_9 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                       product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                       product_analytic_vertical.xGamingAccessoryKit + Content_Marketing, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_9)
vif(MultiLaggamingAccessoryMODEL_9)


## Non-significant p-value columns: Content_Marketing
MultiLaggamingAccessoryMODEL_10 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                        product_analytic_vertical.xJoystickGamingWheel + product_procurement_sla +
                                        product_analytic_vertical.xGamingAccessoryKit, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_10)
vif(MultiLaggamingAccessoryMODEL_10)


## High VIF value columns: product_analytic_vertical.xGamingAccessoryKit, product_analytic_vertical.xJoystickGamingWheel
MultiLaggamingAccessoryMODEL_11 <- lm(formula = gmv ~ units + per_order + product_analytic_vertical.xGamingHeadset +
                                        product_procurement_sla, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_11) # Very slight change in Adjusted R-squared at 3rd place of decimal
vif(MultiLaggamingAccessoryMODEL_11)


## High VIF value columns: product_analytic_vertical.xGamingHeadset
MultiLaggamingAccessoryMODEL_12 <- lm(formula = gmv ~ units + per_order + product_procurement_sla, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_12) # Slight change in Adjusted R-squared at third place of decimal, good to remove taht column
vif(MultiLaggamingAccessoryMODEL_12)


## Less significant p-value columns: product_procurement_sla
MultiLaggamingAccessoryMODEL_13 <- lm(formula = gmv ~ units + per_order, data = MultiLaggamingAccessory_Dataset)


summary(MultiLaggamingAccessoryMODEL_13) # Slight change in Adjusted R-squared at third place of decimal, good to remove taht column
vif(MultiLaggamingAccessoryMODEL_13)



### CROSS VALIDATION
cv.lm(data = MultiLaggamingAccessory_Dataset, form.lm = MultiLaggamingAccessoryMODEL_13, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_13 <- function(var){
  MultiLaggamingAccessory_ElasticityCoeff <- as.numeric(MultiLaggamingAccessoryMODEL_13$coefficients[var]*mean(MultiLaggamingAccessory_Dataset[,var])/mean(MultiLaggamingAccessory_Dataset$gmv))
  return(MultiLaggamingAccessory_ElasticityCoeff)
  
}

MultiLaggamingAccessory_var_list <- list()

for(i in 2:length(MultiLaggamingAccessoryMODEL_13$coefficients)){
  MultiLaggamingAccessory_var_list[i-1] <- ElasticityValue_13(names(MultiLaggamingAccessoryMODEL_13$coefficients)[i])
  
}

MultiLaggamingAccessory_ElasticityCoeff.outputs <- data.frame(names(MultiLaggamingAccessoryMODEL_13$coefficients[2:length(MultiLaggamingAccessoryMODEL_13$coefficients)]))
MultiLaggamingAccessory_ElasticityCoeff.outputs <- cbind(MultiLaggamingAccessory_ElasticityCoeff.outputs,do.call(rbind.data.frame, MultiLaggamingAccessory_var_list))
colnames(MultiLaggamingAccessory_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

MultiLaggamingAccessory_ElasticityCoeff.outputs$Direction <- ifelse(MultiLaggamingAccessory_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
MultiLaggamingAccessory_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1     units     0.5009  Positive
#2 per_order    -0.0189  Negative

# Plotting Elasticity
ggplot(MultiLaggamingAccessory_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("GamingAccessory - Multiplicative and Distributive Lag Model") +xlab("Variables")


################################################################################################################################################################
#                                                 :::::::: Modeling [HomeAudio] ::::::::
################################################################################################################################################################

#********************************************************[Linear Regression Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
LinearReg_homeAudio_Dataset <- HomeAudio_final[,-c(21:22,63:77)]

## Scaling variables
LinearReg_homeAudio_Dataset[,2:ncol(LinearReg_homeAudio_Dataset)] <- scale(LinearReg_homeAudio_Dataset[,2:ncol(LinearReg_homeAudio_Dataset)])

### Stepwise Regression to remove insignificant and correlated variables
LinearReg_homeAudio_Base.Model<- lm(gmv ~ 1 , data= LinearReg_homeAudio_Dataset)  # base intercept only model
LinearReg_homeAudio_All.Model <- lm(gmv ~ . , data= LinearReg_homeAudio_Dataset) # full model with all predictors
LinearReg_homeAudio_StepModel<- step(LinearReg_homeAudio_Base.Model, scope = list(lower = LinearReg_homeAudio_Base.Model, upper = LinearReg_homeAudio_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
LinearReg_homeAudio_ShortListedVariables <- names(unlist(LinearReg_homeAudio_StepModel[[1]])) # get shortlisted variable.
LinearReg_homeAudio_ShortListedVariables <- LinearReg_homeAudio_ShortListedVariables[!LinearReg_homeAudio_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using LinearReg_homeAudio_StepModel]
LinearReg_homeAudioMODEL_1 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation +
                                   Digital_adstock + sla + price_tag.xMass_Product + product_analytic_vertical.xKaraokePlayer +
                                   SpecialSaleDay.xEid...Rathayatra + week + deliverybdays +
                                   TV_adstock + product_analytic_vertical.xDJController + Content_Marketing +
                                   TV, data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_1)
vif(LinearReg_homeAudioMODEL_1)


## High VIF and Non-significant p-value columns: price_tag.xMass_Product, TV
## Non-significant p-value columns: SpecialSaleDay.xEid...Rathayatra
LinearReg_homeAudioMODEL_2 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation +
                                   Digital_adstock + sla + product_analytic_vertical.xKaraokePlayer +
                                   week + deliverybdays + TV_adstock + product_analytic_vertical.xDJController +
                                   Content_Marketing, data = LinearReg_homeAudio_Dataset)

summary(LinearReg_homeAudioMODEL_2)
vif(LinearReg_homeAudioMODEL_2)


## Slightly High VIF and Non-significant p-value columns: Content_Marketing
## Less significant p-value columns: sla
LinearReg_homeAudioMODEL_3 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation +
                                   Digital_adstock + product_analytic_vertical.xKaraokePlayer + week + deliverybdays +
                                   TV_adstock + product_analytic_vertical.xDJController , data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_3)
vif(LinearReg_homeAudioMODEL_3)


## Less significant p-value columns: TV_adstock
LinearReg_homeAudioMODEL_4 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation +
                                   Digital_adstock + product_analytic_vertical.xKaraokePlayer + week + deliverybdays +
                                   product_analytic_vertical.xDJController , data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_4)
vif(LinearReg_homeAudioMODEL_4)


## Slightly higher VIF and less significant p-value columns: deliverybdays
LinearReg_homeAudioMODEL_5 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation +
                                   Digital_adstock + product_analytic_vertical.xKaraokePlayer + week +
                                   product_analytic_vertical.xDJController , data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_5)
vif(LinearReg_homeAudioMODEL_5)


## Non-significant p-value columns: week, product_analytic_vertical.xDJController
LinearReg_homeAudioMODEL_6 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation +
                                   Digital_adstock + product_analytic_vertical.xKaraokePlayer, data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_6)
vif(LinearReg_homeAudioMODEL_6)


## Non-significant p-value columns: product_analytic_vertical.xKaraokePlayer
LinearReg_homeAudioMODEL_7 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates + product_analytic_vertical.xDockingStation +
                                   Digital_adstock, data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_7)
vif(LinearReg_homeAudioMODEL_7)


## Less significant p-value columns: Digital_adstock, product_analytic_vertical.xDockingStation
LinearReg_homeAudioMODEL_8 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates, data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_8)
vif(LinearReg_homeAudioMODEL_8)


## Trying to remove "product_mrp" [as it is having high VIF] and then see change in Adjusted R-squared
LinearReg_homeAudioMODEL_9 <- lm(formula = gmv ~ units + SpecialSaleDay.xDaussera +
                                   product_procurement_sla + Affiliates, data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_9)  # Adjusted R-squared value is changed at 3rd place of decimal, we're good to remove variable
# We also tried to remove "units" variable but change in Adjusted R-squared value was more
vif(LinearReg_homeAudioMODEL_9)


## Non-significant p-value columns: Affiliates, product_procurement_sla
LinearReg_homeAudioMODEL_10 <- lm(formula = gmv ~ units + SpecialSaleDay.xDaussera, data = LinearReg_homeAudio_Dataset)


summary(LinearReg_homeAudioMODEL_10)
vif(LinearReg_homeAudioMODEL_10)




### CROSS VALIDATION
cv.lm(data = LinearReg_homeAudio_Dataset, form.lm = LinearReg_homeAudioMODEL_10, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_2 <- function(var){
  LinearReg_homeAudio_ElasticityCoeff <-as.numeric(LinearReg_homeAudioMODEL_10$coefficients[var]*mean(LinearReg_homeAudio_Dataset[,var])/mean(LinearReg_homeAudio_Dataset$gmv))
  return(LinearReg_homeAudio_ElasticityCoeff)
  
}

LinearReg_homeAudio_var_list <- list()

for(i in 2:length(LinearReg_homeAudioMODEL_10$coefficients)){
  LinearReg_homeAudio_var_list[i-1] <-ElasticityValue_2(names(LinearReg_homeAudioMODEL_10$coefficients)[i])
  
}

LinearReg_homeAudio_ElasticityCoeff.outputs <- data.frame(names(LinearReg_homeAudioMODEL_10$coefficients[2:length(LinearReg_homeAudioMODEL_10$coefficients)]))
LinearReg_homeAudio_ElasticityCoeff.outputs <- cbind(LinearReg_homeAudio_ElasticityCoeff.outputs,do.call(rbind.data.frame, LinearReg_homeAudio_var_list))
colnames(LinearReg_homeAudio_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

LinearReg_homeAudio_ElasticityCoeff.outputs$Direction <- ifelse(LinearReg_homeAudio_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
LinearReg_homeAudio_ElasticityCoeff.outputs

#Variable Elasticity Direction
#1                      units   -0.84250  Negative
#2 SpecialSaleDay.xDaussera   -0.00636  Negative
# Plotting Elasticity
ggplot(LinearReg_homeAudio_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity", width = 0.9) + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y= 0.5),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank()) +
  ggtitle("HomeAudio - Linear Regression Model") +xlab("Variables")



#********************************************************[Koyck Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1 week lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
KoyckhomeAudio_Dataset <- HomeAudio_final[,-c(21:22,63:74,76:77)]

## Scaling variables
KoyckhomeAudio_Dataset[,2:ncol(KoyckhomeAudio_Dataset)] <- scale(KoyckhomeAudio_Dataset[,2:ncol(KoyckhomeAudio_Dataset)])


### Stepwise Regression to remove insignificant and correlated variables
KoyckhomeAudio_Base.Model<- lm(gmv ~ 1 , data= KoyckhomeAudio_Dataset)  # base intercept only model
KoyckhomeAudio_All.Model <- lm(gmv ~ . , data= KoyckhomeAudio_Dataset) # full model with all predictors
KoyckhomeAudio_StepModel<- step(KoyckhomeAudio_Base.Model, scope = list(lower = KoyckhomeAudio_Base.Model, upper = KoyckhomeAudio_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
KoyckhomeAudio_ShortListedVariables <- names(unlist(KoyckhomeAudio_StepModel[[1]])) # get shortlisted variable.
KoyckhomeAudio_ShortListedVariables <- KoyckhomeAudio_ShortListedVariables[!KoyckhomeAudio_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using KoyckhomeAudio_StepModel]
KoyckhomeAudioMODEL_1 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Affiliates + Digital_adstock +
                              sla + product_analytic_vertical.xKaraokePlayer + SpecialSaleDay.xEid...Rathayatra +
                              week + GMV_lag_1_per + deliverybdays + TV_adstock + Digital +
                              wday.xSaturday + SpecialSaleDay.xBSD + product_analytic_vertical.xHomeAudioSpeaker +
                              wday.xSunday + product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                              SpecialSaleDay.xValentine.Day + Online_Marketing_adstock +
                              Other_adstock + price_tag.xMass_Product + Sponsorship + Content_Marketing_adstock +
                              product_analytic_vertical.xSlingBox, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_1)
vif(KoyckhomeAudioMODEL_1)


## Non-significant p-value columns: product_analytic_vertical.xSlingBox
## High VIF and Non-significant p-value columns: product_analytic_vertical.xHomeAudioSpeaker, wday.xSunday
## Slightly High VIF and Non-significant p-value columns: sla
KoyckhomeAudioMODEL_2 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Affiliates + Digital_adstock +
                              product_analytic_vertical.xKaraokePlayer + SpecialSaleDay.xEid...Rathayatra +
                              week + GMV_lag_1_per + deliverybdays + TV_adstock + Digital + wday.xSaturday +
                              SpecialSaleDay.xBSD + product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                              SpecialSaleDay.xValentine.Day + Online_Marketing_adstock + Other_adstock +
                              price_tag.xMass_Product + Sponsorship + Content_Marketing_adstock, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_2)
vif(KoyckhomeAudioMODEL_2)


## Slightly High VIF and Non-significant p-value columns: wday.xSaturday
## Non-significant p-value columns: SpecialSaleDay.xBSD
## Slightly High VIF and less significant p-value columns: Affiliates
KoyckhomeAudioMODEL_3 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer +
                              SpecialSaleDay.xEid...Rathayatra + week + GMV_lag_1_per + deliverybdays +
                              TV_adstock + Digital + product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                              SpecialSaleDay.xValentine.Day + Online_Marketing_adstock + Other_adstock +
                              price_tag.xMass_Product + Sponsorship + Content_Marketing_adstock, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_3)
vif(KoyckhomeAudioMODEL_3)


## High VIF and Non-significant p-value columns: Online_Marketing_adstock
KoyckhomeAudioMODEL_4 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer +
                              SpecialSaleDay.xEid...Rathayatra + week + GMV_lag_1_per + deliverybdays +
                              TV_adstock + Digital + product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                              SpecialSaleDay.xValentine.Day + Other_adstock + price_tag.xMass_Product + Sponsorship +
                              Content_Marketing_adstock, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_4)
vif(KoyckhomeAudioMODEL_4)


## Slightly High VIF and less significant p-value columns: Sponsorship
## High VIF and less significant p-value columns: Content_Marketing_adstock
KoyckhomeAudioMODEL_5 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer +
                              SpecialSaleDay.xEid...Rathayatra + week + GMV_lag_1_per + deliverybdays +
                              TV_adstock + Digital + product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                              SpecialSaleDay.xValentine.Day + Other_adstock + price_tag.xMass_Product, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_5)
vif(KoyckhomeAudioMODEL_5)


## Non-significant p-value columns: Other_adstock
##Less significant p-value columns: deliverybdays
KoyckhomeAudioMODEL_6 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer +
                              SpecialSaleDay.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock +
                              Digital + product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                              SpecialSaleDay.xValentine.Day + price_tag.xMass_Product, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_6)
vif(KoyckhomeAudioMODEL_6)


## Non-significant p-value columns: SpecialSaleDay.xRepublic.Day
KoyckhomeAudioMODEL_7 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer +
                              SpecialSaleDay.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock +
                              Digital + product_analytic_vertical.xFMRadio +
                              SpecialSaleDay.xValentine.Day + price_tag.xMass_Product, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_7)
vif(KoyckhomeAudioMODEL_7)


## High VIF and Non-significant p-value columns: price_tag.xMass_Product
## Non-significant p-value columns: SpecialSaleDay.xValentine.Day
KoyckhomeAudioMODEL_8 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer +
                              SpecialSaleDay.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock +
                              Digital + product_analytic_vertical.xFMRadio, data = KoyckhomeAudio_Dataset)

summary(KoyckhomeAudioMODEL_8)
vif(KoyckhomeAudioMODEL_8)


## High VIF and Non-significant p-vaue value columns: product_analytic_vertical.xFMRadio
KoyckhomeAudioMODEL_9 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                              product_procurement_sla + Digital_adstock + product_analytic_vertical.xKaraokePlayer +
                              SpecialSaleDay.xEid...Rathayatra + week + GMV_lag_1_per + TV_adstock +
                              Digital, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_9)
vif(KoyckhomeAudioMODEL_9)


## Slightly hogh VIF and Non-significant p-value columns: Digital
## Less significant p-value columns: product_analytic_vertical.xKaraokePlayer
KoyckhomeAudioMODEL_10 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                               product_procurement_sla + Digital_adstock + SpecialSaleDay.xEid...Rathayatra +
                               week + GMV_lag_1_per + TV_adstock, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_10)
vif(KoyckhomeAudioMODEL_10)


## Less significant p-va;ue columns: TV_adstock, Digital_adstock
KoyckhomeAudioMODEL_11 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                               product_procurement_sla + SpecialSaleDay.xEid...Rathayatra +
                               week + GMV_lag_1_per, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_11)
vif(KoyckhomeAudioMODEL_11)


## Non-significant p-value columns: week
## Less significant p-value columns: GMV_lag_1_per, SpecialSaleDay.xEid...Rathayatra
KoyckhomeAudioMODEL_12 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                               product_procurement_sla, data = KoyckhomeAudio_Dataset)


summary(KoyckhomeAudioMODEL_12)
vif(KoyckhomeAudioMODEL_12)


## Less significant p-value columns: product_procurement_sla
KoyckhomeAudioMODEL_13 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera, data = KoyckhomeAudio_Dataset)

summary(KoyckhomeAudioMODEL_13)
vif(KoyckhomeAudioMODEL_13)


## Discarding "product_mrp" variable [as it' is's having high VIF] and will check change in Adjusted R-squared value
KoyckhomeAudioMODEL_14 <- lm(formula = gmv ~ units  + SpecialSaleDay.xDaussera, data = KoyckhomeAudio_Dataset)

summary(KoyckhomeAudioMODEL_14) # Slight change in Adjusted R-squared value at 3rd place of decimal
vif(KoyckhomeAudioMODEL_14)



### CROSS VALIDATION
cv.lm(data = KoyckhomeAudio_Dataset, form.lm = KoyckhomeAudioMODEL_14, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_8 <- function(var){
  KoyckhomeAudio_ElasticityCoeff <- as.numeric(KoyckhomeAudioMODEL_14$coefficients[var]*mean(KoyckhomeAudio_Dataset[,var])/mean(KoyckhomeAudio_Dataset$gmv))
  return(KoyckhomeAudio_ElasticityCoeff)
  
}

KoyckhomeAudio_var_list <- list()

for(i in 2:length(KoyckhomeAudioMODEL_14$coefficients)){
  KoyckhomeAudio_var_list[i-1] <- ElasticityValue_8(names(KoyckhomeAudioMODEL_14$coefficients)[i])
  
}

KoyckhomeAudio_ElasticityCoeff.outputs <- data.frame(names(KoyckhomeAudioMODEL_14$coefficients[2:length(KoyckhomeAudioMODEL_14$coefficients)]))
KoyckhomeAudio_ElasticityCoeff.outputs <- cbind(KoyckhomeAudio_ElasticityCoeff.outputs,do.call(rbind.data.frame, KoyckhomeAudio_var_list))
colnames(KoyckhomeAudio_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

KoyckhomeAudio_ElasticityCoeff.outputs$Direction <- ifelse(KoyckhomeAudio_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
KoyckhomeAudio_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                      units   -0.84250  Negative
#2 SpecialSaleDay.xDaussera   -0.00636  Negative
# Plotting Elasticity
ggplot(KoyckhomeAudio_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Koyck Model") +xlab("Variables")




#********************************************************[Multiplicative Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
MultiModel_homeAudio_Dataset <- HomeAudio_final[,-c(21:22,63:77)]

## Replacing 0 value in column with '0.00001' as log(0) is undefined
MultiModel_homeAudio_Dataset[MultiModel_homeAudio_Dataset == 0] <- 0.00001

## Taking log of all variable to buils to Multiplicative model
MultiModel_homeAudio_Dataset <- log(MultiModel_homeAudio_Dataset)

## Check variables for linear relationship or multicollinearity
MultiModel_homeAudio_model <- lm(gmv~.,MultiModel_homeAudio_Dataset)
alias(MultiModel_homeAudio_model)

## Discarding variables which were showing linear relationship or multicollinearity
MultiModel_homeAudio_Dataset <- MultiModel_homeAudio_Dataset[, -c(51:58)]

### Stepwise Regression to remove insignificant and correlated variables
MultiModel_homeAudio_Base.Model<- lm(gmv ~ 1 , data= MultiModel_homeAudio_Dataset)  # base intercept only model
MultiModel_homeAudio_All.Model <- lm(gmv ~ . , data= MultiModel_homeAudio_Dataset) # full model with all predictors
MultiModel_homeAudio_StepModel<- step(MultiModel_homeAudio_Base.Model, scope = list(lower = MultiModel_homeAudio_Base.Model, upper = MultiModel_homeAudio_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MultiModel_homeAudio_ShortListedVariables <- names(unlist(MultiModel_homeAudio_StepModel[[1]])) # get shortlisted variable.
MultiModel_homeAudio_ShortListedVariables <- MultiModel_homeAudio_ShortListedVariables[!MultiModel_homeAudio_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using MultiModel_homeAudio_StepModel]
MultiModel_homeAudioMODEL_1 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product +
                                    TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    product_analytic_vertical.xSlingBox + sla + wday.xWednesday +
                                    Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController +
                                    wday.xMonday + product_analytic_vertical.xSoundMixer + isASaleDayOrNot +
                                    SpecialSaleDay.xChristmas...New.Year + Online_Marketing +
                                    units + Total_Investment, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_1)
vif(MultiModel_homeAudioMODEL_1)


## High VIF and insignificant p-value columns: Total_Investment, units
MultiModel_homeAudioMODEL_2 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product +
                                    TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    product_analytic_vertical.xSlingBox + sla + wday.xWednesday +
                                    Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController +
                                    wday.xMonday + product_analytic_vertical.xSoundMixer + isASaleDayOrNot +
                                    SpecialSaleDay.xChristmas...New.Year + Online_Marketing, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_2)
vif(MultiModel_homeAudioMODEL_2)


## Non-significant p-value columns: product_analytic_vertical.xSoundMixer
## Less significant p-value columns: isASaleDayOrNot
MultiModel_homeAudioMODEL_3 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product +
                                    TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    product_analytic_vertical.xSlingBox + sla + wday.xWednesday +
                                    Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController +
                                    wday.xMonday + SpecialSaleDay.xChristmas...New.Year + Online_Marketing, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_3)
vif(MultiModel_homeAudioMODEL_3)


## Slightly high VIF and Non-significant p-value columns: SpecialSaleDay.xChristmas...New.Year
## High VIF and Non-significant p-value columns: Online_Marketing
## Non-significant p-value columns: wday.xMonday
MultiModel_homeAudioMODEL_4 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product +
                                    TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    product_analytic_vertical.xSlingBox + sla + wday.xWednesday +
                                    Content_Marketing_adstock + Sponsorship + product_analytic_vertical.xDJController, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_4)
vif(MultiModel_homeAudioMODEL_4)


##High VIF and Non-significant p-value columns: product_analytic_vertical.xDJController
MultiModel_homeAudioMODEL_5 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    SEM_adtock + deliverybdays + wday.xTuesday + price_tag.xPremium_Product +
                                    TV_adstock + wday.xSunday + product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    product_analytic_vertical.xSlingBox + sla + wday.xWednesday +
                                    Content_Marketing_adstock + Sponsorship, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_5)
vif(MultiModel_homeAudioMODEL_5)


## High VIF and Non-significant p-value columns: SEM_adtock
## Less significant p-value columns: price_tag.xPremium_Product
MultiModel_homeAudioMODEL_6 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    deliverybdays + wday.xTuesday + TV_adstock + wday.xSunday +
                                    product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    product_analytic_vertical.xSlingBox + sla + wday.xWednesday +
                                    Content_Marketing_adstock + Sponsorship, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_6)
vif(MultiModel_homeAudioMODEL_6)


## Less significant p-value columns: product_analytic_vertical.xSlingBox
## High VIF and less significant p-value columns: wday.xWednesday
MultiModel_homeAudioMODEL_7 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    deliverybdays + wday.xTuesday + TV_adstock + wday.xSunday +
                                    product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    sla + Content_Marketing_adstock + Sponsorship, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_7)
vif(MultiModel_homeAudioMODEL_7)


## High VIF and Non-significant p-value columns: Content_Marketing_adstock
MultiModel_homeAudioMODEL_8 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + Content_Marketing +
                                    deliverybdays + wday.xTuesday + TV_adstock + wday.xSunday +
                                    product_procurement_sla + product_analytic_vertical.xKaraokePlayer +
                                    sla + Sponsorship, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_8)
vif(MultiModel_homeAudioMODEL_8)


## Non-significant p-value columns: Content_Marketing
## Less significant p-value columns: wday.xSunday, product_procurement_sla, wday.xTuesday
MultiModel_homeAudioMODEL_9 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + deliverybdays +
                                    TV_adstock + product_analytic_vertical.xKaraokePlayer +
                                    sla + Sponsorship, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_9)
vif(MultiModel_homeAudioMODEL_9)


## Less significant p-value columns: product_analytic_vertical.xKaraokePlayer
MultiModel_homeAudioMODEL_10 <- lm(formula = gmv ~ price_tag.xMass_Product + product_mrp + deliverybdays +
                                     TV_adstock + sla + Sponsorship, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_10)
vif(MultiModel_homeAudioMODEL_10)


## Discarding "price_tag.xMass_Product" [as it's having high VIF]
MultiModel_homeAudioMODEL_11 <- lm(formula = gmv ~ product_mrp + deliverybdays +
                                     TV_adstock + sla + Sponsorship, data = MultiModel_homeAudio_Dataset)


summary(MultiModel_homeAudioMODEL_11)  # Adjusted R-squared is changed at 3rd place of decimal, we're good to remove that variable
vif(MultiModel_homeAudioMODEL_11)


## Less significant p-value columns: TV_adstock
MultiModel_homeAudioMODEL_12 <- lm(formula = gmv ~ product_mrp + deliverybdays + sla + Sponsorship, data = MultiModel_homeAudio_Dataset)

summary(MultiModel_homeAudioMODEL_12)
vif(MultiModel_homeAudioMODEL_12)


## Non-significant p-value columns: Sponsorship
MultiModel_homeAudioMODEL_13 <- lm(formula = gmv ~ product_mrp + deliverybdays + sla, data = MultiModel_homeAudio_Dataset)

summary(MultiModel_homeAudioMODEL_13)
vif(MultiModel_homeAudioMODEL_13)



### CROSS VALIDATION
cv.lm(data = MultiModel_homeAudio_Dataset, form.lm = MultiModel_homeAudioMODEL_13, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_5 <- function(var){
  MultiModel_homeAudio_ElasticityCoeff <- as.numeric(MultiModel_homeAudioMODEL_13$coefficients[var]*mean(MultiModel_homeAudio_Dataset[,var])/mean(MultiModel_homeAudio_Dataset$gmv))
  return(MultiModel_homeAudio_ElasticityCoeff)
  
}

MultiModel_homeAudio_var_list <- list()

for(i in 2:length(MultiModel_homeAudioMODEL_13$coefficients)){
  MultiModel_homeAudio_var_list[i-1] <- ElasticityValue_5(names(MultiModel_homeAudioMODEL_13$coefficients)[i])
  
}

MultiModel_homeAudio_ElasticityCoeff.outputs <- data.frame(names(MultiModel_homeAudioMODEL_13$coefficients[2:length(MultiModel_homeAudioMODEL_13$coefficients)]))
MultiModel_homeAudio_ElasticityCoeff.outputs <- cbind(MultiModel_homeAudio_ElasticityCoeff.outputs,do.call(rbind.data.frame, MultiModel_homeAudio_var_list))
colnames(MultiModel_homeAudio_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

MultiModel_homeAudio_ElasticityCoeff.outputs$Direction <- ifelse(MultiModel_homeAudio_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
MultiModel_homeAudio_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1   product_mrp    0.95627  Positive
#2 deliverybdays   -0.00313  Negative
#3           sla    0.04153  Positive
# Plotting Elasticity
ggplot(MultiModel_homeAudio_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust=0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Multiplicative Model") +xlab("Variables")






#********************************************************[Distributive Lag Model]

### Preparing dataset
## Discarding lag variables and Moving averages variables but considering 1, 2 and 3 weeks lag value of 'gmv'
## Also removing "list_price" and "promotional_offer" column as those are on dependent variable (i.e. gmv)
## Retaining those is not good idea as 'gmv' won't present in unseen data
DistriLaghomeAudio_Dataset <- HomeAudio_final[,-c(21:22,63:74)]

## Scaling variables
DistriLaghomeAudio_Dataset[,2:ncol(DistriLaghomeAudio_Dataset)] <- scale(DistriLaghomeAudio_Dataset[,2:ncol(DistriLaghomeAudio_Dataset)])


### Stepwise Regression to remove insignificant and correlated variables
DistriLaghomeAudio_Base.Model<- lm(gmv ~ 1 , data= DistriLaghomeAudio_Dataset)  # base intercept only model
DistriLaghomeAudio_All.Model <- lm(gmv ~ . , data= DistriLaghomeAudio_Dataset) # full model with all predictors
DistriLaghomeAudio_StepModel<- step(DistriLaghomeAudio_Base.Model, scope = list(lower = DistriLaghomeAudio_Base.Model, upper = DistriLaghomeAudio_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
DistriLaghomeAudio_ShortListedVariables <- names(unlist(DistriLaghomeAudio_StepModel[[1]])) # get shortlisted variable.
DistriLaghomeAudio_ShortListedVariables <- DistriLaghomeAudio_ShortListedVariables[!DistriLaghomeAudio_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using DistriLaghomeAudio_StepModel]
DistriLaghomeAudioMODEL_1 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla + product_analytic_vertical.xDockingStation +
                                  Digital_adstock + sla + price_tag.xMass_Product + product_analytic_vertical.xKaraokePlayer +
                                  GMV_lag_2_per + week + deliverybdays + GMV_lag_1_per + TV_adstock +
                                  Digital + wday.xSaturday + SpecialSaleDay.xBSD + GMV_lag_3_per +
                                  product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                                  SpecialSaleDay.xValentine.Day + Other_adstock + Radio +
                                  SpecialSaleDay.xFHSD + product_analytic_vertical.xSlingBox +
                                  product_analytic_vertical.xHiFiSystem + Sponsorship + deliverycdays +
                                  wday.xWednesday, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_1)
vif(DistriLaghomeAudioMODEL_1)


## Slightly High VIF and Non-significant p-value columns: wday.xWednesday, product_analytic_vertical.xHiFiSystem, sla
## High VIF and Non-significant p-value columns: deliverybdays
## Non-significant p-value columns: SpecialSaleDay.xFHSD, Sponsorship, product_analytic_vertical.xSlingBox
DistriLaghomeAudioMODEL_2 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla + product_analytic_vertical.xDockingStation +
                                  Digital_adstock + price_tag.xMass_Product + product_analytic_vertical.xKaraokePlayer +
                                  GMV_lag_2_per + week + GMV_lag_1_per + TV_adstock + Digital + wday.xSaturday +
                                  SpecialSaleDay.xBSD + GMV_lag_3_per + product_analytic_vertical.xFMRadio +
                                  SpecialSaleDay.xRepublic.Day + SpecialSaleDay.xValentine.Day +
                                  Other_adstock + Radio + deliverycdays, data = DistriLaghomeAudio_Dataset)

summary(DistriLaghomeAudioMODEL_2)
vif(DistriLaghomeAudioMODEL_2)


## Slightly high VIF and Non-significant p-value columns: wday.xSaturday, product_analytic_vertical.xDockingStation
DistriLaghomeAudioMODEL_3 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla +  Digital_adstock + price_tag.xMass_Product +
                                  product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week +
                                  GMV_lag_1_per + TV_adstock + Digital + SpecialSaleDay.xBSD +
                                  GMV_lag_3_per + product_analytic_vertical.xFMRadio +
                                  SpecialSaleDay.xRepublic.Day + SpecialSaleDay.xValentine.Day +
                                  Other_adstock + Radio + deliverycdays, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_3)
vif(DistriLaghomeAudioMODEL_3)


## Less significant p-value columns: GMV_lag_3_per, deliverycdays, Other_adstock
## Slightly high VIF and less significant p-value columns: Radio
DistriLaghomeAudioMODEL_4 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla +  Digital_adstock + price_tag.xMass_Product +
                                  product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week +
                                  GMV_lag_1_per + TV_adstock + Digital + SpecialSaleDay.xBSD +
                                  product_analytic_vertical.xFMRadio + SpecialSaleDay.xRepublic.Day +
                                  SpecialSaleDay.xValentine.Day, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_4)
vif(DistriLaghomeAudioMODEL_4)


## Non-significant p-value columns: SpecialSaleDay.xRepublic.Day
DistriLaghomeAudioMODEL_5 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla +  Digital_adstock + price_tag.xMass_Product +
                                  product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week +
                                  GMV_lag_1_per + TV_adstock + Digital + SpecialSaleDay.xBSD +
                                  product_analytic_vertical.xFMRadio + SpecialSaleDay.xValentine.Day, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_5)
vif(DistriLaghomeAudioMODEL_5)


## High VIF and Non-significant p-value columns: price_tag.xMass_Product
## Non-significant p-value columns: SpecialSaleDay.xValentine.Day
DistriLaghomeAudioMODEL_6 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla +  Digital_adstock +
                                  product_analytic_vertical.xKaraokePlayer + GMV_lag_2_per + week +
                                  GMV_lag_1_per + TV_adstock + Digital + SpecialSaleDay.xBSD +
                                  product_analytic_vertical.xFMRadio, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_6)
vif(DistriLaghomeAudioMODEL_6)


## High VIF and Non-significant p-value columns: product_analytic_vertical.xFMRadio
## Slightly High VIF and less significant p-value columns: Digital
## Less significant p-value columns: SpecialSaleDay.xBSD, product_analytic_vertical.xKaraokePlayer
DistriLaghomeAudioMODEL_7 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla +  Digital_adstock + GMV_lag_2_per + week +
                                  GMV_lag_1_per + TV_adstock, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_7)
vif(DistriLaghomeAudioMODEL_7)


## Less significant p-value columns: TV_adstock, week
DistriLaghomeAudioMODEL_8 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla +  Digital_adstock + GMV_lag_2_per +
                                  GMV_lag_1_per, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_8)
vif(DistriLaghomeAudioMODEL_8)


## Non-significant p-value columns: Digital_adstock
## Less significant p-value columns: GMV_lag_1_per, GMV_lag_2_per
DistriLaghomeAudioMODEL_9 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera +
                                  product_procurement_sla, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_9)
vif(DistriLaghomeAudioMODEL_9)


## Discarding "product_procurement_sla" value and will check Adjusted R-squared value
DistriLaghomeAudioMODEL_10 <- lm(formula = gmv ~ units + product_mrp + SpecialSaleDay.xDaussera, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_10) # Slight change in Adjusted R-squared value at 3rd place of decimal
vif(DistriLaghomeAudioMODEL_10)


## Discarding "product_mrp" value and will check Adjusted R-squared value
DistriLaghomeAudioMODEL_11 <- lm(formula = gmv ~ units +  SpecialSaleDay.xDaussera, data = DistriLaghomeAudio_Dataset)


summary(DistriLaghomeAudioMODEL_11) # Slight change in Adjusted R-squared value at 3rd place of decimal
vif(DistriLaghomeAudioMODEL_11)



### CROSS VALIDATION
cv.lm(data = DistriLaghomeAudio_Dataset, form.lm = DistriLaghomeAudioMODEL_11, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_11 <- function(var){
  DistriLaghomeAudio_ElasticityCoeff <- as.numeric(DistriLaghomeAudioMODEL_11$coefficients[var]*mean(DistriLaghomeAudio_Dataset[,var])/mean(DistriLaghomeAudio_Dataset$gmv))
  return(DistriLaghomeAudio_ElasticityCoeff)
  
}

DistriLaghomeAudio_var_list <- list()

for(i in 2:length(DistriLaghomeAudioMODEL_11$coefficients)){
  DistriLaghomeAudio_var_list[i-1] <- ElasticityValue_11(names(DistriLaghomeAudioMODEL_11$coefficients)[i])
  
}

DistriLaghomeAudio_ElasticityCoeff.outputs <- data.frame(names(DistriLaghomeAudioMODEL_11$coefficients[2:length(DistriLaghomeAudioMODEL_11$coefficients)]))
DistriLaghomeAudio_ElasticityCoeff.outputs <- cbind(DistriLaghomeAudio_ElasticityCoeff.outputs,do.call(rbind.data.frame, DistriLaghomeAudio_var_list))
colnames(DistriLaghomeAudio_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

DistriLaghomeAudio_ElasticityCoeff.outputs$Direction <- ifelse(DistriLaghomeAudio_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
DistriLaghomeAudio_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1                      units   -0.84250  Negative
#2 SpecialSaleDay.xDaussera   -0.00636  Negative
# Plotting Elasticity
ggplot(DistriLaghomeAudio_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.1),hjust = 1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Distributive Lag Model") +xlab("Variables")





#********************************************************[Multiplicative + Distributive Lag Model]
## Discarding lag variables and Moving averages variables but considering 1, 2 and 3 weeks lag value of 'gmv'
## Retaining those is not good idea as 'gmv' won't present in unseen data
MultiLaghomeAudio_Dataset <- HomeAudio_final[,-c(21:22,63:74)]

## Replacing 0 value in column with '0.00001' as log(0) is undefined
MultiLaghomeAudio_Dataset[MultiLaghomeAudio_Dataset == 0] <- 0.00001

## Tranforming negative values
MultiLaghomeAudio_Dataset$GMV_lag_1_per <- 1 + MultiLaghomeAudio_Dataset$GMV_lag_1_per - min(MultiLaghomeAudio_Dataset$GMV_lag_1_per)
MultiLaghomeAudio_Dataset$GMV_lag_2_per <- 1 + MultiLaghomeAudio_Dataset$GMV_lag_2_per - min(MultiLaghomeAudio_Dataset$GMV_lag_2_per)
MultiLaghomeAudio_Dataset$GMV_lag_3_per <- 1 + MultiLaghomeAudio_Dataset$GMV_lag_3_per - min(MultiLaghomeAudio_Dataset$GMV_lag_3_per)

## Taking log of all variable to buils to Multiplicative model
MultiLaghomeAudio_Dataset <- log(MultiLaghomeAudio_Dataset)

## Check variables for linear relationship or multicollinearity
MultiLaghomeAudio_model <- lm(gmv~.,MultiLaghomeAudio_Dataset)
alias(MultiLaghomeAudio_model)

## Discarding variables which were showing linear relationship or multicollinearity
MultiLaghomeAudio_Dataset <- MultiLaghomeAudio_Dataset[, -c(51:63)]


### Stepwise Regression to remove insignificant and correlated variables
MultiLaghomeAudio_Base.Model<- lm(gmv ~ 1 , data= MultiLaghomeAudio_Dataset)  # base intercept only model
MultiLaghomeAudio_All.Model <- lm(gmv ~ . , data= MultiLaghomeAudio_Dataset) # full model with all predictors
MultiLaghomeAudio_StepModel<- step(MultiLaghomeAudio_Base.Model, scope = list(lower = MultiLaghomeAudio_Base.Model, upper = MultiLaghomeAudio_All.Model), direction = "both", trace = 1, steps = 1000)  # perform step-wise algorithm
MultiLaghomeAudio_ShortListedVariables <- names(unlist(MultiLaghomeAudio_StepModel[[1]])) # get shortlisted variable.
MultiLaghomeAudio_ShortListedVariables <- MultiLaghomeAudio_ShortListedVariables[!MultiLaghomeAudio_ShortListedVariables %in% "(Intercept)"]  # remove intercept


### Modeling::

## Building First model after short listing variables [using MultiLaghomeAudio_StepModel]
MultiLaghomeAudioMODEL_1 <- lm(formula = gmv ~ units + product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + TV_adstock + Digital +
                                 product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer +
                                 product_analytic_vertical.xSlingBox + wday.xTuesday + product_analytic_vertical.xFMRadio +
                                 product_analytic_vertical.xDJController + wday.xWednesday +
                                 product_analytic_vertical.xHiFiSystem + Sponsorship + holidayCountInfo +
                                 week + Affiliates + product_analytic_vertical.xVoiceRecorder +
                                 Sponsorship_adstock + Content_Marketing_adstock + SpecialSaleDay.xChristmas...New.Year +
                                 Radio + Online_Marketing_adstock + Other + product_analytic_vertical.xSoundMixer,
                               data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_1)
vif(MultiLaghomeAudioMODEL_1)


## Non-significant p-value columns: product_analytic_vertical.xSoundMixer, product_analytic_vertical.xSlingBox
## High VIF and insignifiacnt p-value columns: units, product_analytic_vertical.xFMRadio, product_analytic_vertical.xHiFiSystem
MultiLaghomeAudioMODEL_2 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + TV_adstock + Digital +
                                 product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer +
                                 wday.xTuesday + product_analytic_vertical.xDJController + wday.xWednesday +
                                 Sponsorship + holidayCountInfo + week + Affiliates + product_analytic_vertical.xVoiceRecorder +
                                 Sponsorship_adstock + Content_Marketing_adstock + SpecialSaleDay.xChristmas...New.Year +
                                 Radio + Online_Marketing_adstock + Other, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_2)
vif(MultiLaghomeAudioMODEL_2)


## High VIF and Non-significant p-value columns: wday.xTuesday
## Less significant p-value columns: week, product_analytic_vertical.xDJController
## High VIF value columns: Online_Marketing_adstock
MultiLaghomeAudioMODEL_3 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + TV_adstock + Digital +
                                 product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer +
                                 wday.xWednesday + Sponsorship + holidayCountInfo + Affiliates +
                                 product_analytic_vertical.xVoiceRecorder + Sponsorship_adstock + Content_Marketing_adstock +
                                 SpecialSaleDay.xChristmas...New.Year + Radio + Other, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_3)
vif(MultiLaghomeAudioMODEL_3)


## High VIF and Non-significant p-value columns: product_analytic_vertical.xVoiceRecorder
MultiLaghomeAudioMODEL_4 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + TV_adstock + Digital +
                                 product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer +
                                 wday.xWednesday + Sponsorship + holidayCountInfo + Affiliates +
                                 Sponsorship_adstock + Content_Marketing_adstock +
                                 SpecialSaleDay.xChristmas...New.Year + Radio + Other, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_4)
vif(MultiLaghomeAudioMODEL_4)


## Non-significant p-value columns: holidayCountInfo
## High VIF value columns: Other
MultiLaghomeAudioMODEL_5 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + TV_adstock + Digital +
                                 product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer +
                                 wday.xWednesday + Sponsorship + Affiliates + Sponsorship_adstock + Content_Marketing_adstock +
                                 SpecialSaleDay.xChristmas...New.Year + Radio , data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_5)
vif(MultiLaghomeAudioMODEL_5)


## Non-significant p-value columns: Radio
MultiLaghomeAudioMODEL_6 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + TV_adstock + Digital +
                                 product_analytic_vertical.xDockingStation + sla + product_analytic_vertical.xKaraokePlayer +
                                 wday.xWednesday + Sponsorship + Affiliates + Sponsorship_adstock + Content_Marketing_adstock +
                                 SpecialSaleDay.xChristmas...New.Year, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_6)
vif(MultiLaghomeAudioMODEL_6)


## Less significant p-value columns: product_analytic_vertical.xKaraokePlayer
## Slightly High VIF and Less significant p-value columns: TV_adstock
MultiLaghomeAudioMODEL_7 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + Digital +
                                 product_analytic_vertical.xDockingStation + sla + wday.xWednesday +
                                 Sponsorship + Affiliates + Sponsorship_adstock + Content_Marketing_adstock +
                                 SpecialSaleDay.xChristmas...New.Year, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_7)
vif(MultiLaghomeAudioMODEL_7)


## Less significant p-value columns: product_analytic_vertical.xDockingStation
## High VIF value columns: Sponsorship_adstock
MultiLaghomeAudioMODEL_8 <- lm(formula = gmv ~ product_mrp + deliverybdays + Digital_adstock +
                                 product_procurement_sla + wday.xSunday + Digital + sla +
                                 wday.xWednesday + Sponsorship + Affiliates + Content_Marketing_adstock +
                                 SpecialSaleDay.xChristmas...New.Year, data = MultiLaghomeAudio_Dataset)

summary(MultiLaghomeAudioMODEL_8)
vif(MultiLaghomeAudioMODEL_8)


## Slightly High VIF and Non-significant p-value columns: Digital_adstock
## Less significant p-value columns: Digital, Sponsorship
MultiLaghomeAudioMODEL_9 <- lm(formula = gmv ~ product_mrp + deliverybdays +
                                 product_procurement_sla + wday.xSunday + sla +
                                 wday.xWednesday + Affiliates + Content_Marketing_adstock +
                                 SpecialSaleDay.xChristmas...New.Year, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_9)
vif(MultiLaghomeAudioMODEL_9)


## High VIF value columns: wday.xWednesday
MultiLaghomeAudioMODEL_10 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla +
                                  wday.xSunday + sla + Affiliates + Content_Marketing_adstock +
                                  SpecialSaleDay.xChristmas...New.Year, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_10)
vif(MultiLaghomeAudioMODEL_10)


## Non-significant p-value columns: Content_Marketing_adstock
MultiLaghomeAudioMODEL_11 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla +
                                  wday.xSunday + sla + Affiliates + SpecialSaleDay.xChristmas...New.Year, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_11)
vif(MultiLaghomeAudioMODEL_11)


## Non-significant p-value columns: SpecialSaleDay.xChristmas...New.Year
MultiLaghomeAudioMODEL_12 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla +
                                  wday.xSunday + sla + Affiliates, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_12)
vif(MultiLaghomeAudioMODEL_12)


## Non-significant p-value columns: wday.xSunday
MultiLaghomeAudioMODEL_13 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla +
                                  sla + Affiliates, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_13)
vif(MultiLaghomeAudioMODEL_13)


## Non-significant p-value columns: Affiliates
MultiLaghomeAudioMODEL_14 <- lm(formula = gmv ~ product_mrp + deliverybdays + product_procurement_sla +
                                  sla, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_14)
vif(MultiLaghomeAudioMODEL_14)


## Discarding 'product_procurement_sla' variable [as it's having less significant p-value]
MultiLaghomeAudioMODEL_15 <- lm(formula = gmv ~ product_mrp + deliverybdays + sla, data = MultiLaghomeAudio_Dataset)


summary(MultiLaghomeAudioMODEL_15)
vif(MultiLaghomeAudioMODEL_15)



### CROSS VALIDATION
cv.lm(data = MultiLaghomeAudio_Dataset, form.lm = MultiLaghomeAudioMODEL_15, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)



### Estimating Elasticity coefficients

ElasticityValue_14 <- function(var){
  MultiLaghomeAudio_ElasticityCoeff <- as.numeric(MultiLaghomeAudioMODEL_15$coefficients[var]*mean(MultiLaghomeAudio_Dataset[,var])/mean(MultiLaghomeAudio_Dataset$gmv))
  return(MultiLaghomeAudio_ElasticityCoeff)
  
}

MultiLaghomeAudio_var_list <- list()

for(i in 2:length(MultiLaghomeAudioMODEL_15$coefficients)){
  MultiLaghomeAudio_var_list[i-1] <- ElasticityValue_14(names(MultiLaghomeAudioMODEL_15$coefficients)[i])
  
}

MultiLaghomeAudio_ElasticityCoeff.outputs <- data.frame(names(MultiLaghomeAudioMODEL_15$coefficients[2:length(MultiLaghomeAudioMODEL_15$coefficients)]))
MultiLaghomeAudio_ElasticityCoeff.outputs <- cbind(MultiLaghomeAudio_ElasticityCoeff.outputs,do.call(rbind.data.frame, MultiLaghomeAudio_var_list))
colnames(MultiLaghomeAudio_ElasticityCoeff.outputs) <- c("Variable","Elasticity")

MultiLaghomeAudio_ElasticityCoeff.outputs$Direction <- ifelse(MultiLaghomeAudio_ElasticityCoeff.outputs$Elasticity > 0, "Positive", "Negative")
MultiLaghomeAudio_ElasticityCoeff.outputs
#Variable Elasticity Direction
#1   product_mrp    0.95627  Positive
#2 deliverybdays   -0.00313  Negative
#3           sla    0.04153  Positive
# Plotting Elasticity
ggplot(MultiLaghomeAudio_ElasticityCoeff.outputs, aes(x=reorder(Variable,Elasticity),y=Elasticity, fill = Direction)) +
  geom_bar(position="dodge",stat="identity") + theme_base() + coord_flip() +
  scale_fill_manual(values=c(Positive="green3",Negative="red")) + geom_text(aes(label=Variable, y=-0.5),hjust = 0.1, color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  ggtitle("HomeAudio - Multiplicative and Distributive Lag Model") +xlab("Variables")

