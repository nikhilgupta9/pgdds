#NYC Police Department has collected data for parking tickets.   
#For the scope of this analysis, parking tickets over the year 2017 is being considered
#We will try and perform some exploratory analysis on a part of this data

#Initializing SparkR session
Sys.setenv(SPARK_HOME = '/usr/local/spark')
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

# loading PACKAGES
library(sparklyr)
library(dplyr)
library(stringr)
library(ggplot2)

# Creating a Spark DataFrame and examine structure
nyc_parking_tickets_17 <- SparkR::read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", "CSV", header="true", inferSchema = "true")

# Checking the Spark Dataframe - nyc_parking_tickets_17
str(nyc_parking_tickets_17) #10 Variables 

#Meta-Data:
#[1] Summons Number - num - Unique Identity Of Summons [Primary Key]   
#[2] Plate ID - chr - Registered Plate Id     
#[3] Registration State - chr - State Of Plate Registration 
#[4] Issue Date - POSIXct - Issue Date     
#[5] Violation Code - int - Type Of Violation  
#[6] Vehicle Body Type - chr - Vehicle Body Type Written On Summons 
#[7] Vehicle Make - chr - Brand Of Car Written On Summons 
#[8] Violation Precint - int - Precinct Of Violation
#[9] Issuer Precint - int - Precint of Issuer
#[10] Violation Time - chr  - Time Violation Occurred printed as per summons

head(nyc_parking_tickets_17)

# Examining the size and properties of the dataframe nyc_parking_tickets_17
nrow(nyc_parking_tickets_17) #108,03,028 rows
ncol(nyc_parking_tickets_17) #10 columns against 10 variables
dim(nyc_parking_tickets_17) 
printSchema(nyc_parking_tickets_17)


##--------------------Data Quality Analysis and Data Cleaning----------------------------------------------------

#1. Eliminating space between names in each column and converting all to lower alphabet form for analysis purpose 
colnames(nyc_parking_tickets_17)<- str_trim(colnames(nyc_parking_tickets_17), side= "both")
colnames(nyc_parking_tickets_17)<- str_replace_all(colnames(nyc_parking_tickets_17), pattern=" ", replacement = "_")
colnames(nyc_parking_tickets_17) <- tolower(colnames(nyc_parking_tickets_17))
colnames(nyc_parking_tickets_17)

#2. Eliminating duplicate rows based on summons_number (unique identifier) if any 
nyc_parking_tickets_17 <- SparkR:: dropDuplicates(nyc_parking_tickets_17, "summons_number")
nrow(nyc_parking_tickets_17)
#no duplicates found

#3.1 Checking for missing values/Null
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Missing_values_2017 <- SparkR::sql("SELECT COUNT(*) No_of_Records
                                   FROM nyc_pt_17
                                   where (summons_Number <> '' 
                                   or plate_id <> '' 
                                   or registration_state <> '' 
                                   or issue_date <> ''
                                   or violation_code <> ''
                                   or vehicle_body_type <> ''
                                   or vehicle_make <> ''
                                   or violation_precinct <> ''
                                   or issuer_precinct <> ''
                                   or violation_time <> '' )")

head(Missing_values_2017) #No missing values

#3.2.1 Checking for missing NA/ N/A values
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

NA_values_2017 <- SparkR::sql("SELECT COUNT(*) No_of_Records
                              FROM nyc_pt_17
                              where (summons_number = 'NA' 
                              or plate_id = 'NA' 
                              or registration_state = 'NA' 
                              or issue_date= 'NA' 
                              or violation_code = 'NA' 
                              or vehicle_body_type = 'NA' 
                              or vehicle_make = 'NA' 
                              or violation_precinct = 'NA' 
                              or issuer_precinct = 'NA' 
                              or violation_time = 'NA')")

head(NA_values_2017) #No NA Values 

#3.2.2

createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

NA_values_2017 <- SparkR::sql("SELECT COUNT(*) No_of_Records
                              FROM nyc_pt_17
                              where (summons_number = 'N/A' 
                              or plate_id = 'N/A' 
                              or registration_state = 'N/A' 
                              or issue_date= 'N/A' 
                              or violation_code = 'N/A' 
                              or vehicle_body_type = 'N/A' 
                              or vehicle_make = 'N/A' 
                              or violation_precinct = 'N/A' 
                              or issuer_precinct = 'N/A' 
                              or violation_time = 'N/A')")

head(NA_values_2017) #No N/A Values 

#3.2.3 - Checking if NA values exist in between for num: summons_number, date: issue_date and chr: violation_time  
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

NA_values_2017 <- SparkR::sql("SELECT summons_number, issue_date, violation_time, COUNT(*) No_of_Records
                              FROM nyc_pt_17
                              where (summons_number like ('%na%') 
                              or issue_date like ('%na%') 
                              or violation_time like ('%na%'))
                              group by summons_number, issue_date, violation_time ")

head(NA_values_2017)  
#Nan values found in violation_time

#3.2.4 Checking number of records of nan values in violation_time
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

NA_violation_time_values_2017 <- SparkR::sql("SELECT COUNT(*) No_of_Records
                                             FROM nyc_pt_17
                                             where violation_time like ('%na%')
                                             ")

head(NA_violation_time_values_2017) #63 na values found in violation_time
#We cannot omit the na values found in violation_time at this stage as it remains in between the values. In our further steps when reaching violation_time, 
#we will seggregate the values violation_time into hours, minutes and AM_PM to find where NA exists in these columns

#4. Checking if descrepencies exist in the range of parking ticket Issue Dates for the fiscal year 2017 

##Converting issue_date to suitable format 
nyc_parking_tickets_17$issue_date <- SparkR::to_date(nyc_parking_tickets_17$issue_date, "MM/dd/yyyy")

nrow(nyc_parking_tickets_17)
str(nyc_parking_tickets_17)

##Checking the First Issue Date & Last Issue Date for fiscal year ending 2017 (ranging from 01-07-2016 to 30-06-2017)
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Issue_Date_Range_2017 <- SparkR::sql("SELECT min(issue_date)as First_Issue_Date,
                                     max(issue_date)as Last_Issue_Date
                                     FROM nyc_pt_17")

head(Issue_Date_Range_2017)

# First_Issue_Date    Last_Issue_Date                                              
#      1972-03-30         2069-11-19

##Checking the Number of irrelevant records for fiscal year ending 2017 (ranging from 01-07-2016 to 30-06-2017)
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Irrelevant_Issue_Dates_2017 <- SparkR::sql("SELECT COUNT(*) as Num_of_Records
                                           FROM nyc_pt_17 
                                           WHERE (issue_date > '2017-06-30'
                                           or issue_date < '2016-07-01')")
head(Irrelevant_Issue_Dates_2017)
#2,63,465 irrelevant records present in dataframe - nyc_parking_tickets_17 = 2.5% of total records


##Subsetting the DataFrame - nyc_parking_tickets_17 according to the Fiscal Year ending June'17
nyc_parking_tickets_17 <- nyc_parking_tickets_17[
  nyc_parking_tickets_17$issue_date >= "2016-07-01" & 
    nyc_parking_tickets_17$issue_date <= "2017-06-30"]

nrow(nyc_parking_tickets_17) 
#Successfully removed irrelevant rows 
#105,39,563 relevant rows in dataframe - nyc_parking_tickets_17

head(nyc_parking_tickets_17)

#Deriving seaparte Ticket Issue Year and Ticket Issue Month columns for analysis purpose 
nyc_parking_tickets_17$issue_Yr <- year(nyc_parking_tickets_17$issue_date)
nyc_parking_tickets_17$issue_Mnth <- month(nyc_parking_tickets_17$issue_date)

#Checking the year and month range of for fiscal year ending 2017 (ranging from 01-07-2016 to 30-06-2017)
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Issue_Yr_Mnth_2016 <- SparkR::sql("SELECT issue_Yr,
                                  issue_Mnth,
                                  count(*)as Num_of_Records
                                  FROM nyc_pt_17
                                  where issue_yr = '2016'
                                  GROUP BY issue_Yr,
                                  issue_Mnth
                                  ORDER BY 1,2")
head(Issue_Yr_Mnth_2016) 
#has all 6 mnths of year 2016 for fiscal year as on  June'17


createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Issue_Yr_Mnth_2017 <- SparkR::sql("SELECT issue_Yr,
                                  issue_Mnth,
                                  count(*)as Num_of_Records
                                  FROM nyc_pt_17
                                  where issue_yr = '2017'
                                  GROUP BY issue_Yr,
                                  issue_Mnth
                                  ORDER BY 1,2")
head(Issue_Yr_Mnth_2017) 
#has all 6 mnths of year 2017 for fiscal year as on  June'17


##5. Checking for irregularities in the range of registration_state 
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Registration_States_2017 <- SparkR::sql("SELECT registration_state, COUNT(*) as Num_of_Records
                                        FROM nyc_pt_17 
                                        group by registration_state
                                        order by Num_of_Records desc")
head(Registration_States_2017,nrow(Registration_States_2017)) 
#New York state registration has the highest count in the records given for the fiscal year ending June,2017
#Registration_State = '99' could be invalid


##6. Checking for irregularities in the range of vehicle_body_type  
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Vehicle_Body_Type_2017 <- SparkR::sql("SELECT vehicle_body_type, COUNT(*) as Num_of_Records
                                      FROM nyc_pt_17 
                                      group by vehicle_body_type
                                      order by Num_of_Records desc")
head(Vehicle_Body_Type_2017,nrow(Vehicle_Body_Type_2017)) 
#SUBN has the highest number of records for fiscal year ending June'17  


#7. Checking for irregularities in the range of vehicle_make
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Vehicle_Make_Type_2017 <- SparkR::sql("SELECT vehicle_make, COUNT(*) as Num_of_Records
                                      FROM nyc_pt_17 
                                      group by vehicle_make
                                      order by Num_of_Records desc")
head(Vehicle_Make_Type_2017,nrow(Vehicle_Make_Type_2017)) 
#FORD has highest count under vehicle make for fiscal year ending June,2017.

#8. Checking for irregularities in violation_time'

# 8.1 Extracting hours, minutes and AM/PM fromn Violation Time for analysis purpose
nyc_parking_tickets_17$violation_hour <- substr(nyc_parking_tickets_17$violation_time, 1, 2)
nyc_parking_tickets_17$violation_minutes <- substr(nyc_parking_tickets_17$violation_time, 3, 4)
nyc_parking_tickets_17$violation_AM_PM <- substr(nyc_parking_tickets_17$violation_time, 5,6)

head(nyc_parking_tickets_17) #checking

# 8.2 Checking for discrepancies in the range of violation_hour 

#8.2.1
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_Hour_2017 <- SparkR::sql("SELECT violation_hour, COUNT(*) as Num_of_Records
                                   FROM nyc_pt_17 
                                   group by violation_hour
                                   order by 1")
head(Violation_Hour_2017) 
#    violation_hour   Num_of_Records                                                 
#             .2              1
#             .3              1
#             .9              1
#             0.              3
#             00          57769
#             01        1147763

#discrepancy found with 6 records of violation_hour - .2,.3,.9,0.

#8.2.2
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_Hour_2017 <- SparkR::sql("SELECT violation_hour, COUNT(*) as Num_of_Records
                                   FROM nyc_pt_17 
                                   group by violation_hour
                                   order by 1 desc")
head(Violation_Hour_2017) 

#       violation_hour   Num_of_Records                                                 
# 1             na             53
# 2             87              6
# 3             86              1
# 4             85              1
# 5             84              2
# 6             82              1

#53 out of 63 NA records found in violation hour. Discrepancies found as hours cannot be more than 24 > It must fall in the range of 1-24
#Remaining 10 NA values assumed to be ommitted while removing irrelevant issue date records

#8.2.3
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_Hour_2017 <- SparkR::sql("SELECT violation_hour, COUNT(*) as Num_of_Records
                                   FROM nyc_pt_17 
                                   group by violation_hour
                                   order by Num_of_Records desc")
head(Violation_Hour_2017) 
#Most records fall in hours between 1-12.

#8.4.4 Checking the number of records that fall beyond the range of 1-24 for violation_hours
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_Hour_2017 <- SparkR::sql("SELECT COUNT(*) as Num_of_Records
                                   FROM nyc_pt_17 
                                   where violation_hour > '24' 
                                   ")
head(Violation_Hour_2017) 
#139 records will not be ommitted as deleting these records may affect other columns with valid data
#6 records show inconsistency in violation_hour where violation hour = 0.2,0.3,0.9,0.
#Total inconsistent records due to violation_hour = 145
#However, 145 of these records will be  excluded only  when required (during further analysis)

#We assume violation_hour to be in the range between 1-24 
#8.3 Checking for discrepancies in the range of violation_hour, violation_minutes and violation_AM_PM filtering by AM = 'A'
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_Timestamp_AM_2017 <- SparkR::sql("SELECT violation_hour,violation_minutes,violation_AM_PM, COUNT(*) as Num_of_Records
                                           FROM nyc_pt_17 
                                           where violation_AM_PM = 'A'
                                           group by violation_hour,violation_minutes, violation_AM_PM
                                           order by Num_of_Records desc")
head(Violation_Timestamp_AM_2017) 

#Checking for discrepancies in the range of violation_hour, violation_minutes and violation_AM_PM for '12' hours only
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_12H_2017 <- SparkR::sql("SELECT violation_hour,violation_minutes,violation_AM_PM, COUNT(*) as Num_of_Records
                                  FROM nyc_pt_17 
                                  where violation_hour = '12'
                                  group by violation_hour,violation_minutes, violation_AM_PM
                                  order by 1,2")
head(Violation_12H_2017) 
#there are discrepancies as in some places it is 12 - PM and in some it is 12 - AM

#Checking for discrepancies in the range of violation_hour, violation_minutes and violation_AM_PM for '00' hours only
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_00H_2017 <- SparkR::sql("SELECT violation_hour,violation_minutes,violation_AM_PM, COUNT(*) as Num_of_Records
                                  FROM nyc_pt_17 
                                  where violation_hour = '00'
                                  group by violation_hour,violation_minutes, violation_AM_PM
                                  order by 1,2")
head(Violation_00H_2017) 
#there are discrepancies as in some places it is 00 - PM and in some it is 00 - AM


#8.4 Checking for discrepancies in the range of violation_minutes
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_minutes_2017 <- SparkR::sql("SELECT violation_minutes, COUNT(*) as Num_of_Records
                                      FROM nyc_pt_17 
                                      group by violation_minutes
                                      order by 1")
head(Violation_minutes_2017,nrow(Violation_minutes_2017)) 
#Irregularities observed with letters and symbols 
#Not eliminating or replacing at this stage these values for further analysis purpose


#8.5 Checking for discrepancies in the range of violation_AM_PM
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_AMPM_2017 <- SparkR::sql("SELECT violation_AM_PM, COUNT(*) as Num_of_Records
                                   FROM nyc_pt_17 
                                   group by violation_AM_PM
                                   order by 1")
head(Violation_AMPM_2017,nrow(Violation_AMPM_2017)) 
#Irregularities observed with letters other than 'A' and 'P' and missing values
#Not eliminating or replacing these values at this stage for further analysis purpose

#8.6 Since there are discrepancies observed only for 00 & 12, '00'Hours is going to be replaced by '12'Hours and correctly coallated back into violation_time to get if it is AM/PM correctly
nyc_parking_tickets_17$violation_hour <- regexp_replace(x = nyc_parking_tickets_17$violation_hour,pattern = "00",replacement = "12")

#Converting 'A'into 'AM' & 'P'into 'PM in violation_AM_PM column before concatenating into violation_time
nyc_parking_tickets_17$violation_AM_PM <- regexp_replace(x = nyc_parking_tickets_17$violation_AM_PM,pattern = "A",replacement = "AM")

nyc_parking_tickets_17$violation_AM_PM <- regexp_replace(x = nyc_parking_tickets_17$violation_AM_PM,pattern = "P",replacement = "PM")

head(nyc_parking_tickets_17)

#Concatenating in the format given of Hours,Minutes and AM/PM back into violation_time 
nyc_parking_tickets_17$violation_time = concat_ws(sep=":", nyc_parking_tickets_17$violation_hour, nyc_parking_tickets_17$violation_minutes, nyc_parking_tickets_17$violation_AM_PM)

#Now Concatenating Issue date with Violation Time for analysis as no missing values were found under Issue_Date (but were found for Violation_AM_PM and violation_minutes) 
nyc_parking_tickets_17$violation_time_date = concat_ws(sep=" ", nyc_parking_tickets_17$issue_date,nyc_parking_tickets_17$violation_time)

#Converting violation_time_date to timestamp
nyc_parking_tickets_17$violation_time_date <- to_timestamp(x = nyc_parking_tickets_17$violation_time_date, format = "yyyy-MM-dd h:mm:a")
head(nyc_parking_tickets_17)

#-------------------------
str(nyc_parking_tickets_17)
dim(nyc_parking_tickets_17) # Rows: 10539563       Columns:16 

#-------------------------------Examine the data------------------------------------------------------

#ques-1 Find the total number of tickets for the year.
Number_of_Tickets_2017<- nrow(nyc_parking_tickets_17)
Number_of_Tickets_2017 #10539563 unique tickets (145 records not completely valid due to violation_time)


#ques-2 Find out the number of unique states from where the cars that got parking tickets came from.

#Checking number of records for incorrect registration_state = 99 
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

registration_state_99_2017<- SparkR::sql("SELECT registration_state, count(*)as Num_of_Tickets 
                                         from  nyc_pt_17
                                         where registration_state = '99'
                                         group by registration_state
                                         ")
head(registration_state_99_2017) #34,720 entries found under incorrect registered state = 99

#Adjusting these 34,720 entries under the highest registered state - NY (as per analysis in 5. - Data Quality Analysis)
nyc_parking_tickets_17$registration_state <- regexp_replace(x = nyc_parking_tickets_17$registration_state,pattern = "99",replacement = "NY")

#Checking Distribution of records under unique Registered States for fiscsal year ending June'17 
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

registration_state_2017<- SparkR::sql("SELECT distinct(registration_state), count(*)as Num_of_Tickets 
                                      from nyc_pt_17 
                                      group by registration_state
                                      order by Num_of_Tickets desc")
head((registration_state_2017),nrow(registration_state_2017))


#Number of Unique Registered States for fiscal year ending(2017) 
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

registration_state_2017<- SparkR::sql("SELECT count(distinct(registration_state)) as Number_Of_Unique_Registereted_States
                                      from nyc_pt_17 
                                      ")
head(registration_state_2017)
#66 unique states are registered

#---------------------------------Aggregation tasks -------------------------------------------------------------------------------

#Ques-1:How often does each violation code occur? Display the frequency of the top five violation codes.
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_Frequency_2017<- SparkR::sql("SELECT distinct(violation_code) as Violation_Code, count(*)as Frequency_of_Occurence 
                                       from  nyc_pt_17
                                       group by violation_code
                                       order by Frequency_of_Occurence desc")
head(Violation_Frequency_2017,5)
#      Violation_Code     Frequency_of_Occurence                                         
# 1             21                1500396
# 2             36                1345237
# 3             38                1050418
# 4             14                 880152
# 5             20                 609231


#Ques-2 How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? 

#Vehicle Body Type
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Vehicle_Body_Type_2017<- SparkR::sql("SELECT vehicle_body_type, count(*)as Frequency_of_Tickets
                                     from  nyc_pt_17
                                     group by vehicle_body_type
                                     order by Frequency_of_Tickets desc")
head(Vehicle_Body_Type_2017,5)
#       vehicle_body_type    Frequency_of_Tickets                                        
# 1              SUBN              3632003
# 2              4DSD              3017372
# 3               VAN              1384121
# 4              DELV               672123
# 5               SDN               414984

#Vehicle Make Type
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Vehicle_Make_2017<- SparkR::sql("SELECT vehicle_make, count(*)as Frequency_of_Tickets
                                from  nyc_pt_17
                                group by vehicle_make
                                order by Frequency_of_Tickets desc")
head(Vehicle_Make_2017, 5)

#     vehicle_make    Frequency_of_Tickets                                             
# 1         FORD              1250777
# 2        TOYOT              1179265
# 3        HONDA              1052006
# 4        NISSA               895225
# 5        CHEVR               698024


#Ques-3 Find the (5 highest) frequency of tickets for each of the following:

#3.1 Violation Precinct - (this is the precinct of the zone where the violation occurred)
#3.1.1   5 highest frequency of tickets for 'Violation Precinct' 
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Violation_Precinct_2017<- SparkR::sql("SELECT violation_precinct, count(*)as Frequency_of_Tickets
                                      from  nyc_pt_17
                                      group by violation_precinct
                                      order by Frequency_of_Tickets desc")
head(Violation_Precinct_2017)
#        violation_precinct    Frequency_of_Tickets                                       
# 1                  0              1950083
# 2                 19               528317
# 3                 14               347736
# 4                  1               326961
# 5                 18               302008
# 6                114               292682

#3.1.2 Using this, can you make any insights for parking violations in any specific areas of the city?

City_Zone_Violated_2017_top5<- data.frame(head(Violation_Precinct_2017))

ggplot(City_Zone_Violated_2017_top5, aes(x=as.factor(violation_precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Violated City Zone") + ylab("Frequency of Tickets") + ggtitle(" Top 5 Violation Precinct vs Frequency of Ticket - 2017") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#3.2.  'Issuer Precinct' - (this is the precinct that issued the ticket)
#3.2.1   5 highest frequency of tickets for 'Issuer_Precinct'
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Issuer_Precinct_2017<- SparkR::sql("SELECT issuer_precinct, count(*)as Frequency_of_Tickets
                                   from  nyc_pt_17
                                   group by issuer_precinct
                                   order by Frequency_of_Tickets desc")
head(Issuer_Precinct_2017)
#       issuer_precinct    Frequency_of_Tickets                                          
# 1               0              2255086
# 2              19               514786
# 3              14               340862
# 4               1               316776
# 5              18               292237
# 6             114               286316

#3.1.2 Using this, can you make any insights for parking violations in any specific areas of the city?
City_Zone_Issued_Ticket_2017_top5<- data.frame(head(Issuer_Precinct_2017))

ggplot(City_Zone_Issued_Ticket_2017_top5, aes(x=as.factor(issuer_precinct), y=Frequency_of_Tickets))+ geom_col() + xlab("Issued Ticket - City Zone") + ylab("Frequency of Tickets") + ggtitle(" Top 5 Issuer Precinct vs Frequency of Ticket - 2017") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#Ques:4 Find the violation code frequency across three precincts which have issued the most number of tickets 
#- do these precinct zones have an exceptionally high frequency of certain violation codes? 
#Are these codes common across precincts? 

#4.1
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Precinct_Code_2017 <- SparkR::sql("SELECT issuer_precinct as Issuer_Precinct, violation_code, COUNT(*) as Violation_Code_Frequency
                                  FROM nyc_pt_17
                                  WHERE issuer_precinct in ('0','19','14','1')
                                  group by issuer_precinct, violation_code
                                  order by Violation_Code_Frequency desc")
head(Precinct_Code_2017)

#    Issuer_Precinct violation_code Violation_Code_Frequency                                            
#1               0      36             1345237
#2               0       7             464690
#3               0      21             258771
#4               0       5             130963
#5              19      46             84789
#6              14      14             73007

#Without Issuer_Precinct = '0'
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Precinct_Code_2017 <- SparkR::sql("SELECT issuer_precinct as Issuer_Precinct, violation_code, COUNT(*) as Violation_Code_Frequency
                                  FROM nyc_pt_17
                                  WHERE issuer_precinct in ('19','14','1')
                                  group by issuer_precinct, violation_code
                                  order by Violation_Code_Frequency desc")
head(Precinct_Code_2017)
#Issuer_Precinct    violation_code      Violation_Code_Frequency                       
# 1         19             46                    84789
# 2         14             14                    73007
# 3          1             14                    72520
# 4         19             38                    71631
# 5         19             37                    71592
# 6         14             69                    57316

#4.2 Violation Code = 14 has a high frequency in both Issuer Precincts 1 and 14 

#qtn 5  You'd want to find out the properties of parking violations across different times of the day:
#5.1 Find a way to deal with missing values, if any.

# For finding missing values after converting into timestamp seems to be relevant for purpose of this analysis
# We converted the column violation_time_date into timestamp after concatenating it with the issue_date as issue_date irregularities were fixed and have no missing values
# However, if missing values are found after running the query below, it is due to irregular records present in violation_hour, violation_minutes and violation_AM_PM after being converted to timestamp

createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")


violation_time_null_2017<- SparkR::sql("SELECT count(*)as Total_Num_of_Records, 
                                       SUM(CASE WHEN violation_time_date is NULL
                                       THEN 1 ELSE 0 END)as Violation_Time_Null
                                       from nyc_pt_17")
head(violation_time_null_2017)
#232 Missing Values found
# Not eliminating these values for analysis purpose as eliminating may discard valid data in other fields
# We will replace the missing values with NA for this purpose 

nyc_parking_tickets_17$violation_time_date <- regexp_replace(x = nyc_parking_tickets_17$violation_time_date,pattern = "NULL",replacement = "NA")
dim(nyc_parking_tickets_17)


#5.2 Violation Time field is specified in a strange format. 
# Find a way to make this into a time attribute that you can use to divide into groups. 
# (Done in Data Cleaning)

#Divide 24 hours into six equal discrete bins of time. The intervals you choose are at your discretion.
#For each of these groups, find the three most commonly occurring violations.

createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_violation_time_bins_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

violation_hour_bins_2017 <- SparkR::sql("SELECT violation_hour,
                                        violation_code,
                                        CASE WHEN violation_hour BETWEEN 0 AND 3
                                        THEN '0_to_3_AM_Hrs'
                                        WHEN violation_hour BETWEEN 4 AND 7
                                        THEN '4_to_7_AM_Hrs'
                                        WHEN violation_hour BETWEEN 8 AND 11
                                        THEN '8_to_11_AM_Hrs'
                                        WHEN violation_hour BETWEEN 12 AND 15
                                        THEN '12_to_15_PM_Hrs' 
                                        WHEN violation_hour BETWEEN 16 AND 19
                                        THEN '16_to_19_PM_Hrs' 
                                        WHEN violation_hour BETWEEN 20 AND 24
                                        THEN '20_to_00_PM_Hrs' 
                                        END AS Violation_Hour_Bins
                                        FROM nyc_pt_violation_time_bins_17
                                        WHERE violation_hour <> 'na'
                                        and violation_hour <=24")

head(violation_hour_bins_2017) #Validating bins 


#5.3 For the three most commonly occurring violation_code, find the most common time of the day (in terms of the bins from the previous part)

createOrReplaceTempView(violation_hour_bins_2017, "violation_hour_vcode_2017")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

hour_bin_vcode_2017 <- SparkR::sql("SELECT Violation_Hour_Bins,
                                   violation_code,
                                   Frequency_of_Tickets
                                   FROM (SELECT Violation_Hour_Bins,
                                   violation_code,
                                   Frequency_of_Tickets,
                                   dense_rank() over (partition by Violation_Hour_Bins order by Frequency_of_Tickets desc) Rnk
                                   FROM (SELECT Violation_Hour_Bins,
                                   violation_code,
                                   count(*)as Frequency_of_Tickets
                                   FROM violation_hour_vcode_2017
                                   GROUP BY Violation_Hour_Bins,
                                   violation_code))
                                   WHERE Rnk <= 3")
head(hour_bin_vcode_2017, nrow(hour_bin_vcode_2017))

df_hour_bin_tkts_2017 <- data.frame(head(hour_bin_vcode_2017, nrow(hour_bin_vcode_2017)))

ggplot(df_hour_bin_tkts_2017, aes(x= as.factor(violation_code), y=Frequency_of_Tickets))+ geom_col()+ facet_grid(~Violation_Hour_Bins) + xlab("Violation Code") + ylab("Frequency of Tickets") + ggtitle("2017-  Violation Code Distribution vs. Violation_Hour_Bin") + geom_text(aes(label=Frequency_of_Tickets),vjust=-0.3)


#Ques-6 Let's try and find some seasonality in this data
#Dividing the whole year into rainy,winter and summer seasons - 4 months each season
#6.1 first divide into seasons
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

Dividing_into_seasons<-SparkR::sql("SELECT violation_code,issue_mnth,
                                   CASE WHEN issue_Mnth  IN ('12','1','2')
                                   THEN 'WINTER'
                                   WHEN issue_Mnth  BETWEEN 3 AND 5
                                   THEN 'SPRING'
                                   WHEN issue_Mnth  BETWEEN 6 AND 8
                                   THEN 'SUMMER'
                                   WHEN issue_Mnth  BETWEEN 9 AND 11
                                   THEN 'FALL'
                                   END AS season
                                   FROM nyc_pt_17 ")

head(Dividing_into_seasons) #checking seasons 

#6.2  and then calculate the frequencies of tickets for each season
createOrReplaceTempView(Dividing_into_seasons, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

frequency_tickets_season<-SparkR::sql("SELECT season,
                                      Count(*)as frequency_of_tickets
                                      FROM  nyc_pt_17
                                      GROUP BY season
                                      ORDER BY frequency_of_tickets desc")

head(frequency_tickets_season)
#SPRING has highest frequency of tickets
# Season       frequency of tickets
# SPRING             2873383
# FALL               2829224
#  WINTER            2483036
#  SUMMER            2353920

#6.3  find the three most common violations for each of these seasons
createOrReplaceTempView(Dividing_into_seasons, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

spring_violation<-SparkR::sql("SELECT violation_code , count(*) as No_Of_Occurances from nyc_pt_17 WHERE season='SPRING' group by violation_code order by No_Of_Occurances desc")
fall_violation<-SparkR::sql("SELECT violation_code , count(*) as No_Of_Occurances from nyc_pt_17 WHERE season='FALL' group by violation_code order by No_Of_Occurances desc")
summer_violation<-SparkR::sql("SELECT violation_code , count(*) as No_Of_Occurances from nyc_pt_17 WHERE season='SUMMER' group by violation_code order by No_Of_Occurances desc")
winter_violation<-SparkR::sql("SELECT violation_code , count(*) as No_Of_Occurances from nyc_pt_17 WHERE season='WINTER' group by violation_code order by No_Of_Occurances desc")

head(spring_violation)
#      violation_code     No_Of_Occurances                                               
# 1             21           402424
# 2             36           344834
# 3             38           271167
# 4             14           256397
# 5             46           173440
# 6             20           157122

head(fall_violation)
#      violation_code     No_Of_Occurances  
# 1             36           456046
# 2             21           357257
# 3             38           283816
# 4             14           216721
# 5             37           169286
# 6             20           160593

head(summer_violation)
#      violation_code    No_Of_Occurances                                               
# 1             21           378699
# 2             38           235725
# 3             14           207495
# 4             36           185019
# 5             20           154465
# 6             37           132283

head(winter_violation)
#     violation_code    No_Of_Occurances                                               
# 1             21           362016
# 2             36           359338
# 3             38           259710
# 4             14           199539
# 5             20           137051
# 6             37           135904

#Ques-7: The fines collected from all the parking violation constitute a revenue source for the NYC police department. 
#Let's take an example of estimating that for the three most commonly occurring codes.

#7.1
createOrReplaceTempView(nyc_parking_tickets_17, "nyc_pt_17")
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

violation_code_2017 <- SparkR::sql("SELECT violation_code , count(*) as No_Of_Occurances 
                                   from nyc_pt_17 
                                   group by violation_code 
                                   order by No_Of_Occurances desc")

head(violation_code_2017,3)

# the top 3 Violation codes are

#  VIOLATION_CODE    NO_OF_OCCURANCES  
#1  21                1500396
#2  36                1345237
#3  38                1050418

#7.2 CALICULATING THE TOTAL AMOUNT FOR THE THREE VIOLATION CODES.
top3_violationcode_2017<- data.frame(head(violation_code_2017,3))
top3_violationcode_2017$fine_rate<- c(55,50,50) #Avg of Fines of top 3 violated code locations covering city and other areas of these codes
top3_violationcode_2017$total_amount_fined<-top3_violationcode_2017$No_Of_Occurances * top3_violationcode_2017$fine_rate
top3_violationcode_2017
#code 21 has the highest collection.

#so here we can infer that the most fine collection takes place due to street cleaning where no parking is indicated through by sign, street marking or traffic control device

sparkR.stop()