
install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)

#Checkpoint 1: Data Cleaning 1
#Load the companies and rounds data into two data frames and name them companies and rounds2 respectively.

companies <- read.delim("companies.txt", sep = "\t", dec = ".", stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F)

#Lowering the cases of both the data sets and changing column name in rounds2 
#to merge them with the commom primary key - permalink

companies$permalink <- toupper(companies$permalink)
rounds2$company_permalink <- toupper(rounds2$company_permalink)
names(rounds2)[1] <- "permalink"

#How many unique companies are present in rounds2?
unique_rounds2<-length(unique(rounds2$permalink))
unique_rounds2

#How many unique companies are present in the companies file?
unique_companies<- length(unique(companies$permalink))
unique_companies

#Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
a = sum(rounds2$permalink %in% companies$permalink)
b= nrow(rounds2)
c <- if (a == b) {print("No")} else {print("Yes")}

#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
master_frame <- merge(companies,rounds2,by = "permalink")

#Checkpoint 2: Funding Type Analysis

#Average funding amount of venture type
venture_average <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type=="venture")], na.rm = T)
venture_average

#Average funding amount of angel type
angel_average <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type=="angel")], na.rm = T)
angel_average

#Average funding amount of seed type
seed_average <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type=="seed")], na.rm = T)
seed_average

#Average funding amount of private equity type
private_equity_average <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type =="private_equity")], na.rm = T)
private_equity_average

#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
#which investment type is the most suitable for them?

investment_type <- data.frame(venture_average,seed_average,private_equity_average,angel_average)

investment_type <- c(investment_type[which(investment_type >= 5000000 & investment_type<= 15000000)])
  
investment_type


#Checkpoint 3: Country Analysis
#Top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type i.e. venture)

venture_subset <- subset(master_frame,funding_round_type == 'venture')
venture_subset

country_investment<- aggregate(venture_subset$raised_amount_usd~venture_subset$country_code,venture_subset,sum, na.rm = TRUE)
country_investment

names(country_investment)[1] <- "Country"
names(country_investment)[2] <- "Venture_Total_Amount_Raised"

top <- arrange(country_investment, desc(Venture_Total_Amount_Raised))
top9 <- head(top,9)
top9


#Checkpoint 4: Sector Analysis 1

install.packages("stringr")
library(stringr)

#Extracting the primary sector from "category_list" column
primary_sector <- sapply(strsplit(master_frame$category_list,"\\|"),
                            function(x) x[1])
master_frame_sector <- cbind(master_frame,primary_sector)


mapping<- read.csv("mapping.csv", stringsAsFactors = FALSE)

#Data Cleaning
mapping_0_pattern <- str_count(mapping$category_list, pattern="0")
sum(mapping_0_pattern)

replacing_0 <- str_replace_all(mapping$category_list,pattern = "0",replacement = "na")
replacing_0

rereplacing <- str_replace(replacing_0,pattern = "Enterprise 2.na",replacement = "Enterprise 2.0")
rereplacing

mapping$category_list <- rereplacing

#Converting Wide data to long data, mapping primary sectors to the 8 main sectors.
#and keeping only those columns which are required to merge into master_frame
mapping_sectors<- gather(mapping,main_sectors,my_val,Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping_sectors <- mapping_sectors[!(mapping_sectors$my_val == 0),]
mapping_sectors<- mapping_sectors[,-3]

#Changing column name to merge
names(mapping_sectors)[1] <- "primary_sector"


#Lowering the cases of both the data sets to merge into master_frame_final
master_frame_sector$primary_sector <- tolower(master_frame_sector$primary_sector)
mapping_sectors$primary_sector <- tolower(mapping_sectors$primary_sector)


#Merging to one data frame with each primary sector mapped to its main sector 
#using "primary_sector" as common primary key
master_frame_final<- merge(x= master_frame_sector,y= mapping_sectors, by = "primary_sector", all.x = T)

#We choose to remove NA values from master_frame_final as it would result 
#in counting investments having NA values too 
#(which woul give wrong output on the number of investments - which is the main criteria for choosing top sectors)

master_frame_final <- na.omit(master_frame_final)

#Checkpoint 5: Sector Analysis 2

#Creating a subset data frame containing Funding Type (FT) as "venture" only (Criteria 1 of Investment type)

names(master_frame_final)[13] <- "FT"
names(master_frame_final)[16] <- "investment_amt"
master_frame_venture <- subset(master_frame_final,master_frame_final$FT == "venture")

#Slicing to dataframes - country wise & investment range wise:
#USA, Great Britain and India (Criteria 2 of top 3 English speaking countries in "venture" type of investment)
#Investment range : 5-15 million USD in each round (Criteria 3 using which investment type venture was derived by taking average of all types)

country1 <- subset(master_frame_venture,master_frame_venture$country_code == "USA" & master_frame_venture$investment_amt >=5000000 & master_frame_venture$investment_amt<=15000000)
country2 <- subset(master_frame_venture,master_frame_venture$country_code == "GBR" & master_frame_venture$investment_amt >=5000000 & master_frame_venture$investment_amt<=15000000)
country3 <- subset(master_frame_venture,master_frame_venture$country_code == "IND" & master_frame_venture$investment_amt >=5000000 & master_frame_venture$investment_amt<=15000000)

#To find the heavily invested main sectors in USA, UK and India (based on number of investments) 
#and also the investment funds across the 8 main sectors 
#(for 5-15 million USD range of investment in each round)

USA_Sector_Investment <- country1 %>%
group_by(main_sectors) %>%
summarise(Investment_Count = n(),Investment_Total= sum(investment_amt))

UK_Sector_Investment <- country2 %>%
  group_by(main_sectors) %>%
  summarise(Investment_Count = n(),Investment_Total= sum(investment_amt))

IND_Sector_Investment <- country3 %>%
  group_by(main_sectors) %>%
  summarise(Investment_Count = n(),Investment_Total= sum(investment_amt))

#To find top 3 sectors of USA, UK and IND based on highest number of investments
heavily_invested_sectors_USA <- arrange(USA_Sector_Investment, desc(Investment_Count))
top3_USA <- head(heavily_invested_sectors_USA,3)
top3_USA 

heavily_invested_sectors_UK <- arrange(UK_Sector_Investment, desc(Investment_Count))
top3_UK <- head(heavily_invested_sectors_UK,3)
top3_UK

heavily_invested_sectors_IND <- arrange(IND_Sector_Investment, desc(Investment_Count))
top3_IND <- head(heavily_invested_sectors_IND,3)
top3_IND 

#To find total number of investments + total investment funds across all sectors in USA , UK and IND
#(because the aim is to invest in an english speaking country with highest number of investments)

USA_Investment_count <- sum(heavily_invested_sectors_USA$Investment_Count) 
USA_Investment_count

USA_Investment_amt <- sum(heavily_invested_sectors_USA$Investment_Total)
USA_Investment_amt

UK_Investment_count <- sum(heavily_invested_sectors_UK$Investment_Count) 
UK_Investment_count

UK_Investment_amt <- sum(heavily_invested_sectors_UK$Investment_Total)
UK_Investment_amt

IND_Investment_count <- sum(heavily_invested_sectors_IND$Investment_Count)  
IND_Investment_count

IND_Investment_amt <- sum(heavily_invested_sectors_IND$Investment_Total)
IND_Investment_amt

#CREATING 3 DATA FRAMES D1,D2,D3 including:
#All the columns of the master_frame along with the primary sector and the main sector
#The total number (or count) of investments for each main sector in a separate column as 'Investment_count'
#The total amount invested in each main sector in a separate column as 'Investment_Total'

D1 <- merge(country1,USA_Sector_Investment,by="main_sectors")
D2 <- merge(country2,UK_Sector_Investment,by="main_sectors")
D3 <- merge(country3,IND_Sector_Investment,by="main_sectors")

#First Top Sector Companies of USA, UK and India
USA_Top_Sector_Company <-
subset(D1,D1$main_sectors == top3_USA$main_sectors[1]) %>%
       group_by(permalink) %>%
       summarise(total=sum(investment_amt))%>%
       arrange(desc(total))

UK_Top_Sector_Company <-
  subset(D2,D2$main_sectors == top3_UK$main_sectors[1]) %>%
  group_by(permalink) %>%
  summarise(total=sum(investment_amt))%>%
  arrange(desc(total))

IND_Top_Sector_Company <-
  subset(D3,D3$main_sectors == top3_IND$main_sectors[1]) %>%
  group_by(permalink) %>%
  summarise(total=sum(investment_amt))%>%
  arrange(desc(total))

#Second Top Sector Companies of USA, UK and India
USA_Top_2_Sector_Company <-
  subset(D1,D1$main_sectors == top3_USA$main_sectors[2]) %>%
  group_by(permalink) %>%
  summarise(total=sum(investment_amt))%>%
  arrange(desc(total))


UK_Top_2_Sector_Company <-
  subset(D2,D2$main_sectors == top3_UK$main_sectors[2]) %>%
  group_by(permalink) %>%
  summarise(total=sum(investment_amt))%>%
  arrange(desc(total))


IND_Top_2_Sector_Company <-
  subset(D3,D3$main_sectors == top3_IND$main_sectors[2]) %>%
  group_by(permalink) %>%
  summarise(total=sum(investment_amt))%>%
  arrange(desc(total))

