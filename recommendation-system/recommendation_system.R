#Understanding the business
#You are the sales manager for "BeerMart", an on-line beer store in the United States.
#You want to build a recommendation system (collaborative) for your store, where customers will be recommended the beer that they are most likely to buy. 
#You collected the data about the ratings that the customers have provided in the past.

#Assumption: Assuming the data file is present in the R working directory
#Importing the data
#Converting all the blanks to NA while importing data
beer_data <- read.csv("beer_data.csv", header=T, na.strings=c("","NA"))

#Loading all the required packages
library(dplyr)
library(recommenderlab)
library(ggplot2)

#Understanding the data
head(beer_data)
#Each record is composed of a beer's name, the name of the user along with ratings provided by users.
#All ratings are on a scale from 1 to 5 with 5 being the best.

summary(beer_data)
#beer_beerid         review_profilename review_overall 
#Min.   :    3   northyorksammy:  1846   Min.   :0.000  
#1st Qu.: 1716   mikesgroove   :  1379   1st Qu.:3.500  
#Median :13892   BuckeyeNation :  1338   Median :4.000  
#Mean   :21661   Thorpe429     :  1072   Mean   :3.815  
#3rd Qu.:39397   ChainGangGuy  :  1046   3rd Qu.:4.500  
#Max.   :77317   (Other)       :469203   Max.   :5.000  
#NA's          :   100

#The summary shows 100 'NA' values.
#Checking for number of NA values in each column
sapply(beer_data, function(x) sum(is.na(x)))

#beer_beerid review_profilename     review_overall 
#    0                100                  0
# The column review_profilename has all the NA values which needs to be removed

#Check for duplicate profile-name
length(unique(beer_data$review_profilename))
#22498

#Converting profile-name to lower case and checking for unique values
profile_lower <- tolower(beer_data$review_profilename)
length(unique(profile_lower))
#22498

#The unique values match after lower case conversion.

#Cleaning NA values
beer_data_NA_removed <- na.omit(beer_data)
sum(is.na(beer_data_NA_removed))
#All NA values successfully removed

#Check for duplicate rows. Checking if a user has posted multiple reviews for same beer
sum(duplicated(beer_data_NA_removed))
#Looks like there are duplicate values in the data frame

#Creating a new data frame from the cleaned data frame with distinct values of beer id and profile name
beer_data_distinct <- distinct(beer_data_NA_removed,beer_beerid,review_profilename,.keep_all = TRUE)
nrow(beer_data_distinct)
#474462 distinct reviews

#We have now ensured that no two rows has the same beer_id and profile name
#Now, we can move to the analysis part

#----------------------------------------------------------------------------------------------------------#
#Data preparation
#1.
#Choose only those beers that have at least N number of reviews
#Figure out an appropriate value of N using EDA; this may not have one correct answer, 
#but you shouldn't choose beers having extremely low number of ratings

#Distinct beers in data frame
group_by_beer_id <- beer_data_distinct %>% group_by(beer_beerid) %>% summarise(overall_beer_reviews=n())
nrow(group_by_beer_id)

#Sorting all beers by decreasing order of review counts
group_by_beer_id[with(group_by_beer_id, order(overall_beer_reviews,decreasing = TRUE)), ]
summary(group_by_beer_id)
#The number of counts range from 977(max) to 1(minimum)
#Maximum number of reviews: Beer(2093)

#Grouping all entries by review_profilename
group_by_profile_name <- beer_data_distinct %>% group_by(review_profilename) %>% summarise(overall_profile_reviews=n())
nrow(group_by_profile_name)
#There are 22497 distinct reviewers

#Sorting all profiles by their review counts
group_by_profile_name[with(group_by_profile_name, order(overall_profile_reviews,decreasing = TRUE)), ]
summary(group_by_profile_name)
#The profile-name "northyorksammy" has maximum reviews: 1842

#The beer reviews range from 977 to 1. Now, we need to find an optimum thrush-hold value to consider
# a beer for evaluation

overall_reviews_plot <- qplot(group_by_beer_id$overall_beer_reviews, geom = "histogram",xlab="Overall reviews", ylab = "Number of beers")
overall_reviews_plot
#A large number of beers have very few reviews. These should be removed from the dataset to analyze

#Creating a data frame with columns Total number of reviews received by a beer and number of beers
#of that kind
frequency_beer_aggregated<-group_by_beer_id %>% group_by(overall_beer_reviews) %>% summarise(frequency=n())
head(frequency_beer_aggregated, 20)
#A tibble: 20 x 2
#overall_beer_reviews  frequency
#<int>     <int>
#1      1     18080
#2      2      6183
#3      3      3079
#4      4      1895
#5      5      1350
#6      6       959
#7      7       785
#8      8       556
#9      9       478
#10     10       401
#11     11       388
#12     12       346
#13     13       295
#14     14       256
#15     15       242
#16     16       202
#17     17       183
#18     18       162
#19     19       213
#20     20       148

#A very high number of beers have reviews less than 5
frequency_beer_aggregated$total_percentage <- (frequency_beer_aggregated$frequency*100/nrow(group_by_beer_id))
head(frequency_beer_aggregated)
#44.85% of the beers have just 1 review.
#15.34% have just 2 reviews
#7.63% have just 3 reviews

#A large number of beers have very few reviews. So, we cannot consider such beers for our analysis
#as the data would be too less to train the model.
#Also, in order to make our analysis fruitful, we should consider a thrush hold number for number of
#reviews given by a person as well.
#A person's who has only reviewed one beer won't help our analysis

#After looking at the frequencies of the data:
#Filtering the data to consider at least 50 overall beer reviews and at least 30 overall reviews by a 
#reviewer

#Filtering where beers have at least 50 reviews
beer_subset_50 <- subset(group_by_beer_id, group_by_beer_id$overall_beer_reviews >= 50)
nrow(beer_subset_50)
#Now, we are left with 2064 beers having at least 50 reviews

#Filtering dataset where reviewers have reviewed at least 30 beers
beer_subset_30 <- subset(group_by_profile_name, group_by_profile_name$overall_profile_reviews >= 30)
#Now, we are left with all the profile_name with at least 30 reviews

#Merging both the subsets to get beer ids with at least 50 reviews and by reviewers at least 30 reviews.
beers_to_consider<-merge(beer_data_distinct,beer_subset_50,by.x="beer_beerid",by.y="beer_beerid")
beers_to_consider<-merge(beers_to_consider,beer_subset_30,by.x="review_profilename",by.y="review_profilename")
summary(beers_to_consider)

#review_profilename  beer_beerid    review_overall overall_beer_reviews overall_profile_reviews
#BuckeyeNation :   518   Min.   :    5   Min.   :1.00   Min.   : 50.0        Min.   :  30.0         
#mikesgroove   :   505   1st Qu.: 1089   1st Qu.:3.50   1st Qu.: 97.0        1st Qu.:  78.0         
#BEERchitect   :   460   Median : 4161   Median :4.00   Median :171.0        Median : 155.0         
#northyorksammy:   455   Mean   :15774   Mean   :3.87   Mean   :233.8        Mean   : 233.1         
#WesWes        :   455   3rd Qu.:29619   3rd Qu.:4.50   3rd Qu.:314.0        3rd Qu.: 299.0         
#TheManiacalOne:   440   Max.   :75086   Max.   :5.00   Max.   :977.0        Max.   :1842.0         
#(Other)       :227255 

#The following entries now have enough data to perform analysis.

#2.
#Convert this data frame to a “realratingMatrix” before you build your collaborative filtering models
rating_matrix <- as(beers_to_consider[,c(1,2,3)], "realRatingMatrix")
class(rating_matrix)

#Check for successful conversion
head(rowMeans(rating_matrix))
head(rowCounts(rating_matrix))
head(colCounts(rating_matrix))
#Successfully converted

#Converting the matrix into a data frame for operations and analysis
beers <- as(rating_matrix, "data.frame")
summary(beers)
str(beers)

#----------------------------------------------------------------------------------------------------------#
#Data Exploration
#Determine how similar the first ten users are with each other and visualize it
#Compute and visualize the similarity between the first 10 beers
#What are the unique values of ratings?
#Visualize the rating values and notice:
#1.The average beer ratings
#2.The average user ratings
#3.The average number of ratings given to the beers
#4.The average number of ratings given by the users

#1.Determine how similar the first ten users are with each other and visualize it
similar_first_ten_users <- similarity(rating_matrix[1:10,],method = "cosine",which = "users")
as.matrix(similar_first_ten_users)
image(as.matrix(similar_first_ten_users), main = "Similarity(users)")

#2.Compute and visualize the similarity between the first 10 beers
similar_first_ten_beers <- similarity(rating_matrix[,1:10],method = "cosine",which = "items")
as.matrix(similar_first_ten_beers)
image(as.matrix(similar_first_ten_beers), main = "Similarity(beers)")

#3.What are the unique values of ratings?
beers %>% group_by(rating) %>% summarise(frequency_rating=n()) %>% nrow()
#There are 9 different ratings in the data set

#Calculating the frequency of each rating
beers %>% group_by(rating) %>% summarise(frequency_rating=n())
## A tibble: 9 x 2
#  rating frequency_rating
#   <dbl>            <int>
#1    1               1120
#2    1.5             1343
#3    2               4474
#4    2.5             7012
#5    3              21189
#6    3.5            42543
#7    4              88240
#8    4.5            51086
#9    5              13081


#4.Visualize the rating values and notice:
#1.The average beer ratings
beer_rating_avg<-beers %>% group_by(item) %>% summarise(avg_rating=mean(rating))
ggplot(beer_rating_avg,aes(x=avg_rating)) + geom_histogram() + labs(x="Average beer rating", y="Number of Beers") + scale_x_discrete(limits=1:5)
summary(beer_rating_avg$avg_rating)
#The average beer rating is 3.806(Mean)
#With median 3.870
#This is almost normal curve which is slightly skewed towards left

#Comparison with the given data set. Without any cleaning
beer_rating_avg_overall<-beer_data_distinct %>% group_by(beer_beerid) %>% summarise(avg_rating=mean(review_overall))
ggplot(beer_rating_avg_overall,aes(x=avg_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Beers")
summary(beer_rating_avg_overall$avg_rating)
#Mean=3.6, Median=3.8
#The distribution is uneven

#2.The average user ratings
user_rating_avg<-beers %>% group_by(user) %>% summarise(avg_rating=mean(rating))
ggplot(user_rating_avg,aes(x=avg_rating)) + geom_histogram() + labs(x="Average user rating", y="Number of Users")
summary(user_rating_avg$avg_rating)
#Mean=3.87, Median=3.90, Uneven Distribution and slightly left skewed


#3.The average number of ratings given to the beers
beer_reviews_average<-beers_to_consider %>% group_by(beer_beerid) %>% summarise(avg_review=mean(overall_beer_reviews))
ggplot(beer_reviews_average,aes(x=avg_review)) + geom_histogram() + labs(x="Average Rating", y="Number of Beers")
summary(beer_reviews_average$avg_review)
#A beer is reviewed by 143 people on an average as per the selected subset of original data

#Comparison with the given data set. Without any cleaning
beer_reviews_average_overall<-group_by_beer_id %>% group_by(beer_beerid) %>% summarise(avg_review=mean(overall_beer_reviews))
ggplot(beer_reviews_average_overall,aes(x=avg_review)) + geom_histogram() + labs(x="Average Rating", y="Number of Users")
summary(beer_reviews_average_overall$avg_review)
#Overall a beer is reviewed by approximately 12 people

#4.The average number of ratings given by the users
user_reviews_average<-beers_to_consider %>% group_by(review_profilename) %>% summarise(avg_review=mean(overall_profile_reviews))
ggplot(user_reviews_average,aes(x=avg_review)) + geom_histogram()
summary(user_reviews_average$avg_review)
#Mean=121
#A user reviews 121 beers on an average according to this data set

#Comparison with the given data set. Without any cleaning
user_reviews_average_overall<-group_by_profile_name%>% group_by(review_profilename) %>% summarise(avg_review=mean(overall_profile_reviews))
ggplot(user_reviews_average_overall,aes(x=avg_review)) + geom_histogram() + labs(x="Average Rating", y="Number of Users")
summary(user_reviews_average_overall$avg_review)
#Mean=21 | An average person reviews 21 beers in the given dataset

#Visualizations with the rating matrix
ratings_histogram <- qplot(getRatings(rating_matrix), binwidth = 1, main = "Ratings histogram", xlab = "Rating received", ylab = "counts")
ratings_histogram
#slightly skewed towards right

normal_rating_histogram <- qplot(getRatings(normalize(rating_matrix, method = "Z-score")),main = "Normalized rating Histogram", xlab = "Rating", ylab = "counts")
normal_rating_histogram
#better visualization

average_rated_beer <- qplot(rowCounts(rating_matrix), binwidth = 10, main = "Average rated beer", xlab = "Number of users", ylab = "Number of beers rated")
average_rated_beer
#Most of the users have rated very less beers.
#Very few have rated more

#----------------------------------------------------------------------------------------------------------#
#Recommendation Models
#Divide your data into training and testing datasets
#1.Experiment with 'split' and 'cross-validation' evaluation schemes

#Scheme 1 with train/test(80/20) using split without CV and good rating = 4
scheme1 <- evaluationScheme(rating_matrix, method = "split", train = .8,k = 1, given = -1, goodRating = 4)
scheme1

#Scheme 2 using Cross Validation , 5 folds and good rating = 4
scheme2 <- evaluationScheme(rating_matrix, method = "cross-validation",k = 5, given = -1, goodRating = 4)
scheme2

#2.Build IBCF and UBCF models
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

results1 <- evaluate(scheme1, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results1)

results2 <- evaluate(scheme2, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results2)

#3.Compare the performance of the two models and suggest the one that should be deployed
#1.Plot the ROC curves for UBCF and IBCF and compare them
#Plotting the ROC curve for result 1
plot(results1, annotate = 1:4, legend="topleft")

#Plotting the ROC curve for result 2
plot(results2, annotate = 1:4, legend="topleft")

#Result
#The UBCF is better than the IBCF for higher values of n
#So, we should consider UBCF for recommendation engine

#4.Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
#Recommendations using UBCF
r <- Recommender(rating_matrix, method = "UBCF")
r
#cokes
cokes_recommendation <- predict(r, rating_matrix['cokes'], n=5)
as(cokes_recommendation, "list")
#Top 5 beers that I would recommend this user would be:
#BeerIds: 7971 | 645 | 1346 | 582 | 2041

#genog
genog_recommendation <- predict(r, rating_matrix['genog'], n=5)
as(genog_recommendation, "list")
#Top 5 beers that I would recommend this user would be:
#BeerIds: 57908 | 1160 | 1093 | 1161 | 1445

#giblet
giblet_recommendation <- predict(r, rating_matrix['giblet'], n=5)
as(giblet_recommendation, "list")
#BeerIds: 19960 | 4083 | 582 | 11757 | 2041
#----------------------------------------------------------------------------------------------------------#
#END