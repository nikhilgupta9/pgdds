#Data sets provided mainly contain:
#1. Demographics of employee, Attrition of these employees based on past data & variables showing current data of employees
#2.Employee Survey of job satisfaction
#3.Manager's Survey of employee performance
#4.In-time & Out-time to keep track of employee work hours, leaves & productivity

#Problem Statement :
#Attrition rate is 15% of the company as its employees leave the company/get fired  every year and need to be replaced.
#The management has contracted an HR analytics firm to understand what factors they should focus on,in order to curb attrition. 
#In other words, they want to know what changes they should make to their workplace, 
#in order to get most of their employees to stay. 
#Also, they want to know which of these variables is most important and needs to be addressed right away.

#Repercussions of the high attrition rate are currently resulting in :
#1.difficult to meet timelines - reputation loss
#2.A sizeable department has to be maintained, for the purposes of recruiting new talent
#3.new employees have to be trained for the job and/or given time to acclimatise themselves to the company


#Solution :- Binary classification method using Logistic Regression where there are two possible outputs -
#To see when will an employee leave/get fired or Not Leave/not get fired.

# Install and Load the required packages
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("cowplot")
install.packages("GGally")
install.packages("dplyr")
install.packages("tidyr")
install.packages("e1071")


library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(tidyr)
library(dplyr)
library(GGally)

#loading the files

emp_survey_data<-read.csv("employee_survey_data.csv")
general_data<-read.csv("general_data.csv")
mgr_survey_data<-read.csv("manager_survey_data.csv")
intime<-read.csv("in_time.csv")
outime<-read.csv("out_time.csv")

str(emp_survey_data)    # 4410 obs of 4 variables including the target variable
str(general_data) # 4410 obs of 24 variables
str(mgr_survey_data) # 4410 obs of 3 variables
str(intime) # 4410 obs of 262 variables
str(outime) # 4410 obs of 262 variables



#data cleaning and data preparation

#changing the names of the first column of intime and outime

names(intime)[1]<-"EmployeeID"
names(outime)[1]<-"EmployeeID"


#removing columns with 100% of NA values as 109080 NA's are equally present in both intime and outime
sum(is.na(intime))     #109080 NA's assuming are the total of : holidays/leaves taken/business travel
sum(is.na(outime))   #109080 NA's assuming are the total of :  holidays/leaves taken/business travel

#since both have same number of NA's, which shows in-out are same. So we can remove NA's from the dataset
#there are basically columns with 100% NA values, assuming those are holidays/leaves taken/business travel:

missing_values <- intime %>% summarise_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

good_features <- filter(missing_values,missing_percentage<1)
good_features <- (good_features$feature)

intime <- intime[,(colnames(intime) %in% good_features)]

#removing missing values for outtime

missing_values <- outime %>% summarise_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

good_features <- filter(missing_values,missing_percentage<1)
good_features <- (good_features$feature)

outime <- outime[,(colnames(outime) %in% good_features)]

#From both the data sets the variables with 100% NA values have been removed
# These variables with 100% NA values are the varibales that we are assuming to be holidays & leaves taken

#converting all the values in intime data set into date format

intime_dateconversion<-sapply(intime[,c(2:250)], function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S"))

#converting all the values in outime data set into date format

outime_dateconversion<-sapply(outime[,c(2:250)], function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S"))

intime_dateconversion<-data.frame(intime_dateconversion)
outime_dateconversion<-data.frame(outime_dateconversion)

#substracting intime with outime to calculate the number of work hours of an employeee on a daily basis
work_hours<-outime_dateconversion-intime_dateconversion

#appending NA values to 0
work_hours[is.na(work_hours)] <- 0


#converting the values of the dataframe - work_hours into numeric
work_hours<-lapply(work_hours, as.numeric)
work_hours<-data.frame(work_hours)

str(work_hours)

#caluclating average time of each employee

work_hours$averagetime<-rowMeans(work_hours)
Target <-subset(work_hours,select =("averagetime"))
Target2<-subset(intime, select = ("EmployeeID"))

#new dataset with 2 variable employee ID and average workingtime
emp_avg_work_hrs<-data.frame(Target, Target2)

#before merging the datasets
#confirming EmployeeID is the key

length(unique(emp_survey_data$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(general_data$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(mgr_survey_data$EmployeeID)) # 4410, confirming EmployeeID is key
length(unique(emp_avg_work_hrs$EmployeeID)) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID,emp_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,mgr_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,emp_avg_work_hrs$EmployeeID) # Identical EmployeeID across these datasets

#Checking for duplicates 
sum(duplicated(general_data$EmployeeID))    #Return 0
sum(duplicated(emp_survey_data$EmployeeID)) #Return 0
sum(duplicated(mgr_survey_data$EmployeeID))  #Return 0
sum(duplicated(intime$EmployeeID))         #Return 0
sum(duplicated(outime$EmployeeID))        #Return 0

#Merging all the dataframes

Emp_data<- merge(general_data,emp_survey_data, by="EmployeeID", all = F)
Emp_data<-merge(Emp_data, mgr_survey_data, by= "EmployeeID", all = F)
Emp_data<-merge(Emp_data, emp_avg_work_hrs, by= "EmployeeID", all = F)

#Emp_data is the master dataframe with 30 variables and 4410 observations 

#As given in data dictionary, we are not converting the categorical variables (Education,EnvironmentSatisfaction,JobInvolvement,JobSatisfaction,WorkLifeBalance,PerformanceRating) into character values 
#as they are already coded in numeric values which will be appropriate for scaling for regression purpose 

# Understanding the structure of the collated file
str(Emp_data) 

# Education, Joblevel,Environment satisfaction,
#jobsatisfaction, jobinvolvement,worklife satisfaction 
#performance rating need to be changed from integer to categorical


cols <- c("Education", "JobLevel", "StockOptionLevel", "EnvironmentSatisfaction", 
          "JobSatisfaction", "WorkLifeBalance", "JobInvolvement", "PerformanceRating" )
Emp_data[cols] <- lapply(Emp_data[cols], factor)

str(Emp_data)

cols1<-c("BusinessTravel", "Department", "EducationField", "Gender", "JobRole", "MaritalStatus")

#All the observations are being converted to lower case

Emp_data$BusinessTravel<-tolower(Emp_data$BusinessTravel)
Emp_data$Department<-tolower(Emp_data$Department)
Emp_data$EducationField<-tolower(Emp_data$EducationField)
Emp_data$Gender<-tolower(Emp_data$Gender)
Emp_data$JobRole<-tolower(Emp_data$JobRole)
Emp_data$MaritalStatus<-tolower(Emp_data$MaritalStatus)


#checking for NA values

sapply(Emp_data, function(x) sum(is.na(x)))
#19 NA values missing from  NumCompaniesWorked
#9 NA values missing in TotalWorkingYears
#25 NA values missing in EnvironmentSatisfaction
#20 NA values missing in JobSatisfaction
#38 NA values missing in WorkLifeBalance

#since 91 obs would get deleted if we remove NA values completely, hence Imputing frequently rated values in place of NA for
#EnvironmentSatisfaction,JobSatisfaction, WorkLifeBalance

#Removing rows with NA values of TotalWorkingYears and NumCompaniesWorked as they are only 9 and 19
Emp_data <- Emp_data[!is.na(Emp_data$NumCompaniesWorked),]
Emp_data <- Emp_data[!is.na(Emp_data$TotalWorkingYears),]

#Replacing 25 NA's in EnvironmentSatisfaction with 3
summary(Emp_data$EnvironmentSatisfaction) #rating 3 is more frequent
Emp_data$EnvironmentSatisfaction[is.na(Emp_data$EnvironmentSatisfaction)] <- 3

#Replacing 20 NA's in JobSatisfaction with 4
summary(Emp_data$JobSatisfaction) #rating 4 is more frequent
Emp_data$JobSatisfaction[is.na(Emp_data$JobSatisfaction)] <- 4

#Replacing 38 NA's in WorkLifeBalance with 3
summary(Emp_data$WorkLifeBalance) #rating 3 is more frequnet
Emp_data$WorkLifeBalance[is.na(Emp_data$WorkLifeBalance)] <- 3

sapply(Emp_data, function(x) sum(is.na(x)))

str(Emp_data)
#NA values has been imputed

##########################################################################################


#based on standard hours, creating a new variable - worktime, to calculate the number of hours worked above/below standard work hours by an employee
Emp_data$Worktime<-Emp_data$StandardHours-Emp_data$averagetime

Emp_data$Worktime<- ifelse(Emp_data$Worktime>0 , "overtime","lesstime")
Emp_data$Worktime<- as.factor(Emp_data$Worktime)

str(Emp_data) #4410 obs with 31 variables


##########################################################################################


# Barcharts for categorical features with stacked employee information
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(Emp_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(Emp_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme,
          align = "h")   

plot_grid(ggplot(Emp_data, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=Over18,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme,
          align = "h") 

plot_grid(ggplot(Emp_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme,
          align = "h") 

plot_grid(ggplot(Emp_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(Emp_data, aes(x=Worktime,fill=Attrition))+ geom_bar()+bar_theme,
          align = "h") 

#Attrition rates seem to be high in proportion for employees who
#travel rarely,working in research & development dept.,lower level of education, gender wise -men in proportion,marital status - single
#Contrast in attrition rates for EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, JobInvolvement
#Attrition rate seems to be high in proportion for employees who work lesser than standard hours - employees could feel less productive or underutilised


#################################################################################################################################################################



# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(Emp_data, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(Emp_data, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) #no outliers

plot_grid(ggplot(Emp_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(Emp_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) #no outliers

plot_grid(ggplot(Emp_data, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(Emp_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) #outliers present

quantile(Emp_data$MonthlyIncome, seq(0,1,0.01)) 
#outliers present here are left without being treated as the values may be relevant


plot_grid(ggplot(Emp_data, aes(NumCompaniesWorked))+ geom_histogram(),
          ggplot(Emp_data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 
quantile(Emp_data$NumCompaniesWorked, seq(0,1,0.01)) #no outliers


plot_grid(ggplot(Emp_data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(Emp_data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

quantile(Emp_data$TotalWorkingYears, seq(0,1,0.01)) #outliers present

#fixing outliers for TotalWorkingYears
box <- boxplot.stats(Emp_data$TotalWorkingYears)
out <- box$out

Emp_data1 <- Emp_data[ !Emp_data$TotalWorkingYears %in% out, ]

Emp_data <- Emp_data1


plot_grid(ggplot(Emp_data, aes(TrainingTimesLastYear))+ geom_histogram(),
          ggplot(Emp_data, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

quantile(Emp_data$TrainingTimesLastYear, seq(0,1,0.01)) #no outliers 


plot_grid(ggplot(Emp_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(Emp_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

quantile(Emp_data$YearsAtCompany, seq(0,1,0.01)) #outliers present

#fixing outliers for YearsAtCompany
box <- boxplot.stats(Emp_data$YearsAtCompany)
out <- box$out

Emp_data1 <- Emp_data[ !Emp_data$YearsAtCompany %in% out, ]

Emp_data <- Emp_data1


plot_grid(ggplot(Emp_data, aes(YearsSinceLastPromotion))+ geom_histogram(),
          ggplot(Emp_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

quantile(Emp_data$YearsSinceLastPromotion, seq(0,1,0.01)) #no outliers

plot_grid(ggplot(Emp_data, aes(YearsWithCurrManager))+ geom_histogram(),
          ggplot(Emp_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

quantile(Emp_data$YearsWithCurrManager, seq(0,1,0.01)) #outliers present

#fixing outliers for YearsAtCompany
box <- boxplot.stats(Emp_data$YearsWithCurrManager)
out <- box$out

Emp_data1 <- Emp_data[ !Emp_data$YearsWithCurrManager %in% out, ]

Emp_data <- Emp_data1


plot_grid(ggplot(Emp_data, aes(averagetime))+ geom_histogram(),
          ggplot(Emp_data, aes(x="",y=averagetime))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

quantile(Emp_data$averagetime, seq(0,1,0.01)) #no outliers

#outliers have been checked and treated 


# Boxplots of numeric variables relative to employee data against attrition status
plot_grid(ggplot(Emp_data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Emp_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Emp_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(Emp_data, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Emp_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Emp_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(Emp_data, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Emp_data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Emp_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(Emp_data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Emp_data, aes(x=Attrition,y=averagetime, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

#Median age = 30 for attrition, less Years At Company resulting in attrition, Years Since Last Promotion could also be a factor
# Less Years With Current Manager could result in attrition, Overworked employees could also churn


#########################################################################################################################################


#Correlation between numeric variables
library(GGally)
ggpairs(Emp_data[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear",
                     "YearsAtCompany","YearsSinceLastPromotion", "YearsWithCurrManager", "averagetime")])


#YearsAtCompany & YearsSinceLastPromotion are correlated
#YearsAtCompany & YearsWithCurrManager are highly correlated


####################################################################################################################################
### Data Preparation

#converting BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus  to factors
str(Emp_data)
cols1 <- c("BusinessTravel", "Department", "EducationField", "Gender", 
          "JobRole", "MaritalStatus")
Emp_data[cols1] <- lapply(Emp_data[cols1], factor)

str(Emp_data)
#Removing unnecessary variables from the dataset 
#Removing EmployeeID, EmployeeCount, Over18 and StandardHours
#because they won't be of any significance 
#Removing averagetime becuase that variable is converted to working hours with factors lesstime and overtime

Emp_data<-Emp_data[, -c(1,9,16,18)] # 3953 obs of 27 variables
str(Emp_data)
#********************************************************************************#

#Standardising continuous variables of Emp_data 

Emp_data$Age<-scale(Emp_data$Age)
Emp_data$DistanceFromHome<-scale(Emp_data$DistanceFromHome)
Emp_data$MonthlyIncome<-scale(Emp_data$MonthlyIncome)
Emp_data$NumCompaniesWorked<-scale(Emp_data$NumCompaniesWorked)
Emp_data$PercentSalaryHike<-scale(Emp_data$PercentSalaryHike)
Emp_data$TotalWorkingYears<-scale(Emp_data$TotalWorkingYears)
Emp_data$TrainingTimesLastYear<-scale(Emp_data$TrainingTimesLastYear)
Emp_data$YearsAtCompany<-scale(Emp_data$YearsAtCompany)
Emp_data$YearsSinceLastPromotion<-scale(Emp_data$YearsSinceLastPromotion)
Emp_data$YearsWithCurrManager<-scale(Emp_data$YearsWithCurrManager)
Emp_data$averagetime<-scale(Emp_data$averagetime)



# converting target variable - attrition from No/Yes,worktime from overtime/lesstime character values to factor with levels 0/1 
Emp_data$Attrition<- ifelse(Emp_data$Attrition=="Yes",1,0)
Emp_data$Worktime<- ifelse(Emp_data$Worktime=="overtime",1,0)


#extracting factor variables from the data set

Employee_data_fact<-Emp_data[,-c(2,5,12,13,14,16,17,18,19,20,26,27)]
str(Employee_data_fact)

# creating dummies for factor variables

dummies<- data.frame(sapply(Employee_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =Employee_data_fact))[,-1]))

# Final dataset
Employee_final<- cbind(Emp_data[,c(2,1,5,12,13,14,16,17,18,19,20,26,27)],dummies)
str(Employee_final)
#final dataset is ready for modelling with 3953 observations and 57 variables 


# Checking attrition rate of employee
Attrition_rate <- sum(Employee_final$Attrition)/nrow(Employee_final)
Attrition_rate # 16.9% attrition rate. 


########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(Employee_final$Attrition, SplitRatio = 0.7)

train = Employee_final[indices,]

test = Employee_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 1941.5....57 coeff..nullDev 2521.6...resDev 1827.5

########################################################################################################################
#Model using STEPAIC function, Removing multicollinearity through VIF check
#######################################################################################################################

model_2<- stepAIC(model_1, direction="both")

summary(model_2)

sort((vif(model_2)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Excluding YearsAtCompany with high VIF & less significance

model_3 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averagetime + 
                 Worktime + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                 Department.xresearch...development + Department.xsales + 
                 EducationField.xtechnical.degree + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)
summary(model_3)

sort((vif(model_3)), decreasing = TRUE)

#Excluding averagetime with high VIF & less significance

model_4 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                 BusinessTravel.xtravel_rarely + Department.xresearch...development + 
                 Department.xsales + EducationField.xtechnical.degree + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_4)

sort((vif(model_4)), decreasing = TRUE)

#Excluding BusinessTravel.xtravel_rarely with high VIF & less significance

model_5 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                 Department.xresearch...development + 
                 Department.xsales + EducationField.xtechnical.degree + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_5)

sort((vif(model_5)), decreasing = TRUE)

#Excluding WorkLifeBalance.x2 with high VIF & less significance

model_6 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                 Department.xresearch...development + 
                 Department.xsales + EducationField.xtechnical.degree + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_6)

sort((vif(model_6)), decreasing = TRUE)


#Department.xresearch...development & Department.xsales have high VIFs and also very significant since the beginning.Hence cannot eliminate.
#TotalWorkingYears has high VIF but is also highly significant

#Excluding variables based on p significance as variables with high VIF cannot be excluded due to high p significance

#Excluding NumCompaniesWorked with less significance

model_7 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                 Department.xresearch...development + 
                 Department.xsales + EducationField.xtechnical.degree + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_7)

#Excluding WorkLifeBalance.x4 with less significance

model_8 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                 Department.xresearch...development + 
                 Department.xsales + EducationField.xtechnical.degree + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_8)

#Excluding MonthlyIncome with less significance

model_9 <- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                 Department.xresearch...development + 
                 Department.xsales + EducationField.xtechnical.degree + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_9)

#Excluding EducationField.xtechnical.degree  with less significance

model_10 <- glm(formula = Attrition ~ Age + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                 Department.xresearch...development + 
                 Department.xsales + JobRole.xhuman.resources + 
                 JobRole.xmanager + JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                 MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_10)

#Excluding JobRole.xresearch.director with less significance

model_11 <- glm(formula = Attrition ~ Age + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  Department.xresearch...development + 
                  Department.xsales + JobRole.xhuman.resources + 
                  JobRole.xmanager + JobRole.xmanufacturing.director + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_11)

#Excluding JobRole.xmanager  with less significance

model_12 <- glm(formula = Attrition ~ Age + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  Department.xresearch...development + 
                  Department.xsales + JobRole.xhuman.resources + 
                  JobRole.xmanufacturing.director + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_12)

#Excluding  JobRole.xhuman.resources with less significance

model_13 <- glm(formula = Attrition ~ Age + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  Department.xresearch...development + 
                  Department.xsales +  JobRole.xmanufacturing.director + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_13)

#Excluding  JobRole.xmanufacturing.director  with less significance

model_14 <- glm(formula = Attrition ~ Age + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  Department.xresearch...development + 
                  Department.xsales +  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_14)

#Excluding  JobInvolvement.x3 with less significance

model_15 <- glm(formula = Attrition ~ Age + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  Department.xresearch...development + 
                  Department.xsales +  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3  
                  , family = "binomial", 
                data = train)

summary(model_15)

#Excluding Department.xresearch...development  with less significance

model_16 <- glm(formula = Attrition ~ Age + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  Department.xsales +  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3  
                , family = "binomial", 
                data = train)

summary(model_16)

#Excluding Department.xsales with less significance

model_17 <- glm(formula = Attrition ~ Age + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3  
                , family = "binomial", 
                data = train)

summary(model_17)

#Excluding Age with less significance 

model_18 <- glm(formula = Attrition ~  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3  
                , family = "binomial", 
                data = train)

summary(model_18)

#Excluding TotalWorkingYears  with less significance

model_19 <- glm(formula = Attrition ~  
                  TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3  
                , family = "binomial", 
                data = train)

summary(model_19)

#Excluding TrainingTimesLastYear with less significance

model_20 <- glm(formula = Attrition ~  
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3  
                , family = "binomial", 
                data = train)

summary(model_20)

#Excluding JobSatisfaction.x2 with less significance

model_21 <- glm(formula = Attrition ~  
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3  
                , family = "binomial", 
                data = train)

summary(model_21)

#Excluding JobSatisfaction.x3  with less significance

model_22 <- glm(formula = Attrition ~  
                  YearsSinceLastPromotion + 
                  YearsWithCurrManager + Worktime + BusinessTravel.xtravel_frequently + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +JobSatisfaction.x4 + WorkLifeBalance.x3  
                , family = "binomial", 
                data = train)

summary(model_22)

########################################################################
# 10 significant variables in the model
#YearsSinceLastPromotion, YearsWithCurrManager, Worktime, BusinessTravel.xtravel_frequently ,
#MaritalStatus.xsingle, EnvironmentSatisfaction.x2, EnvironmentSatisfaction.x3 ,
#EnvironmentSatisfaction.x4 ,JobSatisfaction.x4 , WorkLifeBalance.x3  are the variables affecting attrition of employees significantly

final_model<- model_22

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test) #employeeid excluded prior to modelling


# Let's see the summary 

summary(test_pred)  #Prediction probability from 0.6% to 82%

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition  <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition) #understanding the discrepency between the actual data & predicted data
#Accuracy of the model is 85% i.e.(962+41/962+22+161+41 = 84.5%)


confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes") 
#Sensitivity (Proportion of Yeses correctly predicted) = Attrition Accuracy = 20.2%
#Specificity (Proportion of No's correctly predicted) = Non Attrition Accuracy = 97.8%

#Even though overall model accuracy is good - 85%, Employee Attrition Rate Accuracy is equal to only 20.2%

#######################################################################

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No")) #trying to predict through a different threshold of probability - 0.4


test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#Accuracy - 83.3%
#Sensitivity - 26.23%
#Specificity - 95%
#Sensitivity has gone up but still not high enough for Attrition Accuracy
#######################################################################

test_pred_attrition <- factor(ifelse(test_pred >= 0.30, "Yes", "No")) #trying to predict through a different threshold of probability - 0.3


test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#Accuracy - 82.1%
#Sensitivity - 48.5%
#Specificity - 89%
#With probability of 30% employees will churn (0.3), model is predicting 48.5% accuracy with attrition which is higher than previous

#########################################################################################
# Choosing the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.006792 to 0.825593 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.18 (18% probability of attrition) for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.18, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #70.6%

sens #72.7%

spec #70.2%

#0.18 gives an optimal model with accuracy, sensitivity & specificity at par

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 42.9% KS statistic

#It is a good model as KS statistic is more than 40% and would lie in the top few deciles (1st to 4th).


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

Attrition_decile

#by the 4th decile,then among the top 40% employees who are sorted according to the probability in decreasing order,
#74.3% of those employees are likely to leave/get fired.

#by the model's gain by the end of the 3rd decile is 2.1 times that of a random model's gain at the end of 3 deciles. 
#In other words, the model catches 2.1 times more attritions than a random model would have caught.

