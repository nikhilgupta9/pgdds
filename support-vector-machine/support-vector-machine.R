#Importing data from CSV files into RStudio
#The files to be imported should be present in the working directory of R
#Use getwd(); command to check your working directory and paste the files to be imported there.

TestingDF <- read.delim("mnist_test.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)
TrainingDF <- read.delim("mnist_train.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)

# Data Understanding
#The data consists of 28x28 handwritten digit data
# 28x 28 = 784 columns
#These columns have the pixel intensity for each pixel from 0 to 255


# Data Preparation
# Changing the column name of the first column of both the data frames to 'Actual Digit'
colnames(TestingDF)[1] <- "Actual_Digit"
colnames(TrainingDF)[1] <- "Actual_Digit"

#Adding a new column at the end of the data frames for Identification
TestingDF$Data <- "TestData"
TrainingDF$Data <- "TrainingData"

#Understanding the dimensions of the data frames
dim(TestingDF)
dim(TrainingDF)

#Printing the first 5 rows of the data frames
head(TestingDF)
head(TrainingDF)

#Converting the Actual Digit column as factor
TestingDF$Actual_Digit <- as.factor(TestingDF$Actual_Digit)
TrainingDF$Actual_Digit <- as.factor(TrainingDF$Actual_Digit)

#Checking for NA values
sum(is.na(TrainingDF))
sum(is.na(TestingDF))

#Checking for minimum values in both data frames
sapply(TrainingDF[,-1], min)
sapply(TestingDF[,-1], min)

#Checking whether both the data frames are numeric or not
TrainingDF[, lapply(TrainingDF[,-1], is.numeric) == FALSE] 
TestingDF[, lapply(TestingDF[,-1], is.numeric) == FALSE]

#Checking the dataset for outliers
sapply(TrainingDF[,-1], min)
min(sapply(TrainingDF[,-1], min))
sapply(TrainingDF[,-1], max)
max(sapply(TrainingDF[,-1], max))


MergeDF <- rbind(TrainingDF, TestingDF) #Merging data frames for data cleaning

sum(is.na(MergeDF)) #Check for NA values

#Checking for columns with no variation in data
SameValueColumns <- which(sapply(MergeDF,function(x) length(unique(x)) == 1))
NumOfColumns <-length(SameValueColumns)
NumOfColumns
SameValueColumns

#Removing the same data columns
library(dplyr)
MergeDF2 <- MergeDF %>% select(-SameValueColumns)

#Checking for pixel density > 255 and Actual_digit greater than 9 OR less than 0
which(MergeDF2$Actual_Digit > "9" | MergeDF2$Actual_Digit < "0")
which(MergeDF2 %>% select(-Actual_Digit, -Data) > 255)


#Seperating the merged data frame into Training and Testing
TrainingDF2 <- MergeDF2 %>% filter(Data == "TrainingData") %>% select(-Data)
TestingDF2 <- MergeDF2 %>% filter(Data == "TestData") %>% select(-Data)

str(TrainingDF2$Actual_Digit) #Checking for factor
str(TestingDF2$Actual_Digit)  #Checking for factor

set.seed(100) #Setting seed


#As the dataset is large. We are taking a sample of it. Once we are aking .6666% and then .3334 and will compare both for sampling.
#If they both show same curves, we will take the smaller data set for faster calculation

#Adding packages
library(ggplot2)
library(readr)
library(caret)
library(caTools)

#DataSet 1
TrainingDF2_Sample1 <- sample.split(TrainingDF2$Actual_Digit,SplitRatio = 0.6666)
TrainingSet1 <- TrainingDF2[TrainingDF2_Sample1,]

#DataSet 2
TrainingDF2_Sample2 <- sample.split(TrainingDF2$Actual_Digit,SplitRatio = 0.3334)
TrainingSet2 <-TrainingDF2[TrainingDF2_Sample2,]

#Plotting the frequency of Actual Digits in both sets


Gplot1 <- ggplot(TrainingSet1) + 
  geom_bar(aes(x=as.factor(Actual_Digit)),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,4700,500)) +
  labs(title="SampleCase1\nTrainingSet = 39996 rows",x="Actual Digit",y="Count")

Gplot2 <- ggplot(TrainingSet2) + 
  geom_bar(aes(x=as.factor(Actual_Digit)),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,2400,200)) +
  labs(title="SampleCase2\nTrainSet = 20004 rows",x="Actual Digit",y="Count")

library(gridExtra)
grid.arrange(Gplot1,Gplot2)

#The grid shows that there is not much difference in the occurences of the digits. Thus, we are taking dataset 2
#Data set 2 to Final training data
FinalTrainingData <- TrainingSet2


#Modelling
#Loading Packages
library(kernlab)

#Linear Model
SVM_Linear_Model_1 <- ksvm(Actual_Digit~., data=FinalTrainingData, scale=FALSE, kernel="vanilladot")
SVM_Linear_Model_1
Prediction1 <- predict(SVM_Linear_Model_1,FinalTrainingData)
confusionMatrix(Prediction1,FinalTrainingData$Actual_Digit)
#Accuracy : 1

Prediction_Testing <- predict(SVM_Linear_Model_1,TestingDF2)
confusionMatrix(Prediction_Testing,TestingDF2$Actual_Digit)
#Overall Statistics
#Accuracy : 0.9132 | #P-Value [Acc > NIR] : < 2.2e-16 

#RBF Modelling
SVM_NL_RBF <- ksvm(Actual_Digit~., data=FinalTrainingData, scale=FALSE, kernel="rbfdot")
SVM_NL_RBF
Prediction_RBF <- predict(SVM_NL_RBF, TestingDF2)
#Evaluating ConfusionMatrix
confusionMatrix(Prediction_RBF, TestingDF2$Actual_Digit)
#RBF performed slightly better than linear

#Performing 3 fold cross validation
Control <- trainControl(method = "cv", number = 3,verboseIter=TRUE)

Metric <- "Accuracy"

set.seed(90) #Setting seed

ModelGrid <- expand.grid(.sigma = c(2.2e-16, 3.2e-16, 4.2e-16),.C=c(1,2,3)) #Creating grid
SVM_NonLin_Fit <- train(Actual_Digit~.,data=FinalTrainingData,method="svmRadial", metric=Metric, tuneGrid=ModelGrid, trControl=Control)
SVM_NonLin_Fit

Graph1<-plot(SVM_NonLin_Fit)
Graph1


#Building Final Model

SVM_NonLin_Fit_Final <- ksvm(Actual_Digit~.,data=FinalTrainingData,kernel="rbfdot",scale=FALSE,C=3,kpar=list(sigma=2.2e-16))
SVM_NonLin_Fit_Final
# Hyper Parameters :C = 3
# sigma = 2.2e-16
# Support vectors  :6317

#Checking the Training Accuracy
Eval_SVM_NonLin_Fit_Final_Tr <- predict(Svm_nlRBF_final,FinalTrainingData)
confusionMatrix(Eval_SVM_NonLin_Fit_Final_Tr,FinalTrainingData$Actual_Digit)
# Here the Accuracy = 0.9989 

#Checking the Testing Accuracy
Eval_SVM_NonLin_Fit_Final_Ts <- predict(SVM_NonLin_Fit_Final,TestingDF2)
confusionMatrix(Eval_SVM_NonLin_Fit_Final_Ts,TestingDF2$Actual_Digit)
# Here the Accuracy = 0.9777
