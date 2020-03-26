## Load the following packages. If not installed, use 'install.packages("Name")' command to install
library(tidyverse)
library(MASS)
library(plyr)
library(car)

# Reading the data file. The file should be present in the Working Directory
CarPrices <- read.csv("CarPrice_Assignment.csv")

View(CarPrices)
str(CarPrices)

##Creating levels
# Diesel (0), Gas(1)
levels(CarPrices$fueltype) <- c(0,1) 
CarPrices$fueltype <- as.numeric(levels(CarPrices$fueltype))[CarPrices$fueltype]

# Std (0), Turbo(1)
levels(CarPrices$aspiration) <- c(0,1) # 
CarPrices$aspiration <- as.numeric(levels(CarPrices$aspiration))[CarPrices$aspiration]

#Four (0), Two (1)
levels(CarPrices$doornumber) <- c(0,1) 
CarPrices$doornumber <- as.numeric(levels(CarPrices$doornumber))[CarPrices$doornumber]

#Front (0), Rear(1)
levels(CarPrices$enginelocation) <- c(0,1) 
CarPrices$enginelocation <- as.numeric(levels(CarPrices$enginelocation))[CarPrices$enginelocation]

#Using 'Separate' function to seperate CarName into CarName and Type
CarPricesData <- separate(CarPrices, CarName,into = c("CarName", "Type"),sep = " ")
View(CarPricesData)


#Converting the Multi-level variables to dummy numbers and then to numbers
dummy_carbody <- data.frame(model.matrix( ~carbody, data = CarPricesData))
View(dummy_carbody)
dummy_carbody <- dummy_carbody[,-1]

dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = CarPricesData))
View(dummy_drivewheel)
dummy_drivewheel <- dummy_drivewheel[,-1]

dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = CarPricesData))
View(dummy_enginetype)
dummy_enginetype <- dummy_enginetype[,-1]

dummy_cylinder <- data.frame(model.matrix( ~cylindernumber, data = CarPricesData))
View(dummy_cylinder)
dummy_cylinder <- dummy_cylinder[,-1]

dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = CarPricesData))
View(dummy_fuelsystem)
dummy_fuelsystem <- dummy_fuelsystem[,-1]

#Combining dummy variables to the dataset
CarPrices_1 <- cbind(CarPricesData[,setdiff(names(CarPricesData),
                                           c("Type","carbody","drivewheel","enginetype",
                                             "cylindernumber","fuelsystem"))], 
                    dummy_carbody, dummy_drivewheel, dummy_enginetype, dummy_cylinder, dummy_fuelsystem)
View(CarPrices_1)
str(CarPrices_1)

#Derived Columns
CarPrices_1$Total_MPG <- round(mean(CarPrices_1$citympg + CarPrices_1$highwaympg),2)            #TotalMPG = CityMPG + HighwayMPG

CarPrices_1$SBR <- round(CarPrices_1$stroke/CarPrices_1$boreratio,2)                       #Stroke to Bore Ratio

CarPrices_1$Total_HP <- round(CarPrices_1$Total_MPG/CarPrices_1$horsepower, 2)                       #Total MPG/HorsePower

CarPrices_1$MCB <- round(CarPrices_1$Total_MPG/CarPrices_1$curbweight, 4)                        #TotalMPG/CurbWeight

#Identified some mis-spelled car names
# Data Cleaning - replacing incorrect names with correct car names
levels(as.factor(CarPrices_1$CarName))
company <- mapvalues(CarPrices_1$CarName, from = c("maxda", "porcshce", "vokswagen", 
                                                  "vw", "Nissan", "toyouta"), to = c("mazda", 
                                                                                     "porsche", "volkswagen", "volkswagen", "nissan", "toyota"))
CarPrices_1 <- cbind(CarPrices_1[,-3],company)
CarPrices_1$company <- as.factor(CarPrices_1$company)

set.seed(9999) #Setting seed
options(scipen = 999)

# Creating data sets for training and testing
trainingindices= sample(1:nrow(CarPrices_1), 0.7*nrow(CarPrices_1))
training = CarPrices_1[trainingindices,]
testing = CarPrices_1[-trainingindices,]

# Following Models that are created and replaced
#All variables
Model_1 <-lm(price~.,data=training)
summary(Model_1)

#Estimating the model
step <- stepAIC(Model_1, direction = "both")

Model_2 <- lm(price ~ car_ID + aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                SBR + Total_HP + company, data = training)
summary(Model_2)
vif(Model_2)

#Dropping columns having very high VIF (Car_ID, curbweight etc)
Model_3 <- lm(price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                SBR + Total_HP + company, data = training)
summary(Model_3)
## Checking Multicollinearity
vif(Model_3) 

#Dropping columns having low significance
Model_4 <- lm(price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                SBR + Total_HP + company, data = training)
summary(Model_4)
## Checking Multicollinearity
vif(Model_4)

Model_5 <- lm(price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypel + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                SBR + Total_HP + company, data = training)
summary(Model_5)
## Checking Multicollinearity
vif(Model_5)

Model_6 <- lm(price ~ aspiration + wheelbase + carlength + 
                carheight + curbweight + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                Total_HP + company, data = training)
summary(Model_6)
## Checking Multicollinearity
vif(Model_6)

Model_7 <- lm(price ~ aspiration + wheelbase + carlength + 
                carheight + curbweight + carbodyhatchback +
                carbodywagon + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                company, data = training)
summary(Model_7)
## Checking Multicollinearity
vif(Model_7)

Model_8 <- lm(price ~ wheelbase + carlength + carheight + curbweight 
              + carbodyhatchback  + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + fuelsystemmpfi + 
                company, data = training)
summary(Model_8)
vif(Model_8)

Model_9 <- lm(price ~ wheelbase + carlength + carheight + curbweight 
              + carbodyhatchback  + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + 
                company, data = training)
summary(Model_9)
## Checking Multicollinearity
vif(Model_9)

Model_10 <- lm(price ~ wheelbase + carlength + carheight + curbweight 
               + cylindernumberfive +
                 cylindernumberfour + cylindernumbersix + 
                 company, data = training)
summary(Model_10)
## Checking Multicollinearity
vif(Model_10)

#Significant parameters identified. We can now go forward using this for the prediction
#Predicting the results using the testing dataset
Predict_1 <- predict(Model_10,testing[,-20])
testing$testing_price <- Predict_1

#Now testing b/w the rsquare and the adjusted rsquare calculated
r <- cor(testing$price,testing$testing_price)
rsquared <- cor(testing$price,testing$testing_price)^2
rsquared
