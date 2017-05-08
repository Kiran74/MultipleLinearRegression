
## Multi Linear Regression###

## Business Understanding

#Business Objective
# Mycar Dream wants to automate the process of predicting the car mileage
# which fits the customer preferences

##Data Understanding

## Reading the data set into R #####

carmileage <- read.csv("carMPG.csv")

### Checking the structure of car mileage

str(carmileage)

############ Data preparation #########################

##### Removing duplicate Rows###

### Checking is there any duplicate rows in the carmileage dataset

length(which(duplicated(carmileage)))

#[1] 0

### There are no duplicate rows in the data.

## Formatting the variables ##### 

### Let us look at each variable and decide the type of it.

## 1. MPG ("numeric" since its a continuous variable)

class(carmileage$MPG)

unique(carmileage$MPG)

## 2. Cylinders (Integer since multi valued discrete variable)

class(carmileage$Cylinders)

unique(carmileage$Cylinders)


## 3. Displacement ("numeric" since it is a continuous variable)

class(carmileage$Displacement)

unique(carmileage$Displacement)


### 4.Horsepower ("numeric since it is a continuous variable")

class(carmileage$Horsepower)

unique(carmileage$Horsepower)

# Converting Factor Variable type to numeric as it is a continous variable

carmileage$Horsepower <- as.numeric(levels(carmileage$Horsepower))[carmileage$Horsepower]

### 5.Weight ("Numeric since it is a continuous variable")

class(carmileage$Weight)

unique(carmileage$Weight)



## 6.Acceleration ("numeric since it is continuous variable")

class(carmileage$Acceleration)

unique(carmileage$Acceleration)

## 7.Model year(" integer since it is multivalued discrete variable")

class(carmileage$Model_year)

unique(carmileage$Model_year)


## 8. Origin (" integer since it is a multivalued discrete variable")

class(carmileage$Origin)

#convert the origin variable to factor variable

carmileage$Origin<-as.factor(carmileage$Origin)

unique(carmileage$Origin)

### 9. Car name


class(carmileage$Car_Name)

unique(carmileage$Car_Name)



##### Data Cleaning : Missing value and outlier treatment for all variables ##### 

## 1.MPG

class(carmileage$MPG)

unique(carmileage$MPG)

boxplot(carmileage$MPG, na.action = NULL)

quantile(carmileage$MPG, probs = seq(0,1,0.025))


### 2. Cylinders

class(carmileage$Cylinders)

unique(carmileage$Cylinders)


### 3.Displacement


class(carmileage$Displacement)

unique(carmileage$Displacement)

boxplot(carmileage$Displacement, na.action = NULL)

## As seen in the box plot there are no outlier values.

###### 4. Horsepower

class(carmileage$Horsepower)

## we replace the NA values with mean.

carmileage$Horsepower[which(is.na(carmileage$Horsepower))] <- mean(carmileage$Horsepower,na.rm = TRUE)

unique(carmileage$Horsepower)

boxplot(carmileage$Horsepower)

## There are outliers in the Horsepower variable so we remove them
## using the capping and flooring method.

plot(carmileage$Horsepower)

quantile(carmileage$Horsepower, probs = seq(0,1,0.005), na.rm = TRUE)

carmileage$Horsepower[carmileage$Horsepower > 170.0000] <- 170.0000

carmileage$Horsepower[carmileage$Horsepower < 58.000] <- 58.000

boxplot(carmileage$Horsepower)

## now we dont see any outlier values in the Horsepower variable


### 5.	Weight

class(carmileage$Weight)

unique(carmileage$Weight)

boxplot(carmileage$Weight)

## we could not see that there are outliers in the Weight variable.
## we check the quantile to check for any exterme values and perform flooring 
## and capping on Weight

quantile(carmileage$Weight, probs = seq(0,1,0.005), na.rm = TRUE)

carmileage$Weight[carmileage$Weight > 4515.560] <- 4515.560

carmileage$Weight[carmileage$Weight < 1753.410] <- 1753.410

#### 6. Acceleration

class(carmileage$Acceleration)

unique(carmileage$Acceleration)

boxplot(carmileage$Acceleration)

## we could see that there are outliers in the Acceleration variable so we remove them
## using the capping and flooring method.

quantile(carmileage$Acceleration, probs = seq(0,1,0.005), na.rm = TRUE)

carmileage$Acceleration[carmileage$Acceleration > 19.6540] <-19.6540

carmileage$Acceleration[carmileage$Acceleration < 9.4850] <- 9.4850

boxplot(carmileage$Acceleration)

## now we dont see any outlier values in the Acceleration variable

###### 7.Model year

class(carmileage$Model_year)

summary(carmileage$Model_year)

#######  8.Origin

class(carmileage$Origin)

summary(carmileage$Origin)

####### 9.Car_Name

class(carmileage$Car_Name)

summary(carmileage$Car_Name)

###################   Variables Transformation #########################

## create CarCompany variable and assign all the sub categories of car names to CarCompany

carmileage$Car_Company <- gsub( " .*$", "", carmileage$Car_Name)

#### There are spelling mistakes in the car company variable so lets us correct that

unique(carmileage$Car_Company)

########## After Verifying all the unique values of car company names 
##we see there are few companies which are wrongly spelled

## "chevroelt" and "chevy" has been wrongly spelled so let us correct it by assigning Chevrolet

carmileage$Car_Company[carmileage$Car_Company %in% c("chevroelt","chevy")]<-"chevrolet"

unique(carmileage$Car_Company)

## "toyota" has been wrongly spelled as "toyouta" 

carmileage$Car_Company[carmileage$Car_Company=="toyouta"]<-"toyota"

## "volkswagen" has been wrongly spelled as "vokswagen" and "vw" 

carmileage$Car_Company[carmileage$Car_Company %in% c("vokswagen","vw")]<-"volkswagen"

## we have "mercedes-benz" and "mercedes" in the dataset
## let us change the mercedes-benz to mercedes  

carmileage$Car_Company[carmileage$Car_Company=="mercedes-benz"]<-"mercedes"

## MAZDA has been wrongly spelled as maxda so we replace them.

carmileage$Car_Company[carmileage$Car_Company=="maxda"]<-"mazda"

## Now we use Model.matrix to convert the Categorical variable Car_Company
## into a numeric variables with 1 or 0 for all the factor levels

Carcomp_numeric <- data.frame(model.matrix(~Car_Company, data = carmileage))[,-1]


## we convert the Discrete variable cyinders into Numeric by binning

cylinderbins<- data.frame(cut(carmileage$Cylinders, breaks = 3))

# Change the column to cylinders

names(cylinderbins) <- "cylinders"

## create the dummy variable for cylinders after binning

cylinders_dummy <- data.frame(model.matrix(~cylinders, data = cylinderbins))

#removing the first column intercept is not required

cylinders_dummy <- data.frame(model.matrix(~cylinders, data = cylinderbins))[-1]

## convert the Discrete Variable Origin into numeric 
## dreating dummy variable for Origin variable

# convert the origin variable in to factor

Origin_numeric <- data.frame(model.matrix(~Origin, data = carmileage))[-1]

## we reduce the number of variables by reducing the year of model 

ModelYear_bins<- data.frame(cut(carmileage$Model_year, breaks = 4))

#Change the column name to Years
names(ModelYear_bins) <- "Year"

ModelYear_dummy <- data.frame(model.matrix(~Year, data = ModelYear_bins))

ModelYear_dummy <- data.frame(model.matrix(~Year, data = ModelYear_bins))[-1]

# change the column names to Year 2006:2009 ,"Year:2009-2012","Year:2012-2015"

names(ModelYear_dummy) <- c("Year:2006-2009","Year:2009-2012","Year:2012-2015")

## we bind the Numeric variables created from categorical variables and remove the 
## categorical variables to proceed with model creation.

carmileage <- carmileage[,-c(2,7:10)]

### create a new variable carmileage1 and add all the dummy variables and numeric variables to it

carmileage1 <- cbind(carmileage, Carcomp_numeric,Origin_numeric,cylinders_dummy,ModelYear_dummy)

##training and test data sets 

set.seed(100)

# Split the dataset in to 70:30 

indices= sample(1:nrow(carmileage1), 0.7*nrow(carmileage1))

train = carmileage1[indices,]
test = carmileage1[-indices,]

####### Model building #######

model_1 <- lm(MPG~.,data=train)
summary(model_1)

library("MASS")
library("car")

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")

step

## model 2 

model_2 <- lm(formula = MPG ~ Horsepower + Weight + Acceleration + Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companynissan + Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_2)

vif(model_2)

## we remove Acceleration as it has vif >2 and high P value compared to other variables
## which have higher VIF so since it is not so significant we remove it.

model_3 <- lm(formula = MPG ~ Horsepower + Weight + Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companynissan + Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_3)

vif(model_3)


## we remove Horse power as it has some what larger p value compared to weight.
## we also check the correlation between Horsepower and Weight as they seem to be 
## Collinear

cor(carmileage1$Horsepower, carmileage1$Weight)

## [1] 0.8831895

## They seem to be be highly collinear so we remove Horsepower as it has high p value 
## compared to weight.

model_4 <- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companynissan + Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_4)

vif(model_4)


## now upon checking the VIF values of all variables we find that they are as per
## our business rule (less than 2) but since our model contains more variables than
## needed ( 5 as per our rule) we use P value to reduce them.

summary(model_4)

## we remove Car_Companynissan

model_5 <- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_5)

## we remove Car_Companymercury

model_6<- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda +  
                Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_6)

## we remove Car_Companyford

model_7<- lm(formula = MPG ~ Weight +Car_Companydatsun + 
               Car_Companyhonda +  Car_Companyplymouth + Car_Companypontiac + 
               Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
               cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
               `Year:2012-2015`, data = train)

summary(model_7)

## we remove Car_Companytriumph



model_8<- lm(formula = MPG ~ Weight +Car_Companydatsun + 
               Car_Companyhonda +  Car_Companyplymouth + Car_Companypontiac + 
               Car_Companytoyota + Car_Companyvolkswagen + 
               cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
               `Year:2012-2015`, data = train)

summary(model_8)


## we remove Car_Companytoyota



model_9<- lm(formula = MPG ~ Weight +Car_Companydatsun + 
               Car_Companyhonda +  Car_Companyplymouth + Car_Companypontiac + 
               Car_Companyvolkswagen + 
               cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
               `Year:2012-2015`, data = train)

summary(model_9)


## we remove Car_Companyhonda


model_10<- lm(formula = MPG ~ Weight +Car_Companydatsun + 
               Car_Companyplymouth + Car_Companypontiac + 
               Car_Companyvolkswagen + 
               cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
               `Year:2012-2015`, data = train)

summary(model_10)

## we remove Car_Companypontiac

model_11<- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyplymouth + 
                Car_Companyvolkswagen + 
                cylinders.4.67.6.33. + `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_11)

## we remove Car_Companyplymouth

model_12<- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyvolkswagen + cylinders.4.67.6.33. + 
                `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_12)

## we remove Car_Companydatsun

model_13<- lm(formula = MPG ~ Weight  + 
                Car_Companyvolkswagen + cylinders.4.67.6.33. + 
                `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_13)

## we remove Car_Companyvolkswagen



model_14<- lm(formula = MPG ~ Weight  + 
                 cylinders.4.67.6.33. + 
                `Year:2006-2009` + `Year:2009-2012` + 
                `Year:2012-2015`, data = train)

summary(model_14)

vif(model_14)

## Model contains 5 variables namely Weight ,
#cylinders between(4.67 and 6.33) which is 5 and 6  and the model year 
#These five variables influence the model and predict the best future outcomes
#and helps to take decisions


## Model Evaluation and Testing

## predict using test data set

Predict_1 <- predict(model_14,test)

# store the predicted values in the column predictedMPG

test$predictedMPG <- Predict_1

# correlation between actual and predicted values

cor(test$MPG,test$predictedMPG)

# [1] 0.9145243

## correlation shows 91 % between actual and predicted values

## R -Squared Value

cor(test$MPG,test$testMPG)^2

##  0.8363546

## Model can be Accepted

## The model has only 5 variables.
## The model is highly predictive in nature i.e it shows 83.63% (R squared) of accuracy.


# plot the moel
plot(model_14)




