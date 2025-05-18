#Packages
install.packages("tidyverse")
install.packages("MASS")
install.packages("car")
install.packages("Metrics")

#Libraries
library(readr)
library(dplyr)
library(tidyverse)
library(MASS)
library(car)
library(Metrics)

#Testing data set will be names EmployeeSalary and Training data will be VerifyData
#Read in File 
EmployeeSalary <- read.csv(file.choose())
View(EmployeeSalary)
str(EmployeeSalary)

#clean data omit any missing values
EmployeeSalary <- na.omit(EmployeeSalary)

#Get rid of unnecessary characters 
EmployeeSalary$AnnualIncomeNeeded <- gsub(",", "", EmployeeSalary$AnnualIncomeNeeded)
EmployeeSalary$DiffFromSalary <- gsub(",", "", EmployeeSalary$DiffFromSalary)
EmployeeSalary$CurrentSalary <- gsub(",", "", EmployeeSalary$CurrentSalary)

#Convert characters values to numerical values
EmployeeSalary$AnnualIncomeNeeded <- as.numeric(as.character(EmployeeSalary$AnnualIncomeNeeded))
EmployeeSalary$DiffFromSalary <- as.numeric(as.character(EmployeeSalary$DiffFromSalary))
EmployeeSalary$CurrentSalary <- as.numeric(as.character(EmployeeSalary$CurrentSalary))

#Making numerical columns to categorical only for ones that have #1-5 
EmployeeSalary$Education <- as.factor(EmployeeSalary$Education)
EmployeeSalary$EnvironmentSatisfaction <- as.factor(EmployeeSalary$EnvironmentSatisfaction)
EmployeeSalary$JobInvolvement <- as.factor(EmployeeSalary$JobInvolvement)
EmployeeSalary$JobLevel <- as.factor(EmployeeSalary$JobLevel)
EmployeeSalary$JobSatisfaction <- as.factor(EmployeeSalary$JobSatisfaction)
EmployeeSalary$PerformanceRating <- as.factor(EmployeeSalary$PerformanceRating)
EmployeeSalary$RelationshipSatisfaction <- as.factor(EmployeeSalary$RelationshipSatisfaction)
EmployeeSalary$WorkLifeBalance <- as.factor(EmployeeSalary$WorkLifeBalance)
  
#summary of employee salary
summary(EmployeeSalary)

#testing correlation of variables (variables must be in numerical format)
cor(EmployeeSalary)

#---------------------------Verification Data-----------------------------------
VerifyData <- read.csv(file.choose())
View(VerifyData)
str(VerifyData)

#Create same conditions as training data set (EmployeeSalary)
#Clean data for missing values
VerifyData <- na.omit(VerifyData)

#Get rid of unnecessary characters for conversion
VerifyData$AnnualIncomeNeeded <- gsub(",", "", VerifyData$AnnualIncomeNeeded)
VerifyData$DiffFromSalary <- gsub(",", "", VerifyData$DiffFromSalary)
VerifyData$CurrentSalary <- gsub(",", "", VerifyData$CurrentSalary)

#convert data to numerical values
VerifyData$AnnualIncomeNeeded <- as.numeric(as.character(VerifyData$AnnualIncomeNeeded))
VerifyData$DiffFromSalary <- as.numeric(as.character(VerifyData$DiffFromSalary))
VerifyData$CurrentSalary <- as.numeric(as.character(VerifyData$CurrentSalary)) 

#Convert numerical columns to categorical by factor for columns rating #1-5
VerifyData$Education <- as.factor(VerifyData$Education)
VerifyData$EnvironmentSatisfaction <- as.factor(VerifyData$EnvironmentSatisfaction)
VerifyData$JobInvolvement <- as.factor(VerifyData$JobInvolvement)
VerifyData$JobLevel <- as.factor(VerifyData$JobLevel)
VerifyData$JobSatisfaction <- as.factor(VerifyData$JobSatisfaction)
VerifyData$PerformanceRating <- as.factor(VerifyData$PerformanceRating)
VerifyData$RelationshipSatisfaction <- as.factor(VerifyData$RelationshipSatisfaction)
VerifyData$WorkLifeBalance <- as.factor(VerifyData$WorkLifeBalance)

#------------------------------Modeling-----------------------------------------

#linear regression model with all variables
lmModel <- lm(AnnualIncomeNeeded ~ Age + DistanceFromHome + Education + EnvironmentSatisfaction +
                     JobInvolvement + JobLevel + JobSatisfaction + NumCompaniesWorked + AvgOverTime + PercentSalaryHike +
                     PerformanceRating + RelationshipSatisfaction + StockOption + TotalWorkingYears + TrainingTimesLastYear +
                     WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + DiffFromSalary
                   , data = EmployeeSalary)
summary(lmModel)

#Running StepAIC model with lmModel for better model fitting used to create perfect model
StepModel <- stepAIC(lmModel, direction = 'both')
summary(StepModel)

#Model
true_model <- lm(AnnualIncomeNeeded ~ Age + PercentSalaryHike + WorkLifeBalance + DiffFromSalary, data = EmployeeSalary)
summary(true_model)

#validate training data (Employee Salary)
training_predictions <- predict(true_model,EmployeeSalary)

#Add predictions as column to dataset
EmployeeSalary['Predictions'] <- training_predictions
view(EmployeeSalary)

#validate model with the test data (VerifyData)
predictModel <- predict(true_model,VerifyData)
predictModel

#Add predictions as column to dataset
VerifyData["Predictions"] <- predictModel
View(VerifyData)

#-----------------------Baselines and Model Results-----------------------------

#Calculating R squared for training and test data
Training_R2 <- summary(true_model)$r.squared
Testing_R2 <- 1 - sum((VerifyData$AnnualIncomeNeeded - VerifyData$`Predictions`)^2) / sum((VerifyData$AnnualIncomeNeeded - mean(VerifyData$AnnualIncomeNeeded))^2)

#Calculating MEAN Squared Error for training and test data
train_mse <- mean((EmployeeSalary$AnnualIncomeNeeded - EmployeeSalary$Predictions)^2)
test_mse <- mean((VerifyData$AnnualIncomeNeeded - VerifyData$`Predictions`)^2)

#Calculating Root Mean Squared Error for training and test data
train_rmse <- sqrt(train_mse)
test_rmse <- sqrt(test_mse)

cat(" Training Data (EmployeeSalary)", '\n',"R-Squared:", Training_R2, '\n','MSE:', train_mse, '\n', "Testing RMSE:", train_rmse, '\n')

cat(" Testing Data (VerifyData)", '\n', "R-Sqaured:", Testing_R2, '\n', 'MSE:', test_mse, '\n', 'Training RSME:', test_rmse)

#plot training model
plot(EmployeeSalary$AnnualIncomeNeeded, EmployeeSalary$Predictions, xlab = 'Annual Salary Needed', ylab = 'Predicited Salary')
abline(lm(EmployeeSalary$AnnualIncomeNeeded~EmployeeSalary$Predictions))

#plot testing model
plot(VerifyData$AnnualIncomeNeeded, VerifyData$Predictions, xlab = 'Annual Salary Needed', ylab = 'Predicited Salary')
abline(lm(VerifyData$AnnualIncomeNeeded~VerifyData$Predictions))



