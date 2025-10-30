#Project determines whether or not charges to credit card are fraud based on multiple factors
#Will use Logistic model to determine outcomes

#Packages 
install.packages('tidyverse')
install.packages('tidymodels')
install.packages('plotly')
install.packages('dplyr')
install.packages('caTools')
install.packages('pROC')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('neuralnet')

#Libraries 
library(readr)
library(dplyr)
library(plotly)
library(data.table)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(neuralnet)

#Create the dataframe and calling in dataset
CreditCF <- read_csv(file.choose())
View(CreditCF)
summary(CreditCF)

table(CreditCF$Class)

#Exploration 
data.table(CreditCF)

#Data Cleaning (Conversions, mutations, missing values)
CreditCF <- na.omit(CreditCF)

str(CreditCF)
CreditCF$Class <- as.factor(CreditCF$Class)

CreditCF$Amount <- scale(CreditCF$Amount) #normalization 

CreditCF <- CreditCF [,-c(1)]
head(CreditCF)

#Creating separate training and testing dataframes
set.seed(100)
sample_data <- sample.split(CreditCF$Class, SplitRatio = 0.80)

train_data <- subset(CreditCF, sample_data == TRUE)
test_data <- subset(CreditCF, sample_data == FALSE)

dim(train_data)
dim(test_data)

#Data Modeling (Logistic Regression)

Logistic_model <- glm(Class~., test_data, family = binomial())
summary(Logistic_model)

plot(Logistic_model)

Logistic_model2 <- glm(Class~., train_data, family = binomial())
summary(Logistic_model2)

plot(Logistic_model2)

#Testing ROC
lr.predict <- predict(Logistic_model,test_data, probability = TRUE)
auc.gb <- roc(test_data$Class, lr.predict, plot = TRUE, col = 'blue')

lr.predict2 <- predict(Logisitc_model2, train_data, probability = TRUE)
auc.gb2 <- roc(train_data$Class, lr.predict2, plot = TRUE, col = 'red')

#Decision Tree
deci_tree <- rpart(Class~. , CreditCF, method = 'class') 
predicted_values <- predict(deci_tree, CreditCF, type = 'class')
probability <- predict(deci_tree, CreditCF, type = 'prob')
rpart.plot(deci_tree)

#Neuralnet
Nn_model <- neuralnet::neuralnet(Class~., train_data, linear.output = FALSE)
plot(Nn_model)

predNn <- compute(Nn_model, test_data)
resultNn <- predict$net.result
resultNm <- ifelse(resultNn>0.6,1,0)