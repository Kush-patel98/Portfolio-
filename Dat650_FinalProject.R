install.packages("plotly")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggiraph")

library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggiraph)

EmployeeAttrition <- read.csv("/Users/Kush/Desktop/EmployeeAttrition_Data.csv")
View(EmployeeAttrition)

#Modded employee attrition data set to only show employee attrition (NOs)
Mod <- filter(EmployeeAttrition, Attrition == "No")
View(Mod)
summary(Mod)

#drop NA values
Mod <- Mod %>%
  drop_na()

#Separate data set for needed to look at character values are numerical values
Mod_2 <- Mod

#Replace values in department column with numbers 
Mod_2$Department[Mod_2$Department == 'Sales'] <- '3'
Mod_2$Department[Mod_2$Department == "Research & Development"] <- '2'
Mod_2$Department[Mod_2$Department == "Human Resources"] <- '1'

#convert from character to numeric value
Mod_2$Department <- as.numeric(as.character(Mod_2$Department))

#----------------------------------------------Descriptive Analytics ---------------------------------------------------------
#of num of companies worked at
ggbar <- 
  ggplot(Mod) + 
  geom_bar(aes(x = NumCompaniesWorked))
ggplotly(ggbar)

#Check for the frequency of age 
ggplot(Mod, aes (x = Age)) +
  geom_histogram(binwidth = 2, fill = 'gray', color = 'black', alpha = 1) +
  labs(title = 'Age of Attrition Employees', x = 'Age', y = 'Frequency')

#interactive scatter plot graph depicting the age and Job roles as well as department based of those who left
#Scatter plot of Job roles with ages 
Roles <-
  ggplot(Mod) +
  geom_point(aes(x = Age,
             y = YearsAtCompany,
             color = JobRole,             
             shape = Department))
ggplotly(Roles)

#Scatter plot of department with ages
plot_ly(Mod,
        x = ~Age,
        y = ~YearsAtCompany,
        color = ~Department,
        symbol = ~Department,
        mode = "markers",
        type = "scatter")

#Scatter plot 
Satisfaction <-
  ggplot(Mod) +
  geom_point(aes(x = YearsInCurrentRole,
                 y = YearsSinceLastPromotion,
                 color = Gender,
                 shape = MaritalStatus))
ggplotly(Satisfaction)

#line graph of the pay structure among different departments via distance from home
Pay <- 
  ggplot(Mod, aes(x = JobRole, y = MonthlyIncome)) + 
  geom_boxplot(outlier.color = 'red')

Pay

#boxplot for years since last promotion
promotion <-
  ggplot(Mod, aes(x = JobRole, y = YearsSinceLastPromotion)) + 
  geom_boxplot(outlier.color = 'blue')

promotion

#bar graph differentiating jobs and gender
bar <- ggplot(Mod, aes(JobRole, fill = Gender)) +
  geom_bar() +
  coord_flip()
  
ggplotly(bar)

#work life balance of different job roles
worklifebalance <- ggplot(Mod, aes(WorkLifeBalance, fill = JobRole)) + 
  geom_bar() +
  coord_flip()

ggplotly(worklifebalance)


#--------------------------------------------------Predictive Analytics-----------------------------------------------------

#decision trees
install.packages("tree")
library(tree)

library(rpart)
library(rpart.plot)

#decision tree model for age vs salary
tree1 <- rpart(Age ~ YearsAtCompany + MonthlyIncome, data = Mod, JobLevel)
rpart.plot(tree1, type = 4, extra = 101, fallen.leaves = TRUE, cex = 0.9)

tree2 <- rpart(Gender ~ MonthlyIncome + PercentSalaryHike, data = Mod, JobLevel)
rpart.plot(tree2, type = 4, extra = 101, fallen.leaves = TRUE, cex = 0.9)

