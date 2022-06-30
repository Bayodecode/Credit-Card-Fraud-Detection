#Import the needed library
library(ranger)
library(caret)
library(data.table)
library(readxl)
creditcard_data <- read_excel("/Users/bayodeibironke/Downloads/Credit-card-dataset/creditcard.xlsx")

head(creditcard_data)
tail(creditcard_data)

dim(creditcard_data)

table(creditcard_data$Class)
barplot(table(creditcard_data$Class))
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)

#standard deviation
sd(creditcard_data$Amount)

#Normalization of data
creditcard_data$Amount=scale(creditcard_data$Amount, center = 1, scale = 1)
NewData=creditcard_data[,-c(1)]
head(NewData)

#Data Modelling
#To know how data perform; split into train and test sets
library(caTools)
set.seed(123)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)

#Fitting Logistic Regression Model
Logistic_Model=glm(Class~.,train_data,family=binomial())
summary(Logistic_Model)
plot(Logistic_Model)

