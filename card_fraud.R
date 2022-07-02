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


#Assess the performance of our model
library(pROC)
lr.predict <- predict(Logistic_Model,test_data, probability = TRUE)
auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")
fitted(Logistic_Model)
#To get the numeric value or performance metric for the model
#to get the probability value, include type = response
lr.predict67 <- predict(Logistic_Model,test_data, probability = TRUE, type = "response")
#classify
lr.predict67.classify <- ifelse(lr.predict67>0.5, 1, 0)
#to obtain the confusion matrix - which is to compare the values of the model and the actual value
LRM_convb <- table(lr.predict67.classify, test_data$Class)
class(lr.predict67)
class(test_data$Class)
library(caret)
sensitivity(LRM_convb)
specificity(LRM_convb)

#Artificial Neural Network: machine learning algorithm that are modeled after the human nervous system.
#The ANN models are able to learn the patterns using the historical data and are able to perform classification on the input data.
#?? >To estimate model
library(neuralnet)
set.seed(23)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)

#prediction
#To validate the model
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
#classifying using a threshold
resultANN=ifelse(resultANN>0.5,1,0)
tail(resultANN, 200)

#Getting performance metrics
#Confusion Matrix or 2 by 2 contingency table - comparing the output of a model against the actual model
ANN_conf<- table(resultANN, test_data$Class)
library(caret)
sensitivity(ANN_conf)
specificity(ANN_conf)
class(resultANN)
