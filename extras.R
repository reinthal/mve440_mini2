# clear memory
rm(list=ls())
# Garbage Collect
gc()

library(Ecdat)
library(class)
library(MASS)
library(scatterplot3d)
library(klaR)
library(caret)
library(pROC)
library(e1071)
library(ranger)


# Convert numerical to class
newfac <- rep(0,dim(BudgetUK)[1])
newfac[BudgetUK$children == 1] <-"One"
newfac[BudgetUK$children == 2] <-"Two"
newfac <- as.factor(newfac)
BudgetUK$children <- newfac


# create data partitions for training and testing
inTrain <- createDataPartition( BudgetUK$children , p=3/4, list=FALSE)
trainW <- BudgetUK[inTrain,-10]
testW <- BudgetUK[-inTrain,-10]
trainwY <- BudgetUK$children[inTrain]
testwY <- BudgetUK$children[-inTrain]

# set the validation method to repeated cross validation for 
ctrl <- trainControl(method="repeatedcv",repeats=10,summaryFunction=multiClassSummary)

# Create a classifier using Classification and Regression Tree (CART)
rpartfit <- train(children ~. ,data=BudgetUK[inTrain,],method="rpart",tuneLength=15,trControl=ctrl)
plot(rpartfit)

# Create a classifier using k nearest neighbours
knnfit<-train(children~.,data=BudgetUK[inTrain,],method="knn",tuneLength=15,trControl=ctrl)
plot(knnfit)
knnfit$finalModel


pdafit<-train(children~.,data=BudgetUK[inTrain,],method="pda",tuneLength=15,trControl=ctrl)
plot(pdafit)

# predict without the column children (column 10)

pp<-predict(rpartfit, newdata=BudgetUK[-inTrain,-10],type="raw")
table(pp,BudgetUK$children[-inTrain])

# Whats the error of this model?
length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)

