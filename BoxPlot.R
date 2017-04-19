# clear memory
rm(list=ls())
# Garbage Collect
gc()


########## Libraries


library(Ecdat)
library(rpart)
library(class)
library(MASS)
library(scatterplot3d)
library(klaR)
library(caret)
library(pROC)
library(e1071)
library(ranger)

############# Main

####  Boxplot for methods to compare ####
#########################################

# Data we want to examine
data(BudgetUK, package = "Ecdat")
data(Computers, package = "Ecdat")



########## Start with BudgetUK

# Convert numerical to class
newfac  <-  rep(0,dim(BudgetUK)[1])
newfac[BudgetUK$children == 1]  <- "One"
newfac[BudgetUK$children == 2]  <- "Two"
newfac  <-  as.factor(newfac)
BudgetUK$children  <-  newfac


# create data partitions for training and testing
inTrain  <-  createDataPartition( BudgetUK$children , p=3/4, list=FALSE)

# Compute the error B times
B  <-  1
ERRMAT  <-  matrix(0,B,8)

# set the validation method to cross validation
ctrl  <-  trainControl(method="cv",summaryFunction=multiClassSummary)



# Loop takes 1 hour for BudgetUK
for (b in (1:B)) {
  
  inTrain  <-  createDataPartition(BudgetUK$children,p=3/4,list=FALSE)
  trainW  <-  BudgetUK[inTrain,-10]
  testW  <-  BudgetUK[-inTrain,-10]
  trainwY  <-  BudgetUK$children[inTrain]
  testwY  <-  BudgetUK$children[-inTrain]
  
  
  # CART
  fit <- train(children~.,data=BudgetUK[inTrain,],method="rpart",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  ERRMAT[b,1] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  # Random Forest
  
  fit<-train(children~.,data=BudgetUK[inTrain,],method="ranger",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw") 
  ERRMAT[b,2] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  # KNN
  
  fit <- train(children~.,data=BudgetUK[inTrain,],method="knn",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  ERRMAT[b,3] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  # LDA
  
  fit <- train(children~.,data=BudgetUK[inTrain,],method="lda",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  ERRMAT[b,4] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  # QDA
  
  fit <- train(children~.,data=BudgetUK[inTrain,],method="qda",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  ERRMAT[b,5] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  # PDA
  
  fit <- train(children~.,data=BudgetUK[inTrain,],method="pda",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  ERRMAT[b,6] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  # NB - Negative Binomial
  
  fit <- train(children~.,data=BudgetUK[inTrain,],method="nb",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  ERRMAT[b,7] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  # MDA - mixture da
  
  fit <- train(children~.,data=BudgetUK[inTrain,],method="mda",tuneLength=15,trControl=ctrl)
  pp <- predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  ERRMAT[b,8] <- length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  
  print(b)
}



bp <- boxplot(ERRMAT,
              ylab="Cross Validation Error",
              col = 2:8,
              names=c("CART","RF","knn","lda","qda","pda","nb","mda"))

########## Continue with Computers

# Convert to factor
Computers$cd <- as.factor(Computers$cd)

# create data partitions for training and testing
inTrain  <-  createDataPartition( Computers$cd , p=3/4, list=FALSE)

# Compute the error B times
B <- 50
ERRMAT <- matrix(0,B,8)

# set the validation method to cross validation
ctrl <- trainControl(method="cv",summaryFunction=multiClassSummary)


for (b in (1:B)) {
  
  inTrain  <-  createDataPartition(Computers$cd,p=3/4,list=FALSE)
  trainW  <-  Computers[inTrain,-6]
  testW  <-  Computers[-inTrain,-6]
  trainwY  <-  Computers$cd[inTrain]
  testwY  <-  Computers$cd[-inTrain]
  
  # CART
  fit <- train(cd~.,data=Computers[inTrain,],method="rpart",trControl=ctrl)
  pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw")
  ERRMAT[b,1] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  # Random Forest
  fit <- train(cd~.,data=Computers[inTrain,],method="ranger",trControl=ctrl)
  pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw") 
  ERRMAT[b,2] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  # KNN
  
  fit <- train(cd~.,data=Computers[inTrain,],method="knn",trControl=ctrl)
  pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw")
  ERRMAT[b,3] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  # LDA
  
  fit <- train(cd~.,data=Computers[inTrain,],method="lda",trControl=ctrl)
  pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw")
  ERRMAT[b,4] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  # QDA, does not work for some reason. Skip
 
  #fit <- train(cd~.,data=Computers[inTrain,],method="qda",trControl=ctrl)
  #pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw")
  #ERRMAT[b,5] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  # PDA
  
  fit <- train(cd~.,data=Computers[inTrain,],method="pda",trControl=ctrl)
  pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw")
  ERRMAT[b,6] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  # NB - Negative Binomial, does not work for some reason. Skip
  
  #fit <- train(cd~.,data=Computers[inTrain,],method="nb",tuneLength=15,trControl=ctrl)
  #pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw")
  #ERRMAT[b,7] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  # MDA - mixture da
 
  fit <- train(cd~.,data=Computers[inTrain,],method="mda",trControl=ctrl)
  pp <- predict(fit,newdata=Computers[-inTrain,-6],type="raw")
  ERRMAT[b,8] <- length(pp[pp!=Computers$cd[-inTrain]])/length(pp)
  
  print(b)
}



bp  <-  boxplot(ERRMAT,
              ylab="Cross Validation Error",
              col = 2:8,
              names=c("CART","RF","knn","lda","qda","pda","nb","mda"))
#hejhej


