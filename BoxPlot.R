# clear memory
rm(list=ls())
# Garbage Collect
gc()

installset.seed(10)

########## Librarys


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
data(Computers,package = "Ecdat")



########## Start with BudgetUK

# Convert numerical to class
newfac <- rep(0,dim(BudgetUK)[1])
newfac[BudgetUK$children == 1] <-"One"
newfac[BudgetUK$children == 2] <-"Two"
newfac <- as.factor(newfac)
BudgetUK$children <- newfac


# create data partitions for training and testing
inTrain <- createDataPartition( BudgetUK$children , p=3/4, list=FALSE)

# Training data
trainW <- BudgetUK[inTrain,-10]
testW <- BudgetUK[-inTrain,-10]

# Training labels
trainwY <- BudgetUK$children[inTrain]
testwY <- BudgetUK$children[-inTrain]

# Compute the error B times
B<-50
ERRMAT<-matrix(0,B,8)

# set the validation method to cross validation
ctrl<-trainControl(method="cv",summaryFunction=multiClassSummary)



# Loop takes 1 hour for BudgetUK
for (b in (1:B)) {
  
  inTrain <- createDataPartition(BudgetUK$children,p=3/4,list=FALSE)
  trainW <- BudgetUK[inTrain,-10]
  testW <- BudgetUK[-inTrain,-10]
  trainwY <- BudgetUK$children[inTrain]
  testwY <- BudgetUK$children[-inTrain]
  
  
  # CART
  fit<-train(children~.,data=BudgetUK[inTrain,],method="rpart",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # Random Forest
  ERRMAT[b,1]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="ranger",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw") 
  
  
  # KNN
  ERRMAT[b,2]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="knn",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # LDA
  ERRMAT[b,3]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="lda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgeyestUK[-inTrain,-10],type="raw")
  
  
  # QDA
  ERRMAT[b,4]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="qda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  # PDA
  ERRMAT[b,5]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="pda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # NB - Negative Binomial
  ERRMAT[b,6]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="nb",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # MDA - mixture da
  ERRMAT[b,7]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="mda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,8]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  print(b)
}



bp <- boxplot(ERRMAT,
              ylab="Cross Validation Error",
              col = 2:8,
              names=c("CART","RF","knn","lda","qda","pda","nb","mda"))

######### B??rja skriva kod h??r
########## Continue with Computers

# Convert numerical to class
newfac <- rep(0,dim(BudgetUK)[1])
newfac[BudgetUK$children == 1] <-"One"
newfac[BudgetUK$children == 2] <-"Two"
newfac <- as.factor(newfac)
BudgetUK$children <- newfac


# create data partitions for training and testing
inTrain <- createDataPartition( BudgetUK$children , p=3/4, list=FALSE)

# Training data
trainW <- BudgetUK[inTrain,-10]
testW <- BudgetUK[-inTrain,-10]

# Training labels
trainwY <- BudgetUK$children[inTrain]
testwY <- BudgetUK$children[-inTrain]

# Compute the error B times
B<-50
ERRMAT<-matrix(0,B,8)

# set the validation method to cross validation
ctrl<-trainControl(method="cv",summaryFunction=multiClassSummary)



# Loop takes 1 hour for BudgetUK
for (b in (1:B)) {
  
  inTrain <- createDataPartition(BudgetUK$children,p=3/4,list=FALSE)
  trainW <- BudgetUK[inTrain,-10]
  testW <- BudgetUK[-inTrain,-10]
  trainwY <- BudgetUK$children[inTrain]
  testwY <- BudgetUK$children[-inTrain]
  
  
  # CART
  fit<-train(children~.,data=BudgetUK[inTrain,],method="rpart",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # Random Forest
  ERRMAT[b,1]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="ranger",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw") 
  
  
  # KNN
  ERRMAT[b,2]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="knn",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # LDA
  ERRMAT[b,3]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="lda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgeyestUK[-inTrain,-10],type="raw")
  
  
  # QDA
  ERRMAT[b,4]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="qda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  # PDA
  ERRMAT[b,5]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="pda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # NB - Negative Binomial
  ERRMAT[b,6]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="nb",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  
  # MDA - mixture da
  ERRMAT[b,7]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="mda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,8]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  print(b)
}



bp <- boxplot(ERRMAT,
              ylab="Cross Validation Error",
              col = 2:8,
              names=c("CART","RF","knn","lda","qda","pda","nb","mda"))



