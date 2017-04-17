installset.seed(10)

library(Ecdat)
library(class)
library(MASS)
library(scatterplot3d)
library(klaR)
library(caret)
library(pROC)
library(e1071)
library(ranger)

data(BudgetUK, package = "Ecdat")
data("iris3")

######### Plot the data ###################
#### To get a good overview ###############
###########################################

# Create partial plots of data in BudgetUK
allPairs <- pairs(~wfood+wfuel+wcloth+walc+wtrans+wother+totexp+income+
                    age+children,data=BudgetUK,main="BudgetUK")

#train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
#cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#knn(train, test, cl, k = 3, prob=TRUE)
#attributes(.Last.value)

################ KNN #################
# Below is a tutorial on kNN
######################################

BudgetUK$age <- factor(BudgetUK$age)
num.vars <- sapply(BudgetUK, is.numeric)
BudgetUK[num.vars] <- lapply(BudgetUK[num.vars, scale])
myvars <- c("wfood", "wfuel", "children")
BudgetUK.subset <- BudgetUK[myvars]

summary(BudgetUK.subset)

test <- 1:500
test.BudgetUK <- BudgetUK[test,]
train.BudgetUK <- BudgetUK[-test,]

# Create training and test sets
test.tar <- BudgetUK$children[test]
train.tar <- BudgetUK$children[-test]

knn.1 <- knn(train.BudgetUK, test.BudgetUK, train.tar, k=1, prob = TRUE)
knn.5 <- knn(train.BudgetUK, test.BudgetUK, train.tar, k=5, prob = TRUE)
knn.20 <- knn(train.BudgetUK, test.BudgetUK, train.tar, k=20, prob = TRUE)

sum(test.tar == knn.1)/length(test)
sum(test.tar == knn.5)/length(test)
sum(test.tar == knn.20)/length(test)

table(knn.1, test.tar)


##########################################
########### Logistic Regression ##########
##########################################

reg = glm(children~.,data=BudgetUK,family=gaussian)
summary(reg)

##########################################
#### Linear Discriminant Analysis ########
##########################################

fit <- lda(children ~., data=BudgetUK,na.action="na.omit", CV=TRUE)
fit # show results

# Assess the accuracy of the prediction
# percent correct for each category of children
ct <- table(BudgetUK$children, fit$class)

# sensitivity / specificity table
prop.table(ct, 1)

# total percent correct
sum(diag(prop.table(ct)))

##########################################
#### Quadratic Discriminant Analysis #####
##########################################

fit2 <- qda(children ~., data=BudgetUK,CV=TRUE)
fit2

####  Boxplot for methods to compare ####
#########################################

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


# Compute the error B times
B<-15
ERRMAT<-matrix(0,B,8)

ctrl<-trainControl(method="cv",summaryFunction=multiClassSummary)

for (b in (1:B)) {
  
  inTrain <- createDataPartition(BudgetUK$children,p=3/4,list=FALSE)
  trainW <- BudgetUK[inTrain,-10]
  testW <- BudgetUK[-inTrain,-10]
  trainwY <- BudgetUK$children[inTrain]
  testwY <- BudgetUK$children[-inTrain]
  
  fit<-train(children~.,data=BudgetUK[inTrain,],method="rpart",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,1]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="ranger",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,2]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="knn",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,3]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="lda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,4]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="qda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,5]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="pda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,6]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="nb",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,7]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  fit<-train(children~.,data=BudgetUK[inTrain,],method="mda",tuneLength=15,trControl=ctrl)
  pp<-predict(fit,newdata=BudgetUK[-inTrain,-10],type="raw")
  
  ERRMAT[b,8]<-length(pp[pp!=BudgetUK$children[-inTrain]])/length(pp)
  print(b)
}
bp <- boxplot(ERRMAT,names=c("CART","RF","knn","lda","qda","pda","nb","mda"))





