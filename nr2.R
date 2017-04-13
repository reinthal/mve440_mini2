installset.seed(10)

library(Ecdat)
library(class)
library(MASS)

data(BudgetUK, package = "Ecdat")
data("iris3")

######### Plot the data ###################
#### To get a good overview ###############
###########################################

# Create partial plots of data in BudgetUK
allPairs <- pairs(~wfood+wfuel+wcloth+walc+wtrans+wother+totexp+income+
                    age+children,data=BudgetUK,main="BudgetUK")

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
knn(train, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)

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

test.tar <- BudgetUK$children[test]
train.tar <- BudgetUK$children[-test]

knn.1 <- knn(train.BudgetUK, test.BudgetUK, train.tar, k=1)
knn.5 <- knn(train.BudgetUK, test.BudgetUK, train.tar, k=5)
knn.20 <- knn(train.BudgetUK, test.BudgetUK, train.tar, k=20)

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

fit2 <- qda(children ~ ., data=BudgetUK,CV=TRUE)
fit2





