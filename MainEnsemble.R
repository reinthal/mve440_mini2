# clear memory
rm(list=ls())
# Garbage Collect
gc()


library(Ecdat)
library(tictoc)
library(rpart)
library(class)
library(MASS)
library(scatterplot3d)
library(klaR)
library(caret)
library(pROC)
library(e1071)
library(ranger)


# How to run this file:
# You gotta import the Ensemble.R file first
# 1. Either run the code with cmd + enter
# 2. Or run source(/path/to/file/Ensemble.R)


# This file generates the boxplots for a cross validation error
# from a list of classifiers running as an ensemble

# Classifiers:
# -----------
# CART
# knn
# lda
# qda
# pda
# nb
# mda




# Data we want to examine
data(BudgetUK, package = "Ecdat")
data(Computers, package = "Ecdat")

# Convert numerical to class
newfac  <-  rep(0,dim(BudgetUK)[1])
newfac[BudgetUK$children == 1]  <- "One"
newfac[BudgetUK$children == 2]  <- "Two"
newfac  <-  as.factor(newfac)
BudgetUK$children  <-  newfac


# Unit test for  BudgetUK data
my_formula1 <- children ~ .
curr_data1 <- BudgetUK
curr_method1 <- "rpart"
nr_classifiers1 <- 100
predict_col1 <- 10
tic(); err <- ensemble(my_formula1, predict_col1, curr_data1, curr_method1, nr_classifiers1); toc();


# Unit test for Computers data
my_formula2 <- cd ~.
predict_col2 <- 6
curr_data2 <- Computers[,-7]
curr_method2 <- "lda"
nr_classifiers2 <- 10
err <- ensemble(my_formula2, predict_col2,curr_data2,curr_method2,nr_classifiers2)

## Main for plots

# Compute the error B times
B <- 25

methods <- c("rpart","knn","lda","qda","nb")
ERRMAT1 <- matrix(0,B,length(methods))
ERRMAT2 <- matrix(0,B,length(methods))

for (b in (1:B)) {
  print( paste("iteration: ", b) )
  print("-----------")
  for (method in methods) {
    print(paste('method: ',method))
    ERRMAT1[b,1] <- ensemble(my_formula1, predict_col1, curr_data1, method, nr_classifiers1)
    #ERRMAT2[b,1] <- ensemble(my_formula2, predict_col2, curr_data2, method, nr_classifiers2)
  }  
}


bp  <-  boxplot(ERRMAT1,
                ylab="Validation Error",
                col = 2:(length(methods)-1),
                names=methods,
                main=c("BudgetUK"))


bp  <-  boxplot(ERRMAT2,
                ylab="Validation Error ",
                col = 2:(length(methods)-1),
                names=methods, 
                main=c("Computers"))
