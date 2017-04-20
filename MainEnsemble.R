# clear memory
rm(list=ls())
# Garbage Collect
gc()


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
curr_method1 <- "lda"
nr_classifiers1 <- 1
predict_col1 <- 10
err <- ensemble(my_formula1, predict_col1, curr_data1, curr_method1, nr_classifiers1)


# Unit test for Computers data
my_formula2 <- cd ~.
predict_col2 <- 6
curr_data2 <- Computers[,-7]
curr_method2 <- "lda"
nr_classifiers2 <- 10
err <- ensemble(my_formula2, predict_col2,curr_data2,curr_method2,nr_classifiers2)

## Main for plots

# Compute the error B times
B <- 50
ERRMAT1 <- matrix(0,B,6)
ERRMAT2 <- matrix(0,B,6)

for (b in (1:B)) {
  print("iteration: ")
  print(b)
  
  # CART
  ERRMAT1[b,1] <- ensemble(my_formula1, predict_col1, curr_data1, "rpart", nr_classifiers1)
  ERRMAT2[b,1] <- ensemble(my_formula2, predict_col2,curr_data2,"rpart",nr_classifiers2)
  
  # KNN 
  
  ERRMAT1[b,2] <- ensemble(my_formula1, predict_col1, curr_data1, "knn", nr_classifiers1)
  ERRMAT2[b,2] <- ensemble(my_formula2, predict_col2,curr_data2,"knn",nr_classifiers2)
  
  # LDA 

  ERRMAT1[b,3] <- ensemble(my_formula1, predict_col1, curr_data1, "lda", nr_classifiers1)
  ERRMAT2[b,3] <- ensemble(my_formula2, predict_col2,curr_data2,"lda",nr_classifiers2)
  
  # QDA
  
  ERRMAT1[b,4] <- ensemble(my_formula1, predict_col1, curr_data1, "qda", nr_classifiers1)
  ERRMAT2[b,4] <- ensemble(my_formula2, predict_col2,curr_data2,"qda",nr_classifiers2)
  
  # PDA
  
  ERRMAT1[b,5] <- ensemble(my_formula1, predict_col1, curr_data1, "pda", nr_classifiers1)
  ERRMAT2[b,5] <- ensemble(my_formula2, predict_col2,curr_data2,"pda",nr_classifiers2)
  
  # NB - Naive bayes 
  
  ERRMAT1[b,6] <- ensemble(my_formula1, predict_col1, curr_data1, "nb", nr_classifiers1)
  ERRMAT2[b,6] <- ensemble(my_formula2, predict_col2,curr_data2,"nb",nr_classifiers2)
  
}

# jpeg('/path/to/project/mve440_mini2/figures/BudgetUK-ENSEMBLE.png')
jpeg('/chalmers/users/reinthal/Master/repos/mve440_mini2/figures/BudgetUK-ENSEMBLE.jpeg')

bp  <-  boxplot(ERRMAT1,
                ylab="Validation Error",
                col = 2:7,
                names=c("CART","knn","lda","qda","pda","nb"),
                main=c("BudgetUK"))
dev.off()


# Same here - add the relevant path
jpeg('/chalmers/users/reinthal/Master/repos/mve440_mini2/figures/Computers-ENSEMBLE.jpg')
bp  <-  boxplot(ERRMAT2,
                ylab="Validation Error ",
                col = 2:7,
                names=c("CART","knn","lda","qda","pda","nb"), 
                main=c("Computers"))

 def.off()
