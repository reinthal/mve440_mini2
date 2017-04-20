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
# RF
# knn
# lda
# qda
# pda
# nb
# mda



############# Main

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


# Unit test for  BudgetUK data
my_formula <- children ~ .
curr_data <- BudgetUK
curr_method <- "lda"
nr_classifiers <- 5
predict_col <- 10
err <- ensemble(my_formula, predict_col, curr_data, curr_method, nr_classifiers)


# Unit test for Computers data

my_formula <- premium ~.
predict_col <- 8
curr_data <- Computers[,-7]
curr_method <- "lda"
nr_classifiers <- 5
err <- ensemble(my_formula, predict_col,curr_data,curr_method,nr_classifiers)





