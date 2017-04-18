# clear memory
rm(list=ls())
# Garbage Collect
gc()

library(Ecdat)
data(BudgetUK, package = "Ecdat")
data(Computers,package = "Ecdat")

######### ScatterPlot of the data ###############
########## To get a good overview ###############
#################################################

# Create partial plots of data in BudgetUK
spUK <- pairs(~., data=BudgetUK, main="BudgetUK")
spComp <- pairs(~., data=Computers, main="Computers")
