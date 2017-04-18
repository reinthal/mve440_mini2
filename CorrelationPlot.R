
# clear memory
rm(list=ls())
# Garbage Collect
gc()

library(Ecdat)
library(corrplot)
data(BudgetUK, package = "Ecdat")
data(Computers,package = "Ecdat")


# Convert class to numerical for the Computer data
for (b in (6:8)) {
  newvec <- rep(0,dim(Computers)[1])
  newvec[Computers[,b] == "yes"] <- 1
  Computers[,b] <- newvec
}


M1 <- cor(BudgetUK)
M2 <- cor(Computers)
p1 <- corrplot.mixed(M1, lower="ellipse", upper="circle")
p2 <- corrplot.mixed(M2, lower="ellipse", upper="circle")
