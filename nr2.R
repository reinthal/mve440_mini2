library(Ecdat)
data(Computers, package = 'Ecdat')
data(BudgetUK,package='Ecdat')


allPairs2 <- pairs(~.,data=BudgetUK,main="Simple Scatterplot Matrix")
plot(allPairs)
allPairs1 <- pairs(~.,data=Computers,main="Simple Scatterplot Matrix")
plot(allPairs)
