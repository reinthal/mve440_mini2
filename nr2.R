library(Ecdat)
data(Computers, package = 'Ecdat')
data(Airline, package='Ecdat')
data(BudgetUK,package='Ecdat')
data(BudgetItaly,package='Ecdat')

allPairs <- pairs(~.,data=Computers,main="Simple Scatterplot Matrix")
plot(allPairs)
