library(Ecdat)
library(scatterplot3d)
data(BudgetUK, package = "Ecdat")

######### Plot the data ###################
#### To get a good overview ###############
###########################################

# Create partial plots of data in BudgetUK
allPairs <- pairs(~wfood+wfuel+wcloth+walc+wtrans+wother+totexp+income+
                    age+children,data=BudgetUK,main="BudgetUK")
