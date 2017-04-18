
# The ensemble method function
ensemble <- function(my_formula, curr_data, curr_method, nr_ensembles){
  
  # split the data
  folds <- createFolds(curr_data[,1], k = nr_ensembles)
  
  
  VOTES  <-  matrix(0,2,dim(curr_data)[1])
  
  # train the classifier on each of the splits
  for (fold in folds){
    
    # set the validation method to cross validation
    ctrl <- trainControl(method="cv",summaryFunction=multiClassSummary)
    
    fit <- train( my_formula , 
                  data = curr_data[fold,], 
                  method = curr_method, 
                  tuneLength = 15, 
                  trControl = ctrl)
    
    pp <- predict(fit, 
                  newdata = curr_data[-fold,], # possibly exclude predictor column
                  type = "raw")
    
    # count the votes  
    for (i in 1:length(pp)){
      if (pp[i] == "One"){
        VOTES[1,i] <- VOTES[1,i]+1
      }
      else{ # Two
        VOTES[2,i] <- VOTES[2,i]+1
      }
    }
  }
  
  
  
  return(VOTES)
  
  # create predictions for the validation set
  
  # return the predictions
  
  
}


my_formula <- children ~ .
curr_data <- BudgetUK
curr_method <- "lda"
nr_classifiers <- 5

v1 <- ensemble(my_formula, curr_data, curr_method, nr_classifiers)

# split the data
folds <- createFolds(curr_data[,1], k = nr_ensembles)
fold <- folds$Fold1

VOTES  <-  matrix(0,2,dim(curr_data)[1]) # hard-code binary votes

# set the validation method to cross validation
ctrl <- trainControl(method="cv",summaryFunction=multiClassSummary)

fit <- train( my_formula , 
              data = curr_data[fold,], 
              method = curr_method, 
              tuneLength = 15, 
              trControl = ctrl)

pp <- predict(fit, 
              newdata = curr_data[-fold,], # possibly exclude predictor column
              type = "raw")


for (i in 1:length(pp)){
  if (pp[i] == "One"){
    VOTES[1,i] <- VOTES[1,i]+1
  }
  else{ # Two
    VOTES[2,i] <- VOTES[2,i]+1
  }
}





