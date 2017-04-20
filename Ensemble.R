ensemble <- function(my_formula, predict_col, curr_data, curr_method, nr_ensembles){

  bootstrap_fraction <- 4/5
  training_fraction <- 3/4
  nr_datapoints <- dim(curr_data)[1]
  nr_samples <- round(nr_datapoints * training_fraction)
  nr_unique_values <- length(unique(curr_data[,predict_col]))
  
  
  # create the resampled data indeces
  folds <- matrix(0,nr_ensembles,nr_samples)
  for (i in 1:nr_ensembles){
    # sample with replacement
    folds[i,] <- sample(1:nr_datapoints, size = nr_samples, replace = TRUE)  
  }
  
  
  # set the validation method to cross validation
  ctrl <- trainControl(method="cv", summaryFunction=multiClassSummary)
  
  # split the data
  in_train  <-  createDataPartition(curr_data[,predict_col], p=3/4, list=FALSE)
  train_data <- curr_data[in_train,]
  test_data <- curr_data[-in_train,]
  
  VOTES  <-  matrix(0,nr_unique_values, dim(test_data)[1]) 
  
  # train the classifier on each of the splits
  for (iEnsemble in 1:nr_ensembles ){
    
    fold <- folds[iEnsemble,]
    fit <- train( my_formula , 
                  data = curr_data[fold,], 
                  method = curr_method, 
                  tuneLength = 15, 
                  trControl = ctrl)
    
    pp <- predict(fit, 
                  newdata = test_data[,-predict_col], # possibly exclude predictor column
                  type = "raw")
    
    # count the votes  
    values <- unique(curr_data[,predict_col])
    for (i in 1:length(pp)){
      for (j in 1:length(values)){
        if (pp[i] == values[j]){
          VOTES[j,i] <- VOTES[j,i]+1
        }  
      }
    }
  }
  
  predictions <- rep(0, dim(test_data)[1])
  
  # create predictions for the validation set
  for (i in 1:length(pp)){
    predictions[i] <- which.max(VOTES[,i])
  }
  
  predictions <- factor(predictions,labels=values)
  
  
  err <- length( predictions[predictions != test_data[,predict_col]]) / length(predictions)
  # return the predictions
  return(err)
  
}


