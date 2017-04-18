ensemble <- function(labels, data, method, nrEnsembles){
  
  # split the data
  indeces <- createFolds(labels, k = nrEnsembles)
  
  
  # train the classifier on each of the splits
  for (fold in indeces){
    
    # Training data
    trainX <- data[fold]
    testX <- data[-fold]
    
    # Training labels
    trainY <- labels[fold]
    testY <- labels[-fold]
    
    
  }
  
  
  # count the votes
  
  # create predictions for the validation set
  
  # return the classifier
  
}