pre.metrics = function(predicted , expected , uncertain = NULL){

  conf.table <- table(predicted = predicted , expected = expected) #compute the confusion matrix

  conf.names <- colnames(conf.table) #get the name of the behaviours

  result <- matrix(nrow = 2 , ncol = length(conf.names) , dimnames = list(c("recall" , "precision"),
                                                                          c(conf.names)))
  if(!(is.null(uncertain))){
    others <- sum(conf.table[rownames(conf.table) == uncertain])/sum(conf.table)
  }

  for(i in conf.names){
    correct <- conf.table[rownames(conf.table) == i , colnames = i]
    rowsum <- sum(conf.table[rownames(conf.table) == i , ])
    colsum <- sum(conf.table[ , colnames(conf.table) == i])

    result[rownames(result) == "recall" , colnames(result) == i] <- correct / colsum
    result[rownames(result) == "precision" , colnames(result) == i] <- correct / rowsum
  }

  return(list(confusion_matrix = conf.table , metrics = result,uncertain_predictions = others))
}
