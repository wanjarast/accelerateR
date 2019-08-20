pre_metrics = function(predicted , expected , uncertain = NULL){

  conf_table <- table(predicted = predicted , expected = expected) #compute the confusion matrix

  conf_names <- colnames(conf_table) #get the name of the behaviours

  result <- matrix(nrow = 2 , ncol = length(conf_names) , dimnames = list(c("recall" , "precision"),
                                                                          c(conf_names)))
  if(!(is.null(uncertain))){
    others <- sum(conf_table[rownames(conf_table) == uncertain])/sum(conf_table)
  }

  for(i in conf_names){
    correct <- conf_table[rownames(conf_table) == i , colnames = i]
    rowsum <- sum(conf_table[rownames(conf_table) == i , ])
    colsum <- sum(conf_table[ , colnames(conf_table) == i])

    result[rownames(result) == "recall" , colnames(result) == i] <- correct / colsum
    result[rownames(result) == "precision" , colnames(result) == i] <- correct / rowsum
  }
  if(!(is.null(uncertain))){
  return(list(confusion_matrix = conf_table , metrics = result,uncertain_predictions = others))
  }
  if(is.null(uncertain)){
    return(list(confusion_matrix = conf_table , metrics = result))
  }
}
