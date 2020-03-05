pre_metrics = function(predicted , expected , uncertain = NULL){

  conf_table_print <- table(predicted = predicted , expected = expected) #compute the confusion matrix

  # remove the row with the "other" predictions as it makes the calculations impossible
  if(!is.null(uncertain)){
    conf_table <- conf_table_print[!(rownames(conf_table_print) == uncertain),]
    other_table <- conf_table_print[(rownames(conf_table_print) == uncertain),]
  }
  else{
    conf_table <- table(predicted = predicted , expected = expected)
  }

  # total number of all classifications (needed for the accuracy)
  if(!is.null(uncertain)){
    total_n <- sum(conf_table) + sum(other_table)
  }
  else{
    total_n <- sum(conf_table)
  }

  correct_sum <- sum(diag(conf_table))

  conf_names <- colnames(conf_table) #get the name of the behaviours

  result <- matrix(nrow = 3 , ncol = length(conf_names) , dimnames = list(c("recall" , "precision" , "accuracy"),
                                                                          c(conf_names)))
  if(!(is.null(uncertain))){
    others <- sum(other_table)
  }

  for(i in conf_names){
    correct <- conf_table[rownames(conf_table) == i , colnames = i]
    rowsum <- sum(conf_table[rownames(conf_table) == i , ])
    if(!(is.null(uncertain))){
      colsum <- sum(conf_table[ , colnames(conf_table) == i] + other_table[names(other_table) == i])
    }
    colsum <- sum(conf_table[ , colnames(conf_table) == i])
    if(!(is.null(uncertain))){
      true_negatives <- sum(conf_table[rownames(conf_table) != i , colnames(conf_table) != i])
    }
    else{
      true_negatives <- sum(conf_table[rownames(conf_table) != i , colnames(conf_table) != i]) + sum(other_table[names(other_table) != i])
    }

    result[rownames(result) == "recall" , colnames(result) == i] <- correct / colsum
    result[rownames(result) == "precision" , colnames(result) == i] <- correct / rowsum
    result[rownames(result) == "accuracy" , colnames(result) == i] <- (correct + true_negatives)/total_n
  }

  observed_accuray <- correct_sum / total_n
  expected_accuray <- (sum(colSums(conf_table) * rowSums(conf_table))/total_n)/total_n
  kappa <- (observed_accuray - expected_accuray) / (1 - expected_accuray)

  if(!(is.null(uncertain))){
    return(list(confusion_matrix = conf_table_print , metrics = result , kappa = kappa , uncertain_predictions = others))
  }
  if(is.null(uncertain)){
    return(list(confusion_matrix = conf_table_print , metrics = result , kappa = kappa))
  }
}
