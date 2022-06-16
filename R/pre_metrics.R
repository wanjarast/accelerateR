pre_metrics = function(predicted , expected , uncertain = NULL){
  expected <- factor(expected)
  if(!is.null(uncertain)){
    predicted <- factor(predicted , levels = c(levels(expected) , uncertain))
  }  else{
    predicted <- factor(predicted , levels = levels(expected))
  }

  #compute the confusion matrix
  conf_table_print <- table(predicted = predicted , expected = expected)


  # remove the row with the "other" predictions as it makes the calculations impossible
  if(!is.null(uncertain)){
    conf_table <- conf_table_print[!(rownames(conf_table_print) == uncertain),]
    other_table <- conf_table_print[(rownames(conf_table_print) == uncertain),]
  }    else{
    conf_table <- table(predicted = predicted , expected = expected)
  }

  # total number of all classifications (needed for the accuracy)
  if(!is.null(uncertain)){
    total_n <- sum(conf_table) + sum(other_table)
  }    else{
    total_n <- sum(conf_table)
  }

  correct_sum <- sum(diag(conf_table))

  conf_names <- colnames(conf_table) #get the name of the behaviours

  result <- matrix(nrow = 5 , ncol = length(conf_names) ,
                   dimnames = list(c("recall" , "precision" , "f1" , "false_positive_rate" , "accuracy"),
                                   c(conf_names)))
  if(!(is.null(uncertain))){
    others <- sum(other_table)
  }

  for(i in conf_names){
    # True Positives
    correct <- conf_table[rownames(conf_table) == i , colnames = i]
    # Row sum
    rowsum <- sum(conf_table[rownames(conf_table) == i , ])

    if(!(is.null(uncertain))){
      # Column sum
      colsum <- sum(conf_table[ , colnames(conf_table) == i] , other_table[names(other_table) == i])
      # True Negatives
      true_negatives <- sum(conf_table[rownames(conf_table) != i , colnames(conf_table) != i]) +
        sum(other_table[names(other_table) != i])
      # False Negatives
      false_negatives <- sum(conf_table[ , colnames(conf_table) == i]) +
        sum(other_table[names(other_table) == i]) - correct
    }      else{
      # Column sum
      colsum <- sum(conf_table[ , colnames(conf_table) == i])
      # True Negatives
      true_negatives <- sum(conf_table[rownames(conf_table) != i , colnames(conf_table) != i])
      # False Negatives
      false_negatives <- sum(conf_table[ , colnames(conf_table) == i]) - correct
    }

    # False Positives
    false_positives <- sum(conf_table[rownames(conf_table) == i , ]) - correct

    recall <- correct / colsum
    precision <- correct / rowsum
    false_positive_rate <- false_positives / (false_positives + true_negatives)
    accuracy <- (correct + true_negatives)/total_n
    f1 <- 2 * (recall * precision) / (recall + precision)

    result[rownames(result) == "recall" , colnames(result) == i] <- recall
    result[rownames(result) == "precision" , colnames(result) == i] <- precision
    result[rownames(result) == "false_positive_rate" , colnames(result) == i] <- false_positive_rate
    result[rownames(result) == "accuracy" , colnames(result) == i] <- accuracy
    result[rownames(result) == "f1" , colnames(result) == i] <- f1
  }

  observed_accuray <- correct_sum / total_n
  expected_accuray <- (sum(colSums(conf_table) * rowSums(conf_table))/total_n)/total_n
  kappa <- (observed_accuray - expected_accuray) / (1 - expected_accuray)

  if(!(is.null(uncertain))){
    return(list(confusion_matrix = conf_table_print , metrics = result , kappa = kappa , uncertain_predictions = (others / total_n)))
  }
  if(is.null(uncertain)){
    return(list(confusion_matrix = conf_table_print , metrics = result , kappa = kappa))
  }
}
