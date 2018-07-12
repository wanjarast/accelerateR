pre.comp = function(pre.frame,test.frame,algorithms,Formula,Goals=FALSE){
  test_rows <- nrow(test.frame)
  results <- data.frame(

    OutputLDA=rep(NA,test_rows),
    OutputQDA=rep(NA,test_rows),
    OutputKNN=rep(NA,test_rows),
    OutputCART=rep(NA,test_rows),
    OutputRF=rep(NA,test_rows),
    OutputSVM=rep(NA,test_rows))

  if("LDA" %in% algorithms){
    output_lda <- lda(
      formula=Formula,
      data=pre.frame)
  }
  if("QDA" %in% algorithms){
    output_qda <- qda(
      formula=Formula,
      data=pre.frame)
  }
  if("CART" %in% algorithms){
    output_cart <- rpart(
      formula=Formula,
      method="class", data=pre.frame)
    cart_p <- prune(tree=output_cart,
                    cp=output_cart$cptable[which.min(output_cart$cptable[, "xerror"]), "CP"])
  }
  if("RF" %in% algorithms){
    output_rf <- randomForest(formula=Formula,
                              data=pre.frame)
  }
  if("SVM" %in% algorithms){
    output_svm <- svm(
      formula=Formula,
      data=pre.frame, method="C-classification", kernel="radial")
  }

  if("LDA" %in% algorithms){
    results$OutputLDA <- as.character(predict(output_lda, newdata=test.frame)$class)
  }
  if("QDA" %in% algorithms){
    results$OutputQDA <- as.character(predict(output_qda, newdata=test.frame)$class)
  }
  if("KNN" %in% algorithms){
    output_knn <- kknn(
      formula=Formula,
      train=pre.frame, test=test.frame, k=round(sqrt(nrow(pre.frame)-1)))
    results$OutputKNN <- as.character(fitted(output_knn))
  }
  if("CART" %in% algorithms){
    predict_cart <- predict(object=cart_p, newdata=test.frame)
    results$OutputCART <- colnames(predict_cart)[apply(predict_cart, 1, which.max)]
  }
  if("RF" %in% algorithms){
    results$OutputRF <- as.character(predict(object = output_rf , newdata = test.frame))
  }
  if("SVM" %in% algorithms){
    results$OutputSVM <- as.character(predict(object = output_svm , newdata = test.frame))
  }

  if(length(algorithms)>1){
    results$Majority <- apply(results[, -1], 1, maj)
  }
  if(Goals==T){
    Goal=test.frame$Bev
    Goal = as.character(Goal)
    results = cbind(Goal,results)
  }
  results_clear = results[colSums(!is.na(results)) > 0]
  return(results_clear)
}
