pre.cross = function(pre.frame,Formula){
  #pre.frame is the prepared data.frame (output of pre.sample()) which shall be used for the prediction
  results <- data.frame(
    Goal=pre.frame$Bev,
    OutputLDA=NA,
    OutputQDA=NA,
    OutputKNN=NA,
    OutputCART=NA,
    OutputRF=NA,
    OutputSVM=NA)

  pb <- txtProgressBar(min = 0, max = nrow(pre.frame), style = 3)

  for(i in 1:nrow(pre.frame)) {

    output_lda <- lda(
      formula=Formula,
      data=pre.frame[-i, ])
    results$OutputLDA[i] <- as.character(predict(output_lda, newdata=pre.frame[i, ])$class)

    output_qda <- qda(
      formula=Formula,
      data=pre.frame[-i, ])
    results$OutputQDA[i] <- as.character(predict(output_qda, newdata=pre.frame[i, ])$class)

    output_knn <- kknn(
      formula=Formula,
      train=pre.frame[-i, ], test=pre.frame[i, ], k=round(sqrt(nrow(pre.frame)-1)))
    results$OutputKNN[i] <- as.character(fitted(output_knn))

    output_cart <- rpart(
      formula=Formula,
      method="class", data=pre.frame[-i, ])
    cart_p <- prune(tree=output_cart,
                    cp=output_cart$cptable[which.min(output_cart$cptable[, "xerror"]), "CP"])
    predict_cart <- predict(object=cart_p, newdata=pre.frame[i, ])
    results$OutputCART[i] <- colnames(predict_cart)[apply(predict_cart, 1, which.max)]

    output_rf <- randomForest(formula=Formula,
                              data=pre.frame[-i,])
    results$OutputRF[i] <- as.character(predict(object = output_rf , newdata = pre.frame[i,]))

    output_svm <- svm(
      formula=Formula,
      data=pre.frame[-i,], method="C-classification", kernel="radial")
    results$OutputSVM[i] <- as.character(predict(object = output_svm , newdata = pre.frame[i,]))

    setTxtProgressBar(pb, i)
  }
  results$Majority <- apply(results[, -1], 1, maj)
  results$Goal = sapply(results$Goal, as.character)
  return(results)
}
