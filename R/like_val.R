like.val = function(data,algorithm,goal){
  conf_mat <- table(data[, c(grep(paste(algorithm),names(data)), grep(paste(goal),names(data)))])
  output <- vector("numeric",nrow(conf_mat))
  for (i in 1:nrow(conf_mat)){
    output[i] <- diag(conf_mat)[i]/sum(conf_mat[i,])
    names(output)[i] <- rownames(conf_mat)[i]
  }
  return(output)
}
