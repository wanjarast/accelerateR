conf.mat = function(data,algorithm,goal){
  return(table(data[, c(grep(paste(algorithm),names(data)), grep(paste(goal),names(data)))]))
}
