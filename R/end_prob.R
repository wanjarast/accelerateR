end.prob = function(data,algorithms,goal){
  output <- rbind(
    if("LDA" %in% algorithms){
      lda=per.val(data = data , algorithm = "LDA" , goal = goal)},
    if("QDA" %in% algorithms){
      qda=per.val(data = data , algorithm = "QDA" , goal = goal)},
    if("KNN" %in% algorithms){
      knn=per.val(data = data , algorithm = "KNN" , goal = goal)},
    if("CART" %in% algorithms){
      cart=per.val(data = data , algorithm = "CART" , goal = goal)},
    if("RF" %in% algorithms){
      rf=per.val(data = data , algorithm = "RF" , goal = goal)},
    if("SVM" %in% algorithms){
      svm=per.val(data = data , algorithm = "SVM" , goal = goal)},
    if("Majority" %in% algorithms){
      majority=per.val(data = data , algorithm = "Majority" , goal = goal)}
  )
  rownames(output) <- algorithms
  return(output)
}
