end.like = function(data,algorithms,goal){
  output <- rbind(
    if("LDA" %in% algorithms){
      lda=like.val(data = data , algorithm = "LDA" , goal = goal)},
    if("QDA" %in% algorithms){
      qda=like.val(data = data , algorithm = "QDA" , goal = goal)},
    if("KNN" %in% algorithms){
      knn=like.val(data = data , algorithm = "KNN" , goal = goal)},
    if("CART" %in% algorithms){
      cart=like.val(data = data , algorithm = "CART" , goal = goal)},
    if("RF" %in% algorithms){
      rf=like.val(data = data , algorithm = "RF" , goal = goal)},
    if("SVM" %in% algorithms){
      svm=like.val(data = data , algorithm = "SVM" , goal = goal)},
    if("Majority" %in% algorithms){
      majority=like.val(data = data , algorithm = "Majority" , goal = goal)}
  )
  rownames(output) <- algorithms
  return(output)
}
