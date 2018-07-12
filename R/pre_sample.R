pre.sample = function(predictors , behaviours , precount){
  #predictors is the data.frame with all predictors to be used
  #behaviours is a vector with all behaviours that are used for the model
  #precount is the count of observations for each behaviour
  frame1 = NULL
  for (i in behaviours){
    frame2 = subset(predictors,predictors$Bev==i)[sample(1:nrow(subset(predictors,predictors$Bev==i))),][1:precount,]
    frame1 = rbind(frame2 , frame1)
    rm(frame2)
  }
  frame1$Bev <- droplevels(frame1$Bev)
  return(frame1)
}
