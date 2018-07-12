net_prep <- function(data,time,burst){
by(data,data[,paste(time)],function(chunklet){
  x <- matrix(chunklet$x,burst,1,byrow = T)
  y <- matrix(chunklet$y,burst,1,byrow = T)
  z <- matrix(chunklet$z,burst,1,byrow = T)
  array(c(x,y,z),dim = c(burst,1,3))}) %>%
  ##bind every list item(arrays) into 1 giant array
  abind(. , along = 4) %>%
  #reorder the dimentions of the array (though I have no idea what that means)
  aperm(c(4,1,2,3))
#should be 497 110 1 3 (497 burst in total, 110 measurments per burst, 1 axes per array level , 3 array levels for x,y,z axis)
}
