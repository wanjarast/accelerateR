maj <- function(x) {
  tab <- table(x)
  if(length(tab)==1) return(names(tab))
  return(names(which.max(sample(tab))))
}
