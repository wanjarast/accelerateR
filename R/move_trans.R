move.trans <- function(file,time="timestamp",acc="eobs:accelerations-raw",burst,naxes=3){

  data <- data.table::fread(file , header = T)

  names(data)[names(data) == acc] <- "eobs.accelerations.raw"
  names(data)[names(data) == time] <- "timestamp"

  if(naxes == 3){
  reorder.acc <- function(rows){
    df <-data.frame(x=rep(NA,nrow(rows)*burst),y=NA,z=NA)
    V=matrix(sapply(unlist(strsplit(as.character(rows$eobs.accelerations.raw)," ")),as.numeric),byrow=T,ncol=3)
    df$x <- V[,1]
    df$y <- V[,2]
    df$z <- V[,3]

    return(df)
  }
  output <- data %>%
    dplyr::group_by(timestamp) %>%
    dplyr::do(reorder.acc(.))
  return(output)
  }
  if(naxes == 2){
    reorder.acc <- function(rows){
      df <-data.frame(x=rep(NA,nrow(rows)*burst),y=NA)
      V=matrix(sapply(unlist(strsplit(as.character(rows$eobs.accelerations.raw)," ")),as.numeric),byrow=T,ncol=2)
      df$x <- V[,1]
      df$y <- V[,2]
      return(df)
    }
    output <- data %>%
      dplyr::group_by(timestamp) %>%
      dplyr::do(reorder.acc(.))
    return(output)
  }
  if(naxes == 1){
    reorder.acc <- function(rows){
      df <-data.frame(x=rep(NA,nrow(rows)*burst))
      V=matrix(sapply(unlist(strsplit(as.character(rows$eobs.accelerations.raw)," ")),as.numeric),byrow=T,ncol=1)
      df$x <- V[,1]
      return(df)
    }
    output <- data %>%
      dplyr::group_by(timestamp) %>%
      dplyr::do(reorder.acc(.))
    return(output)
  }
}
