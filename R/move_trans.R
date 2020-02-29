move_trans <- function(file = NULL,object = NULL,time="timestamp",acc="eobs:accelerations-raw",burst,naxes=3){

  if(!is.null(file)){
    data <- data.table::fread(file , header = T)
  }

  if(!is.null(object)){
    data <- object
  }

  names(data)[names(data) == acc] <- "eobs_accelerations_raw"
  names(data)[names(data) == time] <- "timestamp"
  burst_diff <- NULL

  if(naxes == 3){
    reorder_acc <- function(rows){
      df <-data.frame(x=rep(NA,nrow(rows)*burst),y=NA,z=NA)
      V=matrix(sapply(unlist(strsplit(as.character(rows$eobs_accelerations_raw)," ")),as.numeric),byrow=T,ncol=3)
      if(nrow(V) == burst){
        df$x <- V[,1]
        df$y <- V[,2]
        df$z <- V[,3]
      }
      if(nrow(V) != burst){
        assign(x =  "burst_diff", value = c(burst_diff , nrow(V)) , inherits = T)
      }
      return(df)
    }
    output <- data%>%
      dplyr::group_by(timestamp)%>%
      dplyr::group_modify(~ reorder_acc(.x))%>%
      dplyr::filter(. , !is.na(x))

    if(!is.null(burst_diff)){
      burst_diff <- unique(burst_diff)
      warning("Found burst of length ", paste(burst_diff , collapse = " , ")  , call. = F)
    }

    return(output)


  }
  if(naxes == 2){
    reorder_acc <- function(rows){
      df <-data.frame(x=rep(NA,nrow(rows)*burst),y=NA)
      V=matrix(sapply(unlist(strsplit(as.character(rows$eobs_accelerations_raw)," ")),as.numeric),byrow=T,ncol=2)
      if(nrow(V) == burst){
        df$x <- V[,1]
        df$y <- V[,2]
      }
      if(nrow(V) != burst){
        assign(x =  "burst_diff", value = c(burst_diff , nrow(V)) , inherits = T)
      }
      return(df)
    }
    output <- data%>%
      dplyr::group_by(timestamp)%>%
      dplyr::do(reorder_acc(.))

    if(!is.null(burst_diff)){
      burst_diff <- unique(burst_diff)
      warning("Found burst of length ", paste(burst_diff , collapse = " , ")  , call. = F)
    }

    return(output)
  }
  if(naxes == 1){
    reorder_acc <- function(rows){
      df <-data.frame(x=rep(NA,nrow(rows)*burst))
      V=matrix(sapply(unlist(strsplit(as.character(rows$eobs_accelerations_raw)," ")),as.numeric),byrow=T,ncol=1)
      if(nrow(V) == burst){
        df$x <- V[,1]
      }
      if(nrow(V) != burst){
        assign(x =  "burst_diff", value = c(burst_diff , nrow(V)) , inherits = T)
      }
      return(df)
    }
    output <- data%>%
      dplyr::group_by(timestamp)%>%
      dplyr::do(reorder_acc(.))

    if(!is.null(burst_diff)){
      burst_diff <- unique(burst_diff)
      warning("Found burst of length ", paste(burst_diff , collapse = " , ")  , call. = F)
    }

    return(output)
  }
}
