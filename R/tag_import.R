tag.import <- function(tag.id , rounding = 10 , eobs = TRUE , data.path = NULL){
  # read all files from the set tag id
  pat.data <- paste(tag.id,"_acc[[:graph:]]+.txt", "|" ,tag.id,"_acc+.txt", sep="")

  if(!is.null(data.path)){
    temp.data <- list.files(path = data.path, pattern = pat.data)
  } else{
    temp.data <- list.files(pattern = pat.data)
  }
  if(isTRUE(eobs)){
    data.list <- lapply(temp.data, fread, drop = c(1,3) , col.names = c("date","time","x","y","z"))
    data <- data.table::rbindlist(data.list)
    # create the timestamp coulumn and round the seconds down to full 10s
    data$timestamp <- paste(data$date , data$time , sep = " ")
    data$timestamp <- dmy_hms(data$timestamp)
    data$timestamp <- floor_date(data$timestamp , unit = seconds(x = rounding))
  } else{
    data.list <- lapply(temp.data, fread)
    data <- data.table::rbindlist(data.list)
  }
  return(data)
}
