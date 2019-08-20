tag_import <- function(tag_id , rounding = 10 , eobs = TRUE , data_path = NULL){
  # read all files from the set tag id
  pat_data <- paste(tag_id,"_acc[[:graph:]]*.txt" , sep="")

  if(!is.null(data_path)){
    temp_data <- list.files(path = data_path, pattern = pat_data , full.names = T)
  } else{
    temp_data <- list.files(pattern = pat_data)
  }
  if(isTRUE(eobs)){
    data_list <- lapply(temp_data, fread, drop = c(1,3) , col.names = c("date","time","x","y","z"))
    data <- data.table::rbindlist(data_list)
    # create the timestamp coulumn and round the seconds down to full 10s
    data$timestamp <- paste(data$date , data$time , sep = " ")
    data$timestamp <- dmy_hms(data$timestamp)
    data$timestamp <- floor_date(data$timestamp , unit = seconds(x = rounding))
  } else{
    data_list <- lapply(temp_data, fread)
    data <- data.table::rbindlist(data_list)
  }
  return(data)
}
