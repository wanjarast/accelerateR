bev_import <- function(id , date=NULL , time=NULL , timestamp = NULL , behaviour , rounding = 10 ,
                       time_format = dmy_hms , match = FALSE , acc_data , data_path = NULL){
  # read all files from the set tag id
  pat_data <- paste(id,"_behaviour[[:graph:]]*.csv", sep="")

  if(!is.null(data_path)){
    temp_data <- list.files(path = data_path, pattern = pat_data , full.names = T)
  } else{
    temp_data <- list.files(pattern = pat_data)
  }

  data_list <- lapply(temp_data, fread, header = T)
  behaviour_data <- data.table::rbindlist(data_list)
  if(is.null(timestamp)){
    behaviour_data$timestamp <- paste(behaviour_data[[date]] , behaviour_data[[time]] , sep = " ")
    time_function <- match.fun(time_format)
    behaviour_data$timestamp <- time_function(behaviour_data$timestamp)
    behaviour_data$timestamp <- floor_date(behaviour_data$timestamp , unit = seconds(x = rounding))
  }else{
    time_function <- match.fun(time_format)
    behaviour_data$timestamp <- time_function(behaviour_data[[timestamp]])
    behaviour_data$timestamp <- floor_date(behaviour_data$timestamp , unit = seconds(x = rounding))
  }

  if(isTRUE(match)){
    combined <- left_join(acc_data , behaviour_data , by = "timestamp")%>%
      dplyr::select(. , timestamp , x,y,z,behaviour)%>%
      filter(. , complete.cases(.))
    return(combined)
  } else{
    return(behaviour_data)
  }
}
