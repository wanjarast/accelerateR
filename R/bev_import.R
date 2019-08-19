bev.import <- function(id , date=NULL , time=NULL , timestamp = NULL , behaviour , rounding = 10 , time_format = dmy_hms , match = FALSE , acc.data , data.path = NULL){
  # read all files from the set tag id
  pat.data <- paste(id,"_behaviour[[:graph:]]*.csv", sep="")

  if(!is.null(data.path)){
    temp.data <- list.files(path = data.path, pattern = pat.data)
  } else{
    temp.data <- list.files(pattern = pat.data)
  }

  data.list <- lapply(temp.data, fread, header = T)
  behaviour.data <- data.table::rbindlist(data.list)
  if(is.null(timestamp)){
    behaviour.data$timestamp <- paste(behaviour.data[[date]] , behaviour.data[[time]] , sep = " ")
    time.function <- match.fun(time_format)
    behaviour.data$timestamp <- time.function(behaviour.data$timestamp)
    behaviour.data$timestamp <- floor_date(behaviour.data$timestamp , unit = seconds(x = rounding))
  }else{
    time.function <- match.fun(time_format)
    behaviour.data$timestamp <- time.function(behaviour.data[[timestamp]])
    behaviour.data$timestamp <- floor_date(behaviour.data$timestamp , unit = seconds(x = rounding))
  }

  if(isTRUE(match)){
    combined <- left_join(data , behaviour.data , by = "timestamp")%>%
      dplyr::select(. , timestamp , x,y,z,behaviour)%>%
      filter(. , complete.cases(.))
    return(combined)
  } else{
    return(behaviour.data)
  }
}
