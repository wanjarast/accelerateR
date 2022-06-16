count_test <- function(data, burstcount, time = "timestamp", id = NULL, remove = FALSE){
  data <- data.table::copy(data)

  if(!is.null(id)){
    # data[, index := paste(unlist(data[, ..time]), unlist(data[, ..id]), sep = "_")]
    data[, n := .N , by = c(time, id)]
  }

  else{
    # data[, index := data[, ..time]]
    data[, n := .N , by = time]
  }

  data[, keep := n == burstcount]

  data_remove <- data[isFALSE(keep)]

  if(nrow(data_remove) < 1){
    message("No missing values")
    return(data)
  }

  else{
    if(remove){
      output <- data[!data_remove, on = c(time, id)]

      removed_length <- nrow(unique(data_remove, by = c(time, id)))

      if(removed_length == 1){
        message(paste(1 , "burst was removed"))
      }
      else{
        message(paste(removed_length , "bursts were removed"))
      }

      return(output)
    }

    else{
      return(data_remove)
    }
  }
}
