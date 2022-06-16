move_trans = function(data = NULL, path = NULL, timestamp = "timestamp", acc = "eobs:accelerations-raw",
         id = "individual-local-identifier", sample_frequency = "eobs:acceleration-sampling-frequency-per-axis" ,
         naxes = 3, no_cores = 1){

  if(!is.null(path)){
    data <- data.table::fread(path , header = T)
  }

  else{
    data <- data.table::copy(data)
  }

  rearrange_func <- function(data, n, naxes, acc, id, sample_frequency){

    acc_data <- data[n, ..acc]

    acc_data <- strsplit(as.character(acc_data)," ")%>%
      unlist(.)%>%
      as.numeric(.)

    if(naxes == 3){
      x <- acc_data[seq(1 , length(acc_data) , 3)]
      y <- acc_data[seq(2 , length(acc_data) , 3)]
      z <- acc_data[seq(3 , length(acc_data) , 3)]

      result <- data.table(x = x , y = y , z = z)
    }

    if(naxes == 2){
      x <- acc_data[seq(1 , length(acc_data) , 2)]
      y <- acc_data[seq(2 , length(acc_data) , 2)]

      result <- data.table(x = x , y = y)
    }

    if(naxes == 1){
      x <- acc_data

      result <- data.table(x = x)
    }

    result[, timestamp := data[n, ..timestamp]]

    result[, id := unlist(data[n, ..id])]

    result[, sample_frequency := data[n, ..sample_frequency]]

    result[, burst_size := length(x)]

    return(result)
  }

  if(no_cores == 1){
    result <- foreach(n = 1:nrow(data), .combine = rbind,
                      .packages = c("accelerateR")) %do% rearrange_func(data, n,
                                                                        naxes = naxes, acc = acc, id = id,
                                                                        sample_frequency = sample_frequency)
  }

  else{
    # Initiate cluster
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)

    result_list <- foreach(n = 1:nrow(data),
                           .packages = c("accelerateR")) %dopar% rearrange_func(data, n,
                                                                                naxes = naxes, acc = acc, id = id,
                                                                                sample_frequency = sample_frequency)

    # close the cluster
    stopCluster(cl)

    result <- rbindlist(result_list)
  }


  message("Found burst of length ", paste(unique(result$burst_size) , collapse = " , "))

  return(result)
}
