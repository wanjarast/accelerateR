move_trans <- function(file = NULL,object = NULL,time="timestamp",acc="eobs:accelerations-raw",burst,naxes=3){

  if(!is.null(file)){
    data <- data.table::fread(file , header = T)
  }

  if(!is.null(object)){
    data <- object
  }

  data <- setDT(data)

  names(data)[names(data) == acc] <- "eobs_accelerations_raw"
  names(data)[names(data) == time] <- "timestamp"

  data$eobs_accelerations_raw <- as.character(data$eobs_accelerations_raw)




  if(naxes == 3){
    # splits the raw acceleration character column into seperate values
    # sperarates by timestamp and saves each timestamp in a list element
    char_split <- data[ , .(lapply(X = strsplit(as.character(eobs_accelerations_raw)," ") , as.numeric)) , by = timestamp]

    # extracts only the list elements
    char_list <- char_split$V1

    # names the list elements according to the original timestamps
    names(char_list) <- as.character(char_split$timestamp)

    # counts the number of measurments for all three axes for each burst
    counts <- unlist(lapply(char_list,length))

    # saves all measurment counts that differ from the expacted
    other_sizes <- unique(counts)[unique(counts) != burst*3]/3

    # removes all bursts with a different number of measurments than expected
    char_list <- char_list[names(char_list) %in% names(counts)[counts == burst*3]]

    # reshapes each list into a matrix -> output is still a list
    output <- lapply(char_list , matrix , ncol=3)%>%
      # turns each list element matrix into a data.table
      lapply(. , as.data.table)%>%
      # bind all data.tables into one data.table
      rbindlist(l = . ,idcol = T)

    # renames the columns in the output data.table
    names(output) <- c("timestamp","x","y","z")
    if(length(other_sizes) != 0){
      # puts out a warning if the data set contains bursts of differt sizes
      warning("Found burst of length ", paste(other_sizes , collapse = " , ")  , call. = F)
    }
    return(output)

  }

  if(naxes == 2){
    # splits the raw acceleration character column into seperate values
    # sperarates by timestamp and saves each timestamp in a list element
    char_split <- data[ , .(lapply(X = strsplit(as.character(eobs_accelerations_raw)," ") , as.numeric)) , by = timestamp]

    # extracts only the list elements
    char_list <- char_split$V1

    # names the list elements according to the original timestamps
    names(char_list) <- as.character(char_split$timestamp)

    # counts the number of measurments for all three axes for each burst
    counts <- unlist(lapply(char_list,length))

    # saves all measurment counts that differ from the expacted
    other_sizes <- unique(counts)[unique(counts) != burst*2]/2

    # removes all bursts with a different number of measurments than expected
    char_list <- char_list[names(char_list) %in% names(counts)[counts == burst*2]]

    # reshapes each list into a matrix -> output is still a list
    output <- lapply(char_list , matrix , ncol=2)%>%
      # turns each list element matrix into a data.table
      lapply(. , as.data.table)%>%
      # bind all data.tables into one data.table
      rbindlist(l = . ,idcol = T)

    # renames the columns in the output data.table
    names(output) <- c("timestamp","x","y")
    if(length(other_sizes) != 0){
      # puts out a warning if the data set contains bursts of differt sizes
      warning("Found burst of length ", paste(other_sizes , collapse = " , ")  , call. = F)
    }
    return(output)

  }
  if(naxes == 1){
    # splits the raw acceleration character column into seperate values
    # sperarates by timestamp and saves each timestamp in a list element
    char_split <- data[ , .(lapply(X = strsplit(as.character(eobs_accelerations_raw)," ") , as.numeric)) , by = timestamp]

    # extracts only the list elements
    char_list <- char_split$V1

    # names the list elements according to the original timestamps
    names(char_list) <- as.character(char_split$timestamp)

    # counts the number of measurments for all three axes for each burst
    counts <- unlist(lapply(char_list,length))

    # saves all measurment counts that differ from the expacted
    other_sizes <- unique(counts)[unique(counts) != burst*1]/1

    # removes all bursts with a different number of measurments than expected
    char_list <- char_list[names(char_list) %in% names(counts)[counts == burst*1]]

    # reshapes each list into a matrix -> output is still a list
    output <- lapply(char_list , matrix , ncol=1)%>%
      # turns each list element matrix into a data.table
      lapply(. , as.data.table)%>%
      # bind all data.tables into one data.table
      rbindlist(l = . ,idcol = T)

    # renames the columns in the output data.table
    names(output) <- c("timestamp","x")
    if(length(other_sizes) != 0){
      # puts out a warning if the data set contains bursts of differt sizes
      warning("Found burst of length ", paste(other_sizes , collapse = " , ")  , call. = F)
    }
    return(output)

  }
}
