count_test <- function(data,burstcount,time="timestamp" , remove = FALSE){
  names(data)[names(data) == time] <- "timestamp"

  df <- data %>%
    dplyr::group_by(timestamp) %>%
    dplyr::summarise(. , n()==burstcount)
  df_false <- df[df$`n() == burstcount`==FALSE,1]

  if(nrow(df_false)==0){
    print("No missing values")
  }
  else{
    if(remove == TRUE){
      output <- data[!ymd_hms(data$timestamp) %in% ymd_hms(df_false$timestamp),]
      cat(paste(nrow(df_false) , "timestamps were removed."))
      return(output)
    }
    else{
      return(df_false)
    }
  }
}
