count.test <- function(data,burstcount,time="timestamp"){
  df <- data %>%
    dplyr::group_by_(time) %>%
    dplyr::summarise(. , n()==burstcount)
  df_false <- df[df$`n() == burstcount`==FALSE,1]

  if(nrow(df_false)==0){print("No missing values")}
  else{
    return(df_false)}
}
