count.test <- function(data,burstcount,time="timestamp"){
  df <- data %>%
    group_by_(time) %>%
    summarise(.,n()==burstcount)
  df_false <- df[df$`n() == burstcount`==FALSE,1]

  if(nrow(df_false)==0){print("No missing values")}
  else{
    return(df_false)}
}
