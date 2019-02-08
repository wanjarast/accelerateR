acto.odba <- function(data , time , ODBA = "ODBA" , cutoff = 1 , night.shift = F){
  data <- data
  names(data)[names(data) == time] <- "timestamp"
  names(data)[names(data) == ODBA] <- "ODBA"

  data$timestamp <- ymd_hms(data$timestamp)

  data$Datum <- as.Date(data$timestamp)


  if(night.shift == T){
    data$Uhrzeit <- strftime(as_datetime(data$timestamp) + hours(12), format="%H:%M" , tz = "UTC")
  }

  else{
    data$Uhrzeit <- strftime(data$timestamp, format="%H:%M" , tz = "UTC")
  }

  data$Datum <- factor(as.character(data$Datum) , levels = unique(as.character(data$Datum)))
  data$date_numeric <- as.numeric(data$Datum)

  data$ODBA[data$ODBA > quantile(x = data$ODBA , probs = cutoff)] <-
    quantile(x = data$ODBA , probs = cutoff)


  unique.date <- c(TRUE , rep(NA,length(lubridate::month(lubridate::ymd(levels(data$Datum))))-1))

  for(i in 2:length(lubridate::month(lubridate::ymd(levels(data$Datum))))){
    unique.date[i] <- !(identical(lubridate::month(lubridate::ymd(levels(data$Datum)))[i],month(lubridate::ymd(levels(data$Datum)))[i-1]))
  }

  if(night.shift == T){
    ggplot()+
      geom_tile(data = data , aes(x = Uhrzeit , y = date_numeric , fill = ODBA) , size =1)+
      theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique.date] , labels = levels(data$Datum)[unique.date])+
      scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                       labels = c("" , "15:00" , "18:00" , "21:00" , "00:00" , "03:00" , "06:00" , "09:00" , "" ))+
      scale_fill_gradient(low = "white", high = "black")+
      ylab(label = "date")+
      xlab(label = "Time of the day")+
      theme(panel.background = element_blank())
  }
  else{
    ggplot()+
      geom_tile(data = data , aes(x = Uhrzeit , y = date_numeric , fill = ODBA) , size =1)+
      theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique.date] , labels = levels(data$Datum)[unique.date])+
      scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                       labels = c("" , "03:00" , "06:00" , "09:00" , "12:00" , "15:00" , "18:00" , "21:00" , "" ))+
      scale_fill_gradient(low = "white", high = "black")+
      ylab(label = "date")+
      xlab(label = "Time of the day")+
      theme(panel.background = element_blank())
  }

}
