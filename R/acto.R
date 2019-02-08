acto <- function(data , time , behaviour , target.bev , daily = FALSE , night.shift = F){
  data <- data
  names(data)[names(data) == time] <- "timestamp"
  names(data)[names(data) == behaviour] <- "Verhalten"

  data$timestamp <- ymd_hms(data$timestamp)
  data$Datum <- as.Date(data$timestamp)
  data$Verhalten <- factor(data$Verhalten)
  data$color <- ifelse(data$Verhalten %in% target.bev , 1 , 0)
  data$color <- factor(data$color)
  data$Datum <- factor(as.character(data$Datum) , levels = unique(as.character(data$Datum)))
  data$date_numeric <- as.numeric(data$Datum)

  if(night.shift == T){
    data$Uhrzeit <- strftime(as_datetime(data$timestamp) + hours(12), format="%H:%M" , tz = "UTC")
  }

  else{
    data$Uhrzeit <- strftime(data$timestamp, format="%H:%M" , tz = "UTC")
  }


  if(daily == TRUE){

    ggplot()+
      geom_point(data = data , aes(x = Uhrzeit , y = date_numeric , color = color) , shape = 15 , size =8)+
      scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+ #grey92 als Standard Hintergrundfarbe
      theme(legend.position = "none" ,axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1) , labels = levels(data$Datum))+
      scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                       labels = c("" , "03:00" , "06:00" , "09:00" , "12:00" , "15:00" , "18:00" , "21:00" , "" ))+
      ylab(label = "date")+
      xlab(label = "Time of the day")+
      theme(panel.background = element_blank())

  }
  else{
    unique.date <- c(TRUE , rep(NA,length(lubridate::month(lubridate::ymd(levels(data$Datum))))-1))

    for(i in 2:length(lubridate::month(lubridate::ymd(levels(data$Datum))))){
      unique.date[i] <- !(identical(lubridate::month(lubridate::ymd(levels(data$Datum)))[i],month(lubridate::ymd(levels(data$Datum)))[i-1]))
    }

    ggplot()+
      geom_tile(data = data , aes(x = Uhrzeit , y = date_numeric , color = color) , size =1)+
      scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+ #grey92 als Standard Hintergrundfarbe
      theme(legend.position = "none" ,axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique.date] , labels = levels(data$Datum)[unique.date])+
      scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                       labels = c("" , "03:00" , "06:00" , "09:00" , "12:00" , "15:00" , "18:00" , "21:00" , "" ))+
      ylab(label = "date")+
      xlab(label = "Time of the day")+
      theme(panel.background = element_blank())
  }

}
