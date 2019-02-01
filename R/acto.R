acto <- function(data , time , date , behaviour , target.bev , daily = FALSE){
  data <- data
  names(data)[names(data) == date] <- "Datum"
  names(data)[names(data) == time] <- "Uhrzeit"
  names(data)[names(data) == behaviour] <- "Verhalten"

  data$Verhalten <- factor(data$Verhalten)
  data$color <- ifelse(data$Verhalten %in% target.bev , 1 , 0)
  data$color <- factor(data$color)
  data$Datum <- factor(as.character(data$Datum) , levels = unique(as.character(data$Datum)))
  data$date_numeric <- as.numeric(data$Datum)


  if(daily == TRUE){

    ggplot()+
      geom_point(data = data , aes(x = Uhrzeit , y = date_numeric , color = color) , shape = 15 , size =8)+
      scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+ #grey92 als Standard Hintergrundfarbe
      theme(legend.position = "none" ,axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(complete$Datum)) , 1) , labels = levels(complete$Datum))+
      scale_x_discrete(breaks = c("02:00" , "04:00" , "06:00" , "08:00" , "10:00" , "12:00",
                                  "14:00" , "16:00" , "18:00" , "20:00" , "22:00"), labels = c("02:00" , "04:00" , "06:00" , "08:00" , "10:00" , "12:00",
                                                                                               "14:00" , "16:00" , "18:00" , "20:00" , "22:00"))+
      ylab(label = "date")+
      theme(panel.background = element_blank())

  }
  else{
    unique.date <- c(TRUE , rep(NA,length(lubridate::month(lubridate::ymd(levels(data$Datum))))-1))

    for(i in 2:length(lubridate::month(lubridate::ymd(levels(data$Datum))))){
      unique.date[i] <- !(identical(lubridate::month(lubridate::ymd(levels(data$Datum)))[i],month(lubridate::ymd(levels(data$Datum)))[i-1]))
    }

    ggplot()+
      geom_point(data = data , aes(x = Uhrzeit , y = date_numeric , color = color) , shape = 15 , size =8)+
      scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+ #grey92 als Standard Hintergrundfarbe
      theme(legend.position = "none" ,axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(complete$Datum)) , 1)[unique.date] , labels = levels(complete$Datum)[unique.date])+
      scale_x_discrete(breaks = c("02:00" , "04:00" , "06:00" , "08:00" , "10:00" , "12:00",
                                  "14:00" , "16:00" , "18:00" , "20:00" , "22:00"), labels = c("02:00" , "04:00" , "06:00" , "08:00" , "10:00" , "12:00",
                                                                                               "14:00" , "16:00" , "18:00" , "20:00" , "22:00"))+
      ylab(label = "date")+
      theme(panel.background = element_blank())
  }

}
