acto <- function(data , time , behaviour , target.bev , daily = FALSE , night.shift = F , sun = F , suncolor = "red" ,
                 timezone = "UTC" , long , lat){
  data <- data
  names(data)[names(data) == time] <- "timestamp"
  names(data)[names(data) == behaviour] <- "Verhalten"

  data$timestamp <- ymd_hms(data$timestamp)
  data$Datum <- as.Date(data$timestamp)

  if(sun == T){
    sunrise <- maptools::sunriset(crds = matrix(c(long,lat) , nrow = 1) ,
                                  dateTime = as_datetime(data$timestamp) ,
                                  POSIXct.out = T ,
                                  direction = "sunrise")$time

    sunset <- maptools::sunriset(crds = matrix(c(long,lat) , nrow = 1) ,
                                 dateTime = as_datetime(data$timestamp) ,
                                 POSIXct.out = T ,
                                 direction = "sunset")$time
  }

  data$Verhalten <- factor(data$Verhalten)
  data$color <- ifelse(data$Verhalten %in% target.bev , 1 , 0)
  data$color <- factor(data$color)
  data$Datum <- factor(as.character(data$Datum) , levels = unique(as.character(data$Datum)))
  data$date_numeric <- as.numeric(data$Datum)

  if(night.shift == T){
    data$Uhrzeit <- strftime(as_datetime(data$timestamp) + hours(12), format="%H:%M" , tz = timezone)
    if(sun == T){
      data$sunrise <- strftime(sunrise + hours(12), format="%H:%M" , tz = timezone)
      data$sunset <- strftime(sunset + hours(12), format="%H:%M" , tz = timezone)

      #match the sun times to timestamps that also exist in the animal data
      sunset.fit <- data$sunset
      for(i in 1:length(unique(data$sunset))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunset),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunset.fit[sunset.fit == unique(data$sunset)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunset <- sunset.fit

      sunrise.fit <- data$sunrise
      for(i in 1:length(unique(data$sunrise))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunrise),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunrise.fit[sunrise.fit == unique(data$sunrise)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunrise <- sunrise.fit
    }
  }

  else{
    data$Uhrzeit <- strftime(data$timestamp, format="%H:%M" , tz = timezone)
    if(sun == T){
      data$sunrise <- strftime(sunrise, format="%H:%M" , tz = timezone)
      data$sunset <- strftime(sunset, format="%H:%M" , tz = timezone)

      #match the sun times to timestamps that also exist in the animal data
      sunset.fit <- data$sunset
      for(i in 1:length(unique(data$sunset))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunset),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunset.fit[sunset.fit == unique(data$sunset)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunset <- sunset.fit

      sunrise.fit <- data$sunrise
      for(i in 1:length(unique(data$sunrise))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunrise),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunrise.fit[sunrise.fit == unique(data$sunrise)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunrise <- sunrise.fit
    }
  }

  unique.date <- c(TRUE , rep(NA,length(lubridate::month(lubridate::ymd(levels(data$Datum))))-1))

  for(i in 2:length(lubridate::month(lubridate::ymd(levels(data$Datum))))){
    unique.date[i] <- !(identical(lubridate::month(lubridate::ymd(levels(data$Datum)))[i],
                                  lubridate::month(lubridate::ymd(levels(data$Datum)))[i-1]))
  }

  basis <- ggplot()+
    geom_tile(data = data , aes(x = Uhrzeit , y = date_numeric , color = color) , size =1.1)+
    theme(legend.position = "none" ,axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
    ylab(label = "date")+
    xlab(label = "Time of the day")+
    theme(panel.background = element_blank())

  if(night.shift == T){
    if(daily != T & sun != T){
      basis+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique.date] , labels = levels(data$Datum)[unique.date])+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "15:00" , "18:00" , "21:00" , "00:00" , "03:00" , "06:00" , "09:00" , "" ))
    }
    else if(daily != T & sun == T){
      basis+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique.date] , labels = levels(data$Datum)[unique.date])+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "15:00" , "18:00" , "21:00" , "00:00" , "03:00" , "06:00" , "09:00" , "" ))+
        geom_path(data = data[data$sunrise %in% data$Uhrzeit,] , aes(x = sunrise , y = date_numeric ,
                                                                     color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        geom_path(data = data[data$sunset %in% data$Uhrzeit,] , aes(x = sunset , y = date_numeric ,
                                                                    color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white" , "sunrise/sunset" = suncolor))
    }
    else if(daily == T & sun == T){
      basis+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1) , labels = levels(data$Datum))+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "15:00" , "18:00" , "21:00" , "00:00" , "03:00" , "06:00" , "09:00" , "" ))+
        geom_path(data = data[data$sunrise %in% data$Uhrzeit,] , aes(x = sunrise , y = date_numeric ,
                                                                     color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        geom_path(data = data[data$sunset %in% data$Uhrzeit,] , aes(x = sunset , y = date_numeric ,
                                                                    color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white" , "sunrise/sunset" = suncolor))
    }
    else if(daily == T & sun != T){
      basis+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1) , labels = levels(data$Datum))+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "15:00" , "18:00" , "21:00" , "00:00" , "03:00" , "06:00" , "09:00" , "" ))
    }
  }

  else{
    if(daily == T & sun != T){
      basis+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "03:00" , "06:00" , "09:00" , "12:00" , "15:00" , "18:00" , "21:00" , "" ))+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1) , labels = levels(data$Datum))
    }
    else if(daily != T & sun != T){
      basis+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white"))+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "03:00" , "06:00" , "09:00" , "12:00" , "15:00" , "18:00" , "21:00" , "" ))+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique.date] , labels = levels(data$Datum)[unique.date])
    }
    else if(daily == T & sun == T){
      basis+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "03:00" , "06:00" , "09:00" , "12:00" , "15:00" , "18:00" , "21:00" , "" ))+
        geom_path(data = data[data$sunrise %in% data$Uhrzeit,] , aes(x = sunrise , y = date_numeric ,
                                                                     color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        geom_path(data = data[data$sunset %in% data$Uhrzeit,] , aes(x = sunset , y = date_numeric ,
                                                                    color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white" , "sunrise/sunset" = suncolor))+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1) , labels = levels(data$Datum))
    }
    else if(daily != T & sun == T){
      basis+
        scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                         labels = c("" , "03:00" , "06:00" , "09:00" , "12:00" , "15:00" , "18:00" , "21:00" , "" ))+
        geom_line(data = data[data$sunrise %in% data$Uhrzeit,] , aes(x = sunrise , y = date_numeric ,
                                                                     color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        geom_line(data = data[data$sunset %in% data$Uhrzeit,] , aes(x = sunset , y = date_numeric ,
                                                                    color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        scale_color_manual("Status", values = c("1" = "black" , "0" = "white" , "sunrise/sunset" = suncolor))+
        scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique.date] , labels = levels(data$Datum)[unique.date])
    }
  }
}
