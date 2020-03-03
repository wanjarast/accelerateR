acto_odba <- function(data , time = "timestamp" , ODBA = "ODBA" , cutoff = 1 ,
                      night_shift = F , sun = F , suncolor = "red" ,
                      timezone = "UTC" , long , lat){
  data <- data
  names(data)[names(data) == time] <- "timestamp"
  names(data)[names(data) == ODBA] <- "ODBA"

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

  if(night_shift == T){
    data$Uhrzeit <- strftime(as_datetime(data$timestamp) + hours(12), format="%H:%M" , tz = timezone)
    if(sun == T){
      data$sunrise <- strftime(sunrise + hours(12), format="%H:%M" , tz = timezone)
      data$sunset <- strftime(sunset + hours(12), format="%H:%M" , tz = timezone)
      #match the sun times to timestamps that also exist in the animal data
      sunset_fit <- data$sunset
      for(i in 1:length(unique(data$sunset))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunset),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunset_fit[sunset_fit == unique(data$sunset)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunset <- sunset_fit

      sunrise_fit <- data$sunrise
      for(i in 1:length(unique(data$sunrise))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunrise),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunrise_fit[sunrise_fit == unique(data$sunrise)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunrise <- sunrise_fit
    }
  }

  else{
    data$Uhrzeit <- strftime(data$timestamp, format="%H:%M" , tz = timezone)
    if(sun == T){
      data$sunrise <- strftime(sunrise, format="%H:%M" , tz = timezone)
      data$sunset <- strftime(sunset, format="%H:%M" , tz = timezone)

      #match the sun times to timestamps that also exist in the animal data
      sunset_fit <- data$sunset
      for(i in 1:length(unique(data$sunset))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunset),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunset_fit[sunset_fit == unique(data$sunset)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunset <- sunset_fit

      sunrise_fit <- data$sunrise
      for(i in 1:length(unique(data$sunrise))){
        distance <- abs(as.numeric(hms::as.hms(paste(unique(data$sunrise),"00",sep = ":")))[i]-
                          as.numeric(hms::as.hms(paste(unique(data$Uhrzeit),"00",sep = ":"))))

        sunrise_fit[sunrise_fit == unique(data$sunrise)[i]] <- unique(data$Uhrzeit)[which.min(distance)]

      }

      data$sunrise <- sunrise_fit
    }
  }

  data$Datum <- factor(as.character(data$Datum) , levels = unique(as.character(data$Datum)))
  data$date_numeric <- as.numeric(data$Datum)

  data$ODBA[data$ODBA > quantile(x = data$ODBA , probs = cutoff)] <-
    quantile(x = data$ODBA , probs = cutoff)


  unique_date <- c(TRUE , rep(NA,length(lubridate::month(lubridate::ymd(levels(data$Datum))))-1))

  for(i in 2:length(lubridate::month(lubridate::ymd(levels(data$Datum))))){
    unique_date[i] <- !(identical(lubridate::month(lubridate::ymd(levels(data$Datum)))[i],
                                  lubridate::month(lubridate::ymd(levels(data$Datum)))[i-1]))
  }

  if(night_shift == T){
    shift <- ggplot(data = data)+
      geom_tile(aes(x = Uhrzeit , y = date_numeric , fill = ODBA) , size =1)+
      theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique_date] , labels = levels(data$Datum)[unique_date])+

      scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                       labels = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))])+
      scale_fill_gradient(low = "white", high = "black")+
      ylab(label = "date")+
      xlab(label = "Time of the day")+
      theme(panel.background = element_blank())
    if(sun == T){
      shift+
        geom_path(data = data[data$sunrise %in% data$Uhrzeit,] , aes(x = sunrise , y = date_numeric ,
                                                                     color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        geom_path(data = data[data$sunset %in% data$Uhrzeit,] , aes(x = sunset , y = date_numeric ,
                                                                    color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        scale_color_manual(name = "" , values = c("sunrise/sunset" = suncolor))
    }
    else{shift}
  }
  else{
    noshift <- ggplot(data = data)+
      geom_tile(aes(x = Uhrzeit , y = date_numeric , fill = ODBA) , size =1)+
      theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))+
      scale_y_reverse(breaks = seq(1 , length(levels(data$Datum)) , 1)[unique_date] , labels = levels(data$Datum)[unique_date])+
      scale_x_discrete(breaks = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))],
                       labels = sort(unique(data$Uhrzeit))[round(seq(1 , length(unique(data$Uhrzeit)) , length.out = 9))])+
      scale_fill_gradient(low = "white", high = "black")+
      ylab(label = "date")+
      xlab(label = "Time of the day")+
      theme(panel.background = element_blank())
    if(sun == T){
      noshift+
        geom_path(data = data[data$sunrise %in% data$Uhrzeit,] , aes(x = sunrise , y = date_numeric ,
                                                                     color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        geom_path(data = data[data$sunset %in% data$Uhrzeit,] , aes(x = sunset , y = date_numeric ,
                                                                    color = "sunrise/sunset" , group = 1),size = 1.5 , alpha = 0.7)+
        scale_color_manual(name = "" , values = c("sunrise/sunset" = suncolor))
    }
    else(noshift)
  }
}
