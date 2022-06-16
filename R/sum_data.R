sum_data = function(data,time,x=NULL,y=NULL,z=NULL,stats,behaviour=NULL,burstcount=NULL,windowstart=1,id=NULL,IntDur=NULL){
  # turn the data into a data table
  data <- data.table::setDT(data)
  data <- data.table::copy(data)

  if(!is.null(x)){
    data.table::setnames(data , x , "x")
  }
  if(!is.null(y)){
    data.table::setnames(data , y , "y")
  }
  if(!is.null(z)){
    data.table::setnames(data , z , "z")
  }

  results <- data[, data.table::first(time), by = time]
  results[, V1 := NULL]

  # when the calculation should not be done on the full burst
  if(!is.null(burstcount)){
    # check if the selected window start and the selected burstcount do not go outside the burstlength
    if(windowstart + burstcount - 1 > data[, .N , by = time][1,N]){
      warning("Window will run out of bounds of the burst. Reduce the burstcount or window staring row." , call. = F)
      stop()
    }

    # take a slice of each burst of the selected size
    data <- data[, .SD[windowstart:(windowstart + burstcount - 1)], by = time]
  }

  if(stats == "all"){
    stats <- c("mean","sd","max","min","range","cov","cor","meandiff","sddiff","mdocp","sdocp",
               "Var","q","Pitch","Roll","Yaw","ICV","CV","Kurtosis","Skewness",
               "ODBA")
  }

  # calculate the mean
  if("mean" %in% stats){
    if(!is.null(x)){
      results[ , meanx := data[ , mean(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , meany := data[ , mean(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , meanz := data[ , mean(z) , by = time][,V1]]
    }
  }

  # calculate the standart deviation
  if("sd" %in% stats){
    if(!is.null(x)){
      results[ , sdx := data[ , sd(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , sdy := data[ , sd(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , sdz := data[ , sd(z) , by = time][,V1]]
    }
  }

  # calculate the maximum value
  if("max" %in% stats){
    if(!is.null(x)){
      results[ , maxx := data[ , max(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , maxy := data[ , max(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , maxz := data[ , max(z) , by = time][,V1]]
    }
  }

  # calculate the minimum value
  if("min" %in% stats){
    if(!is.null(x)){
      results[ , minx := data[ , min(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , miny := data[ , min(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , minz := data[ , min(z) , by = time][,V1]]
    }
  }

  # calculate the range
  if("range" %in% stats){
    if(!is.null(x)){
      results[ , rangex := data[ , max(x) - min(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , rangey := data[ , max(y) - min(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , rangez := data[ , max(z) - min(z) , by = time][,V1]]
    }
  }

  # calculate the corvariance between two axes - uses pearson by default
  if("cov" %in% stats){
    if(!is.null(x) & !is.null(y)){
      results[ , covxy := data[ , cov(x, y) , by = time][,V1]]
    }
    if(!is.null(x) & !is.null(z)){
      results[ , covxz := data[ , cov(x, z) , by = time][,V1]]
    }
    if(!is.null(y) & !is.null(z)){
      results[ , covyz := data[ , cov(y, z) , by = time][,V1]]
    }
  }

  # calculate the correlation between two axes - uses pearson by default
  if("cor" %in% stats){
    if(!is.null(x) & !is.null(y)){
      results[ , corxy := data[ , cor(x, y) , by = time][,V1]]
    }
    if(!is.null(x) & !is.null(z)){
      results[ , corxz := data[ , cor(x, z) , by = time][,V1]]
    }
    if(!is.null(y) & !is.null(z)){
      results[ , coryz := data[ , cor(y, z) , by = time][,V1]]
    }
  }

  # calculate the mean difference between two axes
  if("meandiff" %in% stats){
    if(!is.null(x) & !is.null(y)){
      results[ , meandxy := data[ , mean(x - y) , by = time][,V1]]
    }
    if(!is.null(x) & !is.null(z)){
      results[ , meandxz := data[ , mean(x - z) , by = time][,V1]]
    }
    if(!is.null(y) & !is.null(z)){
      results[ , meandyz := data[ , mean(y - z) , by = time][,V1]]
    }
  }

  # calculate the standard deviation between the difference of two axes
  if("sddiff" %in% stats){
    if(!is.null(x) & !is.null(y)){
      results[ , sddxy := data[ , sd(x - y) , by = time][,V1]]
    }
    if(!is.null(x) & !is.null(z)){
      results[ , sddxz := data[ , sd(x - z) , by = time][,V1]]
    }
    if(!is.null(y) & !is.null(z)){
      results[ , sddyz := data[ , sd(y - z) , by = time][,V1]]
    }
  }

  # calculate the mean difference of continues points
  if("mdocp" %in% stats){
    if(!is.null(x)){
      results[ , mdocpx := data[ , mean(x[-1] - x[-length(x)]) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , mdocpy := data[ , mean(y[-1] - y[-length(y)]) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , mdocpz := data[ , mean(z[-1] - z[-length(z)]) , by = time][,V1]]
    }
  }

  # calculate the standard deviation of the difference of continues points
  if("sdocp" %in% stats){
    if(!is.null(x)){
      results[ , sdocpx := data[ , sd(x[-1] - x[-length(x)]) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , sdocpy := data[ , sd(y[-1] - y[-length(y)]) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , sdocpz := data[ , sd(z[-1] - z[-length(z)]) , by = time][,V1]]
    }
  }

  # calculate the weighted mean
  if("Wmean" %in% stats){
    wmean <- function(axis){
      ac = acf(axis,lag.max=length(axis),plot=FALSE)
      ps = Mod(fft(ac$acf))
      pshalf = ps[2:((length(ps))/2)]-0.5
      psn = c(pshalf*100/sum(pshalf),0)
      p = IntDur/(1:length(psn))
      wm = weighted.mean(p,psn)
      return(wm)
    }
    if(!is.null(x)){
      results[ , wmeanx := data[ , wmean(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , wmeany := data[ , wmean(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , wmeanz := data[ , wmean(z) , by = time][,V1]]
    }
  }

  # calculate the variance with 1/N instead of 1/(N-1)
  if("Var" %in% stats){
    variance <- function(axis){
      result <- var(axis)*(length(axis)-1)/length(axis)
      return(result)
    }

    if(!is.null(x)){
      results[ , varx := data[ , variance(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , vary := data[ , variance(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , varz := data[ , variance(z) , by = time][,V1]]
    }
  }

  # calculate the sum of squares of all axes aka euclidean norm
  if("q" %in% stats & !is.null(y) & !is.null(z)){
    results[ , q := data[ , sqrt(mean(x)^2 + mean(y)^2 + mean(z)^2) , by = time][,V1]]
  }

  if("Pitch" %in% stats & !is.null(y) & !is.null(z)){
    results[ , Pitch := data[ , atan2(mean(y) , (sqrt(mean(x)^2+mean(z)^2)))*(180/pi) , by = time][,V1]]
  }

  if("Roll" %in% stats & !is.null(y) & !is.null(z)){
    results[ , Roll := data[ , atan2(mean(x) , (sqrt(mean(y)^2+mean(z)^2)))*(180/pi) , by = time][,V1]]
  }

  if("Yaw" %in% stats & !is.null(y) & !is.null(z)){
    results[ , Yaw := data[ , atan2(mean(z) , (sqrt(mean(x)^2+mean(y)^2)))*(180/pi) , by = time][,V1]]
  }

  if("ICV" %in% stats){
    if(!is.null(x)){
      results[ , ICVx := data[ , mean(x)/sd(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , ICVy := data[ , mean(y)/sd(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , ICVz := data[ , mean(z)/sd(z) , by = time][,V1]]
    }
  }

  if("CV" %in% stats){
    if(!is.null(x)){
      results[ , CVx := data[ , sd(x)/mean(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , CVy := data[ , sd(y)/mean(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , CVz := data[ , sd(z)/mean(z) , by = time][,V1]]
    }
  }

  if("Kurtosis" %in% stats){
    if(!is.null(x)){
      results[ , Kurtosisx := data[ , e1071::kurtosis(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , Kurtosisy := data[ , e1071::kurtosis(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , Kurtosisz := data[ , e1071::kurtosis(z) , by = time][,V1]]
    }
  }

  if("Skewness" %in% stats){
    if(!is.null(x)){
      results[ , Skewnessx := data[ , e1071::skewness(x) , by = time][,V1]]
    }
    if(!is.null(y)){
      results[ , Skewnessy := data[ , e1071::skewness(y) , by = time][,V1]]
    }
    if(!is.null(z)){
      results[ , Skewnessz := data[ , e1071::skewness(z) , by = time][,V1]]
    }
  }

  if("ODBA" %in% stats & !is.null(y) & !is.null(z)){
    results[ , ODBA := data[ , sum(abs(x-mean(x)),abs(y-mean(y)),abs(z-mean(z))) , by = time][,V1]]
  }

  if("FFT" %in% stats){
    fastf = function(axis){
      output <- Mod(fft(axis)/length(axis))
      output <- output[1:(length(axis)%/%2)]
      return(output)
    }


    if(!is.null(x)){
      xnames <- paste("x",c(1:(burstcount %/% 2)) , sep=".")
      fft_datax <- data[ , transpose(list(fastf(x))) , by = time]
      old_namesx <- names(fft_datax)[-1]
      setnames(fft_datax, old_namesx, xnames)
      results <- merge(results , fft_datax, by = time, all = TRUE)
    }


    if(!is.null(y)){
      ynames <- paste("y",c(1:(burstcount %/% 2)) , sep=".")
      fft_datay <- data[ , transpose(list(fastf(y))) , by = time]
      old_namesy <- names(fft_datay)[-1]
      setnames(fft_datay, old_namesy, ynames)
      results <- merge(results , fft_datay, by = time, all = TRUE)
    }


    if(!is.null(z)){
      znames <- paste("z",c(1:(burstcount %/% 2)) , sep=".")
      fft_dataz <- data[ , transpose(list(fastf(z))) , by = time]
      old_namesz <- names(fft_dataz)[-1]
      setnames(fft_dataz, old_namesz, znames)
      results <- merge(results , fft_dataz, by = time, all = TRUE)
    }
  }

  if(!is.null(behaviour)){
    data.table::setnames(data , behaviour , "behaviour_in_function")
    results[ , behaviour_in_function := data[ , head(behaviour_in_function , 1) , by = time][,V1]]
    data.table::setnames(results , "behaviour_in_function" , behaviour)
  }

  if(!is.null(id)){
    data.table::setnames(data , id , "id_in_function")
    results[ , id_in_function := data[ , head(id_in_function , 1) , by = time][,V1]]
    data.table::setnames(results , "id_in_function" , id)
  }

  # Check if any columns produced infinit values
  inf_values <- lapply(results, function(x){any(is.infinite(x))})

  inf_columns <- names(inf_values[inf_values == T])

  if(length(inf_columns > 0)){
    message(paste("These columns hold infinit values:", paste(inf_columns, collapse = ", "), sep = " "))
  }

  return(results)
}
