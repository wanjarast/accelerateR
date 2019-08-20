sum_data = function(data,time,x,y=NULL,z=NULL,stats,behaviour=NULL,burstcount=NULL,windowstart=1,ID=NA,TAG_ID=NA,sex=NA,IntDur=NULL){

  names(data)[names(data)==time] <- "timestamp"

  if(is.null(burstcount)){
    data <- dplyr::group_by(data,timestamp)
  }

  if(!is.null(burstcount)){

    if(burstcount == data %>% dplyr::group_by(. , timestamp) %>% dplyr::summarise(dplyr::n()) %>% dplyr::select(.,2) %>% dplyr::slice(.,1) &
       windowstart == 1){
      data <- dplyr::group_by(data,timestamp)
    }
    else{
      if(windowstart > data %>% dplyr::group_by(. , timestamp) %>% dplyr::summarise(dplyr::n()) %>% dplyr::select(.,2) %>% dplyr::slice(.,1)-burstcount+1){
        warning("Window will run out of bounds of the burst. Reduce the burstcount or window staring row." , call. = F)
        stop()
      }
      data <- dplyr::group_by(data,timestamp) %>%
        dplyr::slice(. , windowstart:(windowstart + burstcount - 1))
    }
  }
  names(data)[names(data)==x] <- "x"
  names(data)[names(data)==y] <- "y"
  names(data)[names(data)==z] <- "z"

  burst_timestamp = dplyr::summarise(data, meanx = mean(x))[,1]
  if("mean" %in% stats){
    meanx = dplyr::summarise(data, meanx = mean(x))[,2]
    if(!is.null(y)){
      meany = dplyr::summarise(data, meany = mean(y))[,2]}
    else{meany=NA}
    if(!is.null(z)){
      meanz = dplyr::summarise(data, meanz = mean(z))[,2]}
    else{meanz=NA}
  }
  else{
    meanx=NA
    meany=NA
    meanz=NA
  }
  if("sd" %in% stats){
    sdx = dplyr::summarise(data, sdx = sd(x))[,2]
    if(!is.null(y)){
      sdy = dplyr::summarise(data, sdy = sd(y))[,2]}
    else(sdy=NA)
    if(!is.null(z)){
      sdz = dplyr::summarise(data, sdz = sd(z))[,2]}
    else(sdz=NA)
  }
  else{
    sdx=NA
    sdy=NA
    sdz=NA
  }
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
    wmx = dplyr::summarise(data, wmx = wmean(x))[,2]
    if(!is.null(y)){
      wmy = dplyr::summarise(data, wmy = wmean(y))[,2]}
    else(wmy=NA)
    if(!is.null(z)){
      wmz = dplyr::summarise(data, wmz = wmean(z))[,2]}
    else(wmz=NA)
  }
  else{
    wmx=NA
    wmy=NA
    wmz=NA
  }
  if("Var" %in% stats){
    variance <- function(axis){
      result <- var(axis)*(length(axis)-1)/length(axis)
      return(result)
    }
    Varx = dplyr::summarise(data ,varx = variance(x))[,2]
    if(!is.null(y)){
      Vary = dplyr::summarise(data ,vary = variance(y))[,2]}
    else(Vary=NA)
    if(!is.null(z)){
      Varz = dplyr::summarise(data ,varz = variance(z))[,2]}
    else(Varz=NA)
  }
  else{
    Varx=NA
    Vary=NA
    Varz=NA
  }
  if("q" %in% stats & !is.null(y) & !is.null(z)){
    dasq = dplyr::summarise(data, q = sqrt(mean(x)^2 + mean(y)^2 + mean(z)^2))[,2]
  }
  else{
    dasq=NA
  }
  if("Pitch" %in% stats & !is.null(y) & !is.null(z)){
    Pitch = dplyr::summarise(data, Pitch = atan2(mean(y) , (sqrt(mean(x)^2+mean(z)^2)))*(180/pi))[,2]
  }
  else{
    Pitch=NA
  }
  if("Roll" %in% stats & !is.null(y) & !is.null(z)){
    Roll = dplyr::summarise(data, Roll = atan2(mean(x) , (sqrt(mean(y)^2+mean(z)^2)))*(180/pi))[,2]
  }
  else{
    Roll=NA
  }
  if("ICV" %in% stats){
    ICVx = dplyr::summarise(data, ICVx = mean(x)/sd(x))[,2]
    if(!is.null(y)){
      ICVy = dplyr::summarise(data, ICVy = mean(y)/sd(y))[,2]}
    else(ICVy=NA)
    if(!is.null(z)){
      ICVz = dplyr::summarise(data, ICVz = mean(z)/sd(z))[,2]}
    else(ICVz=NA)
  }
  else{
    ICVx=NA
    ICVy=NA
    ICVz=NA
  }
  if("CV" %in% stats){
    CVx = dplyr::summarise(data, CVx = sd(x)/mean(x))[,2]
    if(!is.null(y)){
      CVy = dplyr::summarise(data, CVy = sd(y)/mean(y))[,2]}
    else(CVy=NA)
    if(!is.null(z)){
      CVz = dplyr::summarise(data, CVz = sd(z)/mean(z))[,2]}
    else(CVz=NA)
  }
  else{
    CVx=NA
    CVy=NA
    CVz=NA
  }
  if("Kurtosis" %in% stats){
    Kurtosisx = dplyr::summarise(data, Kurtosisx = kurtosis(x))[,2]
    if(!is.null(y)){
      Kurtosisy = dplyr::summarise(data, Kurtosisy = kurtosis(y))[,2]}
    else(Kurtosisy=NA)
    if(!is.null(z)){
      Kurtosisz = dplyr::summarise(data, Kurtosisz = kurtosis(z))[,2]}
    else(Kurtosisz=NA)
  }
  else{
    Kurtosisx = NA
    Kurtosisy = NA
    Kurtosisz = NA
  }
  if("Skewness" %in% stats){
    Skewnessx = dplyr::summarise(data, Skewnessx = skewness(x))[,2]
    if(!is.null(y)){
      Skewnessy = dplyr::summarise(data, Skewnessy = skewness(y))[,2]}
    else(Skewnessy=NA)
    if(!is.null(z)){
      Skewnessz = dplyr::summarise(data, Skewnessz = skewness(z))[,2]}
    else(Skewnessz=NA)
  }
  else{
    Skewnessx = NA
    Skewnessy = NA
    Skewnessz = NA
  }
  if("ODBA" %in% stats & !is.null(y) & !is.null(z)){
    ODBA = dplyr::summarise(data, ODBA = sum(abs(x-mean(x)),abs(y-mean(y)),abs(z-mean(z))))[,2]
  }
  else{
    ODBA = NA
  }
  if("FFT" %in% stats){
    if(!is.null(y) & !is.null(z)){
      names_for_data_frame <- c("timestamp" , paste("x",c(1:burstcount) , sep=".") ,
                                paste("y",c(1:burstcount) ,sep="."),paste("z",c(1:burstcount),sep="."))
      fastf = function(fragment) {
        result <- Mod(fft(fragment$x)/length(fragment$x))
        result2 <- Mod(fft(fragment$y)/length(fragment$y))
        result3 <- Mod(fft(fragment$z)/length(fragment$z))
        results <- c(result,result2,result3)
        final <- as.data.frame(matrix(results,ncol = length(results)))
        return(final)
      }

      fastFT = data %>%
        dplyr::do(fastf(.)) %>%
        dplyr::rename_all(funs(c(names_for_data_frame)))
      fastFT = fastFT[, -1]
    }

    if(is.null(z) & is.null(y)){
      names_for_data_frame <- c("timestamp" , paste("x",c(1:burstcount) , sep="."))

      fastf = function(fragment) {
        results <- Mod(fft(fragment$x)/length(fragment$x))
        final <- as.data.frame(matrix(results,ncol = length(results)))
        return(final)
      }

      fastFT = data %>%
        dplyr::do(fastf(.)) %>%
        dplyr::rename_all(funs(c(names_for_data_frame)))
      fastFT = fastFT[, -1]
    }

    if(is.null(z) & !is.null(y)){
      names_for_data_frame <- c("timestamp" , paste("x",c(1:burstcount) , sep=".") ,
                                paste("y",c(1:burstcount) ,sep="."))
      fastf = function(fragment) {
        result <- Mod(fft(fragment$x)/length(fragment$x))
        result2 <- Mod(fft(fragment$y)/length(fragment$y))
        results <- c(result,result2)
        final <- as.data.frame(matrix(results,ncol = length(results)))
        return(final)
      }

      fastFT = data %>%
        dplyr::do(fastf(.)) %>%
        dplyr::rename_all(funs(c(names_for_data_frame)))
      fastFT = fastFT[, -1]
    }
  }
  else{
    fastFT = NA
  }

  if(!is.null(behaviour)){
    names(data)[names(data)==behaviour] <- "bev"
    bev = dplyr::summarise(data, bev = first(bev))[,2]
  }
  else{
    bev=NA
  }


  df <- data.frame(burst_timestamp,meanx,meany,meanz,sdx,sdy,sdz,Varx,Vary,Varz,wmx,wmy,wmz,CVx,CVy,CVz,ICVx,ICVy,ICVz,dasq,Kurtosisx,
                   Kurtosisy,Kurtosisz,Skewnessx,Skewnessy,Skewnessz,Pitch,Roll,ODBA,fastFT,ID,TAG_ID,sex,bev)
  df_clear = df[colSums(!is.na(df)) > 0]
  names(df_clear)[names(df_clear)=="bev"] <- behaviour

  return(df_clear)
}
