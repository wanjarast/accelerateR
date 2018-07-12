sum.data = function(data,IntDur=NA,time,x,y=NULL,z=NULL,ID=NA,Tag.ID=NA,frequency=NA,sex=NA,stats){
  data <- group_by_(data,time)
  burst.timestamp = summarise_(data, meanx = interp(~mean(v),v=as.name(x)))[,1]
  if("mean" %in% stats){
    meanx = summarise_(data, meanx = interp(~mean(v),v=as.name(x)))[,2]
    if(!is.null(y)){
      meany = summarise_(data, meany = interp(~mean(u),u=as.name(y)))[,2]}
    else{meany=NA}
    if(!is.null(z)){
      meanz = summarise_(data, meanz = interp(~mean(w),w=as.name(z)))[,2]}
    else{meanz=NA}
  }
  else{
    meanx=NA
    meany=NA
    meanz=NA
  }
  if("sd" %in% stats){
    sdx = summarise_(data, sdx = interp(~sd(v),v=as.name(x)))[,2]
    if(!is.null(y)){
      sdy = summarise_(data, sdy = interp(~sd(u),u=as.name(y)))[,2]}
    else(sdy=NA)
    if(!is.null(z)){
      sdz = summarise_(data, sdz = interp(~sd(w),w=as.name(z)))[,2]}
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
    wmx = summarise_(data, wmx = interp(~wmean(v),v=as.name(x)))[,2]
    if(!is.null(y)){
      wmy = summarise_(data, wmy = interp(~wmean(u),u=as.name(y)))[,2]}
    else(wmy=NA)
    if(!is.null(z)){
      wmz = summarise_(data, wmz = interp(~wmean(w),w=as.name(z)))[,2]}
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
    Varx = summarise_(data ,varx = interp(~variance(v),v=as.name(x)))[,2]
    if(!is.null(y)){
      Vary = summarise_(data ,vary = interp(~variance(u),u=as.name(y)))[,2]}
    else(Vary=NA)
    if(!is.null(z)){
      Varz = summarise_(data ,varz = interp(~variance(w),w=as.name(z)))[,2]}
    else(Varz=NA)
  }
  else{
    Varx=NA
    Vary=NA
    Varz=NA
  }
  if("q" %in% stats & !is.null(y) & !is.null(z)){
    dasq = summarise_(data, q = interp(~sqrt(mean(v)^2 + mean(u)^2 + mean(w)^2) , .values = list(v=as.name(x),u=as.name(y),w=as.name(z))))[,2]
  }
  else{
    dasq=NA
  }
  if("Pitch" %in% stats & !is.null(y) & !is.null(z)){
    Pitch = summarise_(data, Pitch = interp(~atan(mean(u)/(sqrt(mean(v)^2+mean(w)^2)))*(180/pi), .values = list(v=as.name(x),u=as.name(y),w=as.name(z))))[,2]
  }
  else{
    Pitch=NA
  }
  if("Roll" %in% stats & !is.null(y) & !is.null(z)){
    Roll = summarise_(data, Roll = interp(~atan(mean(v)/(sqrt(mean(u)^2+mean(w)^2)))*(180/pi), .values = list(v=as.name(x),u=as.name(y),w=as.name(z))))[,2]
  }
  else{
    Roll=NA
  }
  if("ICV" %in% stats){
    ICVx = summarise_(data, ICVx = interp(~mean(v)/sd(v),v=as.name(x)))[,2]
    if(!is.null(y)){
      ICVy = summarise_(data, ICVy = interp(~mean(u)/sd(u),u=as.name(y)))[,2]}
    else(ICVy=NA)
    if(!is.null(z)){
      ICVz = summarise_(data, ICVz = interp(~mean(w)/sd(w),w=as.name(z)))[,2]}
    else(ICVz=NA)
  }
  else{
    ICVx=NA
    ICVy=NA
    ICVz=NA
  }
  if("Kurtosis" %in% stats){
    Kurtosisx = summarise_(data, Kurtosisx = interp(~kurtosis(v),v=as.name(x)))[,2]
    if(!is.null(y)){
      Kurtosisy = summarise_(data, Kurtosisy = interp(~kurtosis(u),u=as.name(y)))[,2]}
    else(Kurtosisy=NA)
    if(!is.null(z)){
      Kurtosisz = summarise_(data, Kurtosisz = interp(~kurtosis(w),w=as.name(z)))[,2]}
    else(Kurtosisz=NA)
  }
  else{
    Kurtosisx = NA
    Kurtosisy = NA
    Kurtosisz = NA
  }
  if("Skewness" %in% stats){
    Skewnessx = summarise_(data, Skewnessx = interp(~skewness(v),v=as.name(x)))[,2]
    if(!is.null(y)){
      Skewnessy = summarise_(data, Skewnessy = interp(~skewness(u),u=as.name(y)))[,2]}
    else(Skewnessy=NA)
    if(!is.null(z)){
      Skewnessz = summarise_(data, Skewnessz = interp(~skewness(w),w=as.name(z)))[,2]}
    else(Skewnessz=NA)
  }
  else{
    Skewnessx = NA
    Skewnessy = NA
    Skewnessz = NA
  }
  if("ODBA" %in% stats & !is.null(y) & !is.null(z)){
    ODBA = summarise_(data, ODBA = interp(~sum(abs(v-mean(v)),abs(u-mean(u)),abs(w-mean(w))), .values = list(v=as.name(x),u=as.name(y),w=as.name(z))))[,2]
  }
  else{
    ODBA=NA
  }

  df <- data.frame(burst.timestamp,meanx,meany,meanz,sdx,sdy,sdz,Varx,Vary,Varz,wmx,wmy,wmz,ICVx,ICVy,ICVz,dasq,Kurtosisx,
                   Kurtosisy,Kurtosisz,Skewnessx,Skewnessy,Skewnessz,Pitch,Roll,ODBA,ID,Tag.ID,frequency,sex)
  df_clear = df[colSums(!is.na(df)) > 0]
  return(df_clear)
}
