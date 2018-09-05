move.trans <- function(data,time="timestamp",burst,naxes=3){
  names(data) <- gsub(pattern = ":" , replacement = "." , x = names(data))
  names(data) <- gsub(pattern = "-" , replacement = "." , x = names(data))
  if(naxes == 3){
  reorder.acc <- function(rows){
    df <-data.frame(x=rep(NA,nrow(rows)*burst),y=NA,z=NA,taxon_name=NA,tag_id=NA,individual_id=NA,study_name=NA)
    V=matrix(sapply(unlist(strsplit(as.character(rows$eobs.accelerations.raw)," ")),as.numeric),byrow=T,ncol=3)
    df$x <- V[,1]
    df$y <- V[,2]
    df$z <- V[,3]
    df$taxon_name <- rows$individual.taxon.canonical.name
    df$tag_id <- rows$tag.local.identifier
    df$individual_id <- rows$individual.local.identifier
    df$study_name <- rows$study.name
    return(df)
  }
  output <- data %>%
    group_by_(time) %>%
    do(reorder.acc(.))
  return(output)
  }
  if(naxes == 2){
    reorder.acc <- function(rows){
      df <-data.frame(x=rep(NA,nrow(rows)*burst),y=NA,taxon_name=NA,tag_id=NA,individual_id=NA,study_name=NA)
      V=matrix(sapply(unlist(strsplit(as.character(rows$eobs.accelerations.raw)," ")),as.numeric),byrow=T,ncol=2)
      df$x <- V[,1]
      df$y <- V[,2]
      df$taxon_name <- rows$individual.taxon.canonical.name
      df$tag_id <- rows$tag.local.identifier
      df$individual_id <- rows$individual.local.identifier
      df$study_name <- rows$study.name
      return(df)
    }
    output <- data %>%
      group_by_(time) %>%
      do(reorder.acc(.))
    return(output)
  }
  if(naxes == 1){
    reorder.acc <- function(rows){
      df <-data.frame(x=rep(NA,nrow(rows)*burst),taxon_name=NA,tag_id=NA,individual_id=NA,study_name=NA)
      V=matrix(sapply(unlist(strsplit(as.character(rows$eobs.accelerations.raw)," ")),as.numeric),byrow=T,ncol=1)
      df$x <- V[,1]
      df$taxon_name <- rows$individual.taxon.canonical.name
      df$tag_id <- rows$tag.local.identifier
      df$individual_id <- rows$individual.local.identifier
      df$study_name <- rows$study.name
      return(df)
    }
    output <- data %>%
      group_by_(time) %>%
      do(reorder.acc(.))
    return(output)
  }
}
