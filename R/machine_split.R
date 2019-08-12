machine_split2 <- function(data , group , train.size , val.size , seed = 142 , names = c("train.data","val.data","test.data")){
  set.seed(seed)
  if(train.size + val.size > 1){
    stop("train.size + val.size have to be smaller than 1")
  }

  data%>%
    split(. , .[[group]]) -> splited.data

  splited.data[sample(length(unique(data[[group]])) * train.size)]%>%
    data.table::rbindlist(.)%>%
    as.data.frame(.) -> train.data

  remaining <- data[!(data[[group]] %in% train.data[[group]]) , ]

  remaining%>%
    split(. , .[[group]]) -> splited.remaining.data

  val.size.calc <- (nrow(data) * val.size) /
    (nrow(data) - train.size*nrow(data))

  splited.remaining.data[sample(length(unique(remaining[[group]])) *
                                  val.size.calc)]%>%
    data.table::rbindlist(.)%>%
    as.data.frame(.) -> val.data
  if(train.size + val.size < 1){
    test.data <- data[!(data[[group]] %in% train.data[[group]]) &
                        !(data[[group]] %in% val.data[[group]]) , ]%>%
      as.data.frame(.)
    assign(names[3] , test.data , envir = .GlobalEnv)
  }

  assign(names[1] , train.data , envir = .GlobalEnv)
  assign(names[2] , val.data , envir = .GlobalEnv)
}
