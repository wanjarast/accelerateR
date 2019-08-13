machine.split <- function(data , group = "timestamp" , behaviour , train.size , val.size , seed = 142 , names = c("train.data","val.data","test.data")){
  set.seed(seed)
  if(train.size + val.size > 1){
    stop("train.size + val.size have to be smaller than 1")
  }
  data%>%
    dplyr::group_by_(. , behaviour , group)%>%
    tidyr::nest(.)%>%
    dplyr::ungroup(.)%>%
    dplyr::group_by_(. , behaviour) -> nested.data

  nested.data%>%
    dplyr::sample_frac( . , size = train.size)%>%
    dplyr::ungroup(.)%>%
    dplyr::slice(. , sample(nrow(.)))%>%
    tidyr::unnest(.)-> train.data

  remaining <- nested.data[!nested.data[[group]] %in% train.data[[group]], ]

  val.size.calc <- (nrow(data) * val.size) /
    (nrow(data) - train.size*nrow(data))

  remaining%>%
    dplyr::sample_frac( . , size = val.size.calc)%>%
    dplyr::ungroup(.)%>%
    dplyr::slice(. , sample(nrow(.)))%>%
    tidyr::unnest(.)-> val.data

  if(train.size + val.size < 1){
    remaining[!(remaining[[group]] %in% val.data[[group]]), ]%>%
      dplyr::ungroup(.)%>%
      dplyr::slice(. , sample(nrow(.)))%>%
      tidyr::unnest(.)-> test.data

    assign(names[3] , test.data , envir = .GlobalEnv)
  }

  assign(names[1] , train.data , envir = .GlobalEnv)
  assign(names[2] , val.data , envir = .GlobalEnv)
}
