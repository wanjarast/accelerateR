machine_split <- function(data , group = "timestamp" , behaviour , train_size , val_size ,
                          seed = 142 , names = c("train_data","val_data","test_data")){
  set.seed(seed)
  if(train_size + val_size > 1){
    stop("train_size + val_size have to be smaller than 1")
  }
  nested_data <- data%>%
    dplyr::group_by_(. , behaviour , group)%>%
    tidyr::nest(.)%>%
    dplyr::ungroup(.)%>%
    dplyr::group_by_(. , behaviour)

  train_data <- nested_data%>%
    dplyr::sample_frac( . , size = train_size)%>%
    dplyr::ungroup(.)%>%
    dplyr::slice(. , sample(nrow(.)))%>%
    tidyr::unnest(.)

  remaining <- nested_data[!nested_data[[group]] %in% train_data[[group]], ]

  val_size_calc <- (nrow(data) * val_size) /
    (nrow(data) - train_size*nrow(data))

  val_data <- remaining%>%
    dplyr::sample_frac( . , size = val_size_calc)%>%
    dplyr::ungroup(.)%>%
    dplyr::slice(. , sample(nrow(.)))%>%
    tidyr::unnest(.)

  if(train_size + val_size < 1){
    test_data <- remaining[!(remaining[[group]] %in% val_data[[group]]), ]%>%
      dplyr::ungroup(.)%>%
      dplyr::slice(. , sample(nrow(.)))%>%
      tidyr::unnest(.)

    assign(names[3] , test_data , envir = .GlobalEnv)
  }

  assign(names[1] , train_data , envir = .GlobalEnv)
  assign(names[2] , val_data , envir = .GlobalEnv)
}
