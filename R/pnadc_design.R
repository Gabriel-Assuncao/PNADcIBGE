#' Create pnadc survey object utilizing its sampling design for analysis with survey package
#'
#' @import survey readr dplyr magrittr
#' @param data_pnadc A tibble of PNADc data read with \code{read_pnadc} function.
#' @return An object of class \code{survey.design} with the data from PNADc survey and its sample design.
#' @examples
#'
#' #Using data read from disk
#' input_path <- pnadc_example("input_example.txt")
#' data_path <- pnadc_example("exampledata.txt")
#' pnadc.df <- read_pnadc(data_path, input_path, vars="VD4002")
#' dictionary.path <- pnadc_example("dictionaryexample.xls")
#' pnadc.df <- pnadc_labeller(pnadc.df,dictionary.path)
#' \dontrun{
#' pnadc.svy <- pnadc_design(pnadc.df)
#' #Calculating unemployment rate
#' survey::svymean(~VD4002, pnadc.svy, na.rm=TRUE)}
#'
#' #Downloading data
#' \dontrun{
#' pnadc.df2<- get_pnadc(2,2017,vars="VD4002")
#' pnadc.df2 <- pnadc_labeller(pnadc.df2,dictionary.path)
#' pnadc.svy2 <- pnadc_design(pnadc.df2)
#' #Calculating unemployment rate
#' survey::svymean(~VD4002, pnadc.svy2, na.rm=TRUE)}
#' @export

pnadc_design <- function(data_pnadc){
  if(sum(class(data_pnadc) == "tbl_df") > 0){
    options(survey.lonely.psu="adjust")
    if("V1027" %in% names(data_pnadc)){
      ########## creating desing object w/o poststratification
      data_pre <- survey::svydesign(ids     = ~UPA,
                          strata  = ~Estrato,
                          data    = data_pnadc,
                          weights = ~V1027,
                          nest    = T)
      ########## defining total for poststratification
      popc.types <- data.frame(posest = as.character(unique(data_pnadc$posest)),
                             Freq   = as.numeric(unique(data_pnadc$V1029)))
      popc.types <- (popc.types[order(popc.types$posest),])
      ########## creating final desing object w/ poststratification
      data_pos <- survey::postStratify(design     = data_pre,
                             strata     = ~posest,
                             population = popc.types)
    }
    else{
      ########## creating desing object w/o poststratification
      data_pre <- survey::svydesign(ids     = ~UPA,
                                    strata  = ~Estrato,
                                    data    = data_pnadc,
                                    weights = ~V1031,
                                    nest    = T)
      ########## defining total for poststratification
      popc.types <- data.frame(posest = as.character(unique(data_pnadc$posest)),
                               Freq   = as.numeric(unique(data_pnadc$V1030)))
      popc.types <- (popc.types[order(popc.types$posest),])
      ########## creating final desing object w/ poststratification
      data_pos <- survey::postStratify(design     = data_pre,
                                       strata     = ~posest,
                                       population = popc.types)
  
    }
  }
  else{
    warning("The sample design was already defined for this microdata.")
    data_pos <- data_pnadc
  }
  return(data_pos)
}
