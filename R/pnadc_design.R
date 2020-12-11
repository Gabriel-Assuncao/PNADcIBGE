#' Create PNADC survey object with its sample design
#' @description This function creates PNADC survey object with its sample design for analysis using \code{survey} package functions.
#' @import survey readr dplyr magrittr RCurl utils timeDate readxl tibble
#' @param data_pnadc A tibble of PNADC microdata read with \code{read_pnadc} function.
#' @return An object of class \code{survey.design} with the data from PNADC and its sample design.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNADcIBGE]{get_pnadc} for downloading, labelling, deflating and creating survey design object for PNADC microdata.\cr \link[PNADcIBGE]{read_pnadc} for reading PNADC microdata.\cr \link[PNADcIBGE]{pnadc_labeller} for labelling categorical variables from PNADC microdata.\cr \link[PNADcIBGE]{pnadc_deflator} for adding deflator variables to PNADC microdata.\cr \link[PNADcIBGE]{pnadc_example} for getting the path of the quarter PNADC example files.
#' @examples
#' # Using data read from disk
#' input_path <- pnadc_example(path="input_example.txt")
#' data_path <- pnadc_example(path="exampledata.txt")
#' dictionary.path <- pnadc_example(path="dictionaryexample.xls")
#' deflator.path <- pnadc_example(path="deflatorexample.xls")
#' pnadc.df <- read_pnadc(microdata=data_path, input_txt=input_path, vars="VD4002")
#' pnadc.df <- pnadc_labeller(data_pnadc=pnadc.df, dictionary.file=dictionary.path)
#' pnadc.df <- pnadc_deflator(data_pnadc=pnadc.df, deflator.file=deflator.path)
#' \donttest{
#' pnadc.svy <- pnadc_design(data_pnadc=pnadc.df)
#' # Calculating unemployment rate
#' survey::svymean(x=~VD4002, design=pnadc.svy, na.rm=TRUE)}
#' \donttest{
#' # Downloading data
#' pnadc.df2 <- get_pnadc(year=2017, quarter=4, vars="VD4002", defyear=2017, defperiod=4,
#'                       labels=TRUE, deflator=TRUE, design=FALSE, savedir=tempdir())
#' pnadc.svy2 <- pnadc_design(data_pnadc=pnadc.df2)
#' # Calculating unemployment rate
#' survey::svymean(x=~VD4002, design=pnadc.svy2, na.rm=TRUE)}
#' @export

pnadc_design <- function(data_pnadc) {
  if (sum(class(data_pnadc) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("UPA", "Estrato", "V1027", "V1029", "posest") %in% names(data_pnadc))) | 
        !(FALSE %in% (c("UPA", "Estrato", "V1030", "V1031", "posest") %in% names(data_pnadc)))) {
      options(survey.lonely.psu="adjust")
      if (!(FALSE %in% (c("UPA", "Estrato", "V1027", "V1029", "posest") %in% names(data_pnadc)))) {
        data_prior <- survey::svydesign(ids=~UPA, strata=~Estrato, data=data_pnadc, weights=~V1027, nest=TRUE)
        popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1029)))
        popc.types <- (popc.types[order(popc.types$posest),])
        data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)
      }
      else {
        data_prior <- survey::svydesign(ids=~UPA, strata=~Estrato, data=data_pnadc, weights=~V1031, nest=TRUE)
        popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1030)))
        popc.types <- (popc.types[order(popc.types$posest),])
        data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)
      }
    }
    else {
      warning("Weight variables required for sample design are missing.")
      data_posterior <- data_pnadc
    }
  }
  else {
    warning("Sample design was already defined for microdata, so applying another design is not possible.")
    data_posterior <- data_pnadc
  }
  return(data_posterior)
}