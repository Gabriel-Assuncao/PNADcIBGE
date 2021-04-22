#' Get the path of the quarter PNADC example files
#' @description This function provides the path of the microdata from quarter 4 of year 2017 of the PNADC example files, loaded with this package.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param path Name of file. If \code{NULL}, the quarter PNADC example files names will be listed.
#' @return A vector with names of all the available quarter PNADC example files or the path for specific requested quarter PNADC example file.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNADcIBGE]{get_pnadc} for downloading, labelling, deflating and creating survey design object for PNADC microdata.\cr \link[PNADcIBGE]{read_pnadc} for reading PNADC microdata.\cr \link[PNADcIBGE]{pnadc_labeller} for labelling categorical variables from PNADC microdata.\cr \link[PNADcIBGE]{pnadc_deflator} for adding deflator variables to PNADC microdata.\cr \link[PNADcIBGE]{pnadc_design} for creating PNADC survey design object.
#' @examples
#' pnadc_example()
#' pnadc_example(path="exampledata.txt")
#' pnadc_example(path="input_example.txt")
#' pnadc_example(path="dictionaryexample.xls")
#' pnadc_example(path="deflatorexample.xls")
#' @export

pnadc_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package="PNADcIBGE"))
  }
  else {
    system.file("extdata", path, package="PNADcIBGE", mustWork=TRUE)
  }
}