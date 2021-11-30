#' Read PNADC microdata
#' @description This function reads PNADC microdata.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param microdata A text file containing microdata from PNADC survey, available on official website:\cr Quarter (select a microdata file, according to the appropriated year and, then, quarter) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/}.\cr Annual per Interview (select a microdata file, according to the appropriated interview and, then, inside the data folder, choose the desired year) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/}.\cr Annual per Topic (select a microdata file, according to the appropriated quarter related to the topic and, then, inside the data folder, choose the desired year) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/}.
#' @param input_txt A text file, related to the microdata, containing the input script for SAS, available on official website:\cr Quarter (select the dictionary and input zip file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/}.\cr Annual per Interview (select a input txt file, according to the appropriated interview and, then, inside the documentation folder, choose the desired year) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/}.\cr Annual per Topic (select a input txt file, according to the appropriated quarter related to the topic, inside the documentation folder) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/}.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @return A tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNADcIBGE]{get_pnadc} for downloading, labeling, deflating and creating survey design object for PNADC microdata.\cr \link[PNADcIBGE]{pnadc_labeller} for labeling categorical variables from PNADC microdata.\cr \link[PNADcIBGE]{pnadc_deflator} for adding deflator variables to PNADC microdata.\cr \link[PNADcIBGE]{pnadc_design} for creating PNADC survey design object.\cr \link[PNADcIBGE]{pnadc_example} for getting the path of the quarter PNADC toy example files.
#' @examples
#' input_path <- pnadc_example(path="input_example.txt")
#' data_path <- pnadc_example(path="exampledata.txt")
#' pnadc.df <- read_pnadc(microdata=data_path, input_txt=input_path, vars=c("VD4001","VD4002"))
#' @export

read_pnadc <- function(microdata, input_txt, vars = NULL) {
  X1 = X2 = X3 = start = end = NULL
  input <- suppressWarnings(suppressMessages({readr::read_table(input_txt, col_names=FALSE) %>% subset(substr(X1, 1, 1) == "@") %>%
    dplyr::mutate(type=ifelse(substr(X3, 1, 1) == "$","c","d"), start=as.numeric(gsub("@", "", X1)), X3=as.integer(chartr("$", " ", X3)), end=start+X3-1)}))
  if (!is.null(vars)) {
    if (any(!(vars %in% input$X2))) {
      missvar <- vars[!(vars %in% input$X2)]
      message(paste("Variables", paste(missvar, collapse=", "), "not present in microdata.\n"))
    }
    input %<>% subset(X2 %in% c("Ano", "Trimestre", "UF", "UPA", "ID_DOMICILIO", "Estrato", "V1008", "V1014", "V1027", "V1028", sprintf("V1028%03d", seq(1:200)), "V1029", "V1030", "V1031", "V1032", sprintf("V1032%03d", seq(1:200)), "V1033", "V1034", "posest", "posest_sxi", "V2003", vars))
  }
  columns <- input %$% readr::fwf_positions(start, end, X2)
  data_pnadc <- suppressWarnings(readr::read_fwf(microdata, columns, col_types=paste0(input$type, collapse="")))
  data_pnadc <- dplyr::mutate(data_pnadc, ID_DOMICILIO=paste0(data_pnadc$UPA, data_pnadc$V1008, data_pnadc$V1014))
  return(data_pnadc)
}
