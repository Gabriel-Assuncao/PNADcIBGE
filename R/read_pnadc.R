#' Read PNADc microdata
#' @import readr dplyr magrittr
#' @param microdata A text file containing microdata from PNADc survey. The file must be downloaded from \url{ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/}
#' @param input_txt A text file available along with the microdata containing the input script for SAS. They are available at \url{ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Dicionario_e_input.zip}
#' @param vars Character vector of the name of the variables you want to keep for analysys. \code{default} is to keep all variables
#' @return A tibble with the survey design variables and selected variables.
#' @examples
#' input_path <- pnadc_example("input_example.txt")
#' data_path <- pnadc_example("exampledata.txt")
#' pnadc.df <- read_pnadc(data_path,input_path,vars="VD4002")
#' @export

read_pnadc <- function(microdata,input_txt,vars=NULL) {
  X1=X2=X3=start=end=NULL
  ########### reading SAS input
  input <- suppressWarnings(suppressMessages({readr::read_table2(input_txt,col_names = F) %>%
    subset(substr(X1, 1, 1) == "@") %>%
    dplyr::mutate(type=ifelse(substr(X3, 1, 1) == "$","c","d"),
           start=as.numeric(gsub("@", "", X1)),
           X3=as.integer(chartr("$", " ", X3)),
           end=start+X3-1)}))

  ########## creating columns specification
  if(!is.null(vars)){
    if(any(!(vars %in% input$X2))) {
      missvar=vars[!(vars %in% input$X2)]
      warning(paste("Variables", paste(missvar,collapse = ", "), "not present in dataset\n"))
      }
    input %<>% subset(X2 %in% c("UPA","Estrato","V1027","posest","V1029",vars))
  }
  columns <- input%$%
    readr::fwf_positions(start,end,X2)

  ########## reading data
  data_pnadc <- suppressWarnings(readr::read_fwf(microdata,columns,
                         col_types = paste0(input$type,collapse = "")))
  return(data_pnadc)
}
