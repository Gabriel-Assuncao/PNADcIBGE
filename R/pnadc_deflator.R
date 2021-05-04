#' Add deflator variables to PNADC microdata
#' @description This function adds deflator variables to PNADC microdata. For deflation of income variables, the documentation provided through the following addresses must be used:\cr Quarter - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/PNADcIBGE_Deflator_Trimestral.pdf}.\cr Annual per Interview - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Documentacao_Geral/PNADcIBGE_Deflator_Anual_Visita.pdf}.\cr Annual per Topic - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/Documentacao_Geral/PNADcIBGE_Deflator_Anual_Trimestre.pdf}.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param data_pnadc A tibble of PNADC microdata read with \code{read_pnadc} function.
#' @param deflator.file The deflator file for selected survey available on official website:\cr Quarter (select the deflator zip file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/}.\cr Annual per Interview (select a deflator xls file, according to the appropriated year) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Documentacao_Geral/}.\cr Annual per Topic (select a deflator xls file, according to the appropriated period and, then, inside the documentation folder, choose the desired year) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/}.
#' @return A tibble with the data provided from PNADC survey and the deflator variables added for use.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNADcIBGE]{get_pnadc} for downloading, labelling, deflating and creating survey design object for PNADC microdata.\cr \link[PNADcIBGE]{read_pnadc} for reading PNADC microdata.\cr \link[PNADcIBGE]{pnadc_labeller} for labelling categorical variables from PNADC microdata.\cr \link[PNADcIBGE]{pnadc_design} for creating PNADC survey design object.\cr \link[PNADcIBGE]{pnadc_example} for getting the path of the quarter PNADC example files.
#' @examples
#' # Using data read from disk
#' input_path <- pnadc_example(path="input_example.txt")
#' data_path <- pnadc_example(path="exampledata.txt")
#' dictionary.path <- pnadc_example(path="dictionaryexample.xls")
#' deflator.path <- pnadc_example(path="deflatorexample.xls")
#' pnadc.df <- read_pnadc(microdata=data_path, input_txt=input_path, vars=c("VD4001","VD4002"))
#' pnadc.df <- pnadc_labeller(data_pnadc=pnadc.df, dictionary.file=dictionary.path)
#' pnadc.df <- pnadc_deflator(data_pnadc=pnadc.df, deflator.file=deflator.path)
#' \donttest{
#' # Downloading data
#' pnadc.df2 <- get_pnadc(year=2017, quarter=4, vars=c("VD4001","VD4002"), defyear=2017, defperiod=4,
#'                       labels=TRUE, deflator=FALSE, design=FALSE, savedir=tempdir())
#' deflator.path2 <- pnadc_example(path="deflatorexample.xls")
#' pnadc.df2 <- pnadc_deflator(data_pnadc=pnadc.df2, deflator.file=deflator.path2)}
#' @export

pnadc_deflator <- function(data_pnadc, deflator.file) {
  if (sum(class(data_pnadc) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("Ano", "Trimestre", "UF") %in% names(data_pnadc)))) {
      data_pnadc <- data_pnadc[, !names(data_pnadc) %in% c("Habitual", "Efetivo", "CO1", "CO1e", "CO2", "CO2e", "CO3"), drop=FALSE]
      deflator <- suppressMessages(readxl::read_excel(deflator.file))
      colnames(deflator)[c(1:3)] <- c("Ano", "Trimestre", "UF")
      deflator$Trimestre <- ifelse(deflator$Trimestre == "01-02-03","1",
                                   ifelse(deflator$Trimestre == "04-05-06","2",
                                          ifelse(deflator$Trimestre == "07-08-09","3",
                                                 ifelse(deflator$Trimestre == "10-11-12","4",
                                                        ifelse(nchar(deflator$Trimestre) > 1,"",
                                                               ifelse(deflator$Trimestre %in% c("1","2","3","4"),deflator$Trimestre,""))))))
      deflator <- deflator[deflator$Trimestre != "",]
      deflator$UF <- as.factor(deflator$UF)
      if (identical(intersect(levels(deflator$UF), levels(as.factor(data_pnadc$UF))), character(0)) & length(levels(deflator$UF)) == length(levels(as.factor(data_pnadc$UF)))) {
        levels(deflator$UF) <- levels(as.factor(data_pnadc$UF))
      }
      data_pnadc <- base::merge(x=data_pnadc, y=deflator, by.x=c("Ano", "Trimestre", "UF"), by.y=c("Ano", "Trimestre", "UF"), all.x=TRUE, all.y=FALSE)
      data_pnadc <- data_pnadc[order(data_pnadc$Estrato, data_pnadc$UPA, data_pnadc$V1008, data_pnadc$V2003),]
      data_pnadc <- tibble::as_tibble(data_pnadc)
    }
    else {
      message("Merge variables required for adding deflator variables are missing.")
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so adding deflator variables is not possible.")
  }
  return(data_pnadc)
}
