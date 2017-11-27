#' Label categorical variables from PNADC datasets
#'
#' @import survey readr dplyr magrittr readxl
#' @param data_pnadc A tibble of PNADc data read with \code{read_pnadc} function.
#' @param dictionary.file The dictionary file for selected survey available on official website: \url{ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Dicionario_e_input.zip}
#' @return  A tibble with the data provided from PNADc survey and its categorical variables as factors with labels.
#' @examples
#' input_path <- pnadc_example("input_example.txt")
#' data_path <- pnadc_example("exampledata.txt")
#' dictionary.path <- pnadc_example("dictionaryexample.xls")
#' pnadc.df <- read_pnadc(data_path, input_path, vars="VD4002")
#' pnadc.df <- pnadc_labeller(pnadc.df,dictionary.path)
#'
#' @export

pnadc_labeller <- function(data_pnadc,dictionary.file){
  dictionary <- readxl::read_excel(dictionary.file)
  X__3=X__6=X__7=NULL
  colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
  dictionary %<>% subset(!is.na(X__6))
  codcurrent <- dictionary$X__3
  for(i in 1:dim(dictionary)[1]){
    if(is.na(dictionary$X__3[i])){
      dictionary$X__3[i] <- codcurrent
    }
    else{
      codcurrent <- dictionary$X__3[i]
    }
  }
  notlabel <- c("Ano", "Trimestre", "UPA", "Estrato", "V1008",
                "V1014", "V1016", "posest", "V2003", "V2008", "V20081",
                "V20082", "V40081", "V40082", "V40083", "V4010", "V4013",
                "V4041", "V4044","V4075A1","VD4031","VD4035",
                "V401511","V401512","V40161","V40162","V40163","V401711",
                "V40181","V40182","V40183")
  vars <- names(data_pnadc)
  varsc <- vars[sapply(data_pnadc, class) == "character"]
  varsf <- setdiff(varsc, notlabel)
  for (i in 1:length(varsf)) {
    if (varsf[i] %in% (dictionary$X__3)) {
      data_pnadc[varsf[i]] <- factor(as.numeric(unlist(data_pnadc[varsf[i]])),
                                     levels = suppressWarnings(
                                       as.numeric(unlist(dictionary %>%
                                              subset(X__3 == varsf[i]) %>%
                                              select(X__6)))),
                                     labels = unlist(dictionary %>%
                                              subset(X__3 == varsf[i]) %>%
                                              select(X__7)))
    }
  }
  return(data_pnadc)
}
