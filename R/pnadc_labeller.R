#' Label categorical variables from PNADC microdata
#' @description This function labels categorical variables from PNADC microdata.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param data_pnadc A tibble of PNADC microdata read with \code{read_pnadc} function.
#' @param dictionary.file The dictionary file for selected survey available on official website:\cr Quarter (select the dictionary and input zip file) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/}.\cr Annual per Interview (select a dictionary xls file, according to the appropriated interview and, then, inside the documentation folder, choose the desired year) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/}.\cr Annual per Topic (select a dictionary xls file, according to the appropriated quarter related to the topic, inside the documentation folder) - \url{https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/}.
#' @return A tibble with the data provided from PNADC survey and its categorical variables as factors with related labels.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNADcIBGE]{get_pnadc} for downloading, labelling, deflating and creating survey design object for PNADC microdata.\cr \link[PNADcIBGE]{read_pnadc} for reading PNADC microdata.\cr \link[PNADcIBGE]{pnadc_deflator} for adding deflator variables to PNADC microdata.\cr \link[PNADcIBGE]{pnadc_design} for creating PNADC survey design object.\cr \link[PNADcIBGE]{pnadc_example} for getting the path of the quarter PNADC example files.
#' @examples
#' # Using data read from disk
#' input_path <- pnadc_example(path="input_example.txt")
#' data_path <- pnadc_example(path="exampledata.txt")
#' dictionary.path <- pnadc_example(path="dictionaryexample.xls")
#' pnadc.df <- read_pnadc(microdata=data_path, input_txt=input_path, vars="VD4002")
#' pnadc.df <- pnadc_labeller(data_pnadc=pnadc.df, dictionary.file=dictionary.path)
#' \donttest{
#' # Downloading data
#' pnadc.df2 <- get_pnadc(year=2017, quarter=4, vars="VD4002", defyear=2017, defperiod=4,
#'                       labels=FALSE, deflator=FALSE, design=FALSE, savedir=tempdir())
#' dictionary.path2 <- pnadc_example(path="dictionaryexample.xls")
#' pnadc.df2 <- pnadc_labeller(data_pnadc=pnadc.df2, dictionary.file=dictionary.path2)}
#' @export

pnadc_labeller <- function(data_pnadc, dictionary.file) {
  if (sum(class(data_pnadc) == "tbl_df") > 0) {
    dictionary <- suppressMessages(readxl::read_excel(dictionary.file))
    X__3 = X__6 = X__7 = NULL
    colnames(dictionary) <- paste0("X__",1:dim(dictionary)[2])
    dictionary %<>% subset(!is.na(X__6))
    codcurrent <- dictionary$X__3
    for (i in 1:dim(dictionary)[1]) {
      if (is.na(dictionary$X__3[i])) {
        dictionary$X__3[i] <- codcurrent
      }
      else {
        codcurrent <- dictionary$X__3[i]
      }
    }
    notlabel <- c("Ano", "Trimestre", "UPA", "Estrato", "V1008", "V1014", "V1016",
                  "V1027", "V1028", "V1029", "V1030", "V1031", "V1032", "posest",
                  "V2003", "V2008", "V20081", "V20082",
                  "V40081", "V40082", "V40083", "V4010", "V4013",
                  "V4041", "V4044", "V4075A1", "VD4031", "VD4035",
                  "V401511", "V401512", "V40161", "V40162", "V40163",
                  "V401711", "V40181", "V40182", "V40183", "S08002",
                  "S080062", "S080063", "S08007", "S08008", "S080091",
                  "S080192", "S080193", "S08020", "S08021", "S080221",
                  "S080322", "S080323", "S08033", "S08034", "S080351",
                  "S080442", "S0804431", "S080444", "S08044B",
                  "S080462", "S0804631", "S080464", "S08046B",
                  "Habitual", "Efetivo", "CO1", "CO1e", "CO2", "CO2e", "CO3")
    vars <- names(data_pnadc)
    varsc <- vars[sapply(data_pnadc, class) == "character"]
    varsf <- setdiff(varsc, notlabel)
    for (i in 1:length(varsf)) {
      if (i > 0 & varsf[i] %in% (dictionary$X__3)) {
        data_pnadc[varsf[i]] <- factor(suppressWarnings(as.numeric(unlist(data_pnadc[varsf[i]]))), 
                                       levels=suppressWarnings(as.numeric(unlist(dictionary %>% subset(X__3 == varsf[i]) %>% select(X__6)))),
                                       labels=unlist(dictionary %>% subset(X__3 == varsf[i]) %>% select(X__7)))
      }
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so labelling categorical variables is not possible.")
  }
  return(data_pnadc)
}