#' Create PNADC survey object with its sample design
#' @description This function creates PNADC survey object with its sample design for analysis using \code{survey} package functions.
#' @import dplyr httr magrittr projmgr RCurl readr readxl survey tibble timeDate utils
#' @param data_pnadc A tibble of PNADC microdata read with \code{read_pnadc} function.
#' @return An object of class \code{survey.design} or \code{svyrep.design} with the data from PNADC and its sample design.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNADcIBGE]{get_pnadc} for downloading, labeling, deflating and creating survey design object for PNADC microdata.\cr \link[PNADcIBGE]{read_pnadc} for reading PNADC microdata.\cr \link[PNADcIBGE]{pnadc_labeller} for labeling categorical variables from PNADC microdata.\cr \link[PNADcIBGE]{pnadc_deflator} for adding deflator variables to PNADC microdata.\cr \link[PNADcIBGE]{pnadc_example} for getting the path of the quarter PNADC toy example files.
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
#' pnadc.svy <- pnadc_design(data_pnadc=pnadc.df)
#' # Calculating proportion of employed and unemployed people
#' if (!is.null(pnadc.svy)) survey::svymean(x=~VD4002, design=pnadc.svy, na.rm=TRUE)}
#' \donttest{
#' # Downloading data
#' pnadc.df2 <- get_pnadc(year=2017, quarter=4, selected=FALSE, vars=c("VD4001","VD4002"),
#'                        defyear=2017, defperiod=4, labels=TRUE, deflator=TRUE, design=FALSE,
#'                        reload=TRUE, curlopts=list(), savedir=tempdir())
#' pnadc.svy2 <- pnadc_design(data_pnadc=pnadc.df2)
#' # Calculating proportion of employed and unemployed people
#' if (!is.null(pnadc.svy2)) survey::svymean(x=~VD4002, design=pnadc.svy2, na.rm=TRUE)}
#' @export

pnadc_design <- function(data_pnadc) {
  if (sum(class(data_pnadc) == "tbl_df") > 0) {
    if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1027", "V1028", "V1029", "V1033", "posest", "posest_sxi") %in% names(data_pnadc))) |
        !(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1030", "V1031", "V1032", "V1034", "posest", "posest_sxi") %in% names(data_pnadc))) |
        !(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1027", "V1028", "V1029", "posest") %in% names(data_pnadc))) | 
        !(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1030", "V1031", "V1032", "posest") %in% names(data_pnadc))) |
        !(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1035", "V1036", "V1037", "V1038", "posest", "posest_sxi") %in% names(data_pnadc))) |
        !(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1039", "V1040", "V1041", "V1042", "posest", "posest_sxi") %in% names(data_pnadc))) |
        !(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1035", "V1036", "V1037", "posest") %in% names(data_pnadc))) | 
        !(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1039", "V1040", "V1041", "posest") %in% names(data_pnadc)))) {
      options(survey.lonely.psu="adjust")
      options(survey.adjust.domain.lonely=TRUE)
      if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1027", "V1028", "V1029", "V1033", "posest", "posest_sxi") %in% names(data_pnadc)))) {
        if (!(FALSE %in% (sprintf("V1028%03d", seq(1:200)) %in% names(data_pnadc)))) {
          data_posterior <- survey::svrepdesign(data=data_pnadc, weight=~V1028, type="bootstrap", repweights="V1028[0-9]+", mse=TRUE, replicates=length(sprintf("V1028%03d", seq(1:200))), df=length(sprintf("V1028%03d", seq(1:200))))
        }
        else {
          data_prior <- survey::svydesign(ids=~UPA+ID_DOMICILIO, strata=~Estrato, data=data_pnadc, weights=~V1027, nest=TRUE)
          popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1029)))
          popc.types <- popc.types[order(popc.types$posest),]
          popi.types <- data.frame(posest_sxi=as.character(unique(data_pnadc$posest_sxi)), Freq=as.numeric(unique(data_pnadc$V1033)))
          popi.types <- popi.types[order(popi.types$posest_sxi),]
          pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
          data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        }
      }
      else if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1030", "V1031", "V1032", "V1034", "posest", "posest_sxi") %in% names(data_pnadc)))) {
        if (!(FALSE %in% (sprintf("V1032%03d", seq(1:200)) %in% names(data_pnadc)))) {
          data_posterior <- survey::svrepdesign(data=data_pnadc, weight=~V1032, type="bootstrap", repweights="V1032[0-9]+", mse=TRUE, replicates=length(sprintf("V1032%03d", seq(1:200))), df=length(sprintf("V1032%03d", seq(1:200))))
        }
        else {
          data_prior <- survey::svydesign(ids=~UPA+ID_DOMICILIO, strata=~Estrato, data=data_pnadc, weights=~V1031, nest=TRUE)
          popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1030)))
          popc.types <- popc.types[order(popc.types$posest),]
          popi.types <- data.frame(posest_sxi=as.character(unique(data_pnadc$posest_sxi)), Freq=as.numeric(unique(data_pnadc$V1034)))
          popi.types <- popi.types[order(popi.types$posest_sxi),]
          pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
          data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        }
      }
      else if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1027", "V1028", "V1029", "posest") %in% names(data_pnadc)))) {
        data_prior <- survey::svydesign(ids=~UPA, strata=~Estrato, data=data_pnadc, weights=~V1027, nest=TRUE)
        popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1029)))
        popc.types <- popc.types[order(popc.types$posest),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)
      }
      else if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1030", "V1031", "V1032", "posest") %in% names(data_pnadc)))) {
        data_prior <- survey::svydesign(ids=~UPA, strata=~Estrato, data=data_pnadc, weights=~V1031, nest=TRUE)
        popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1030)))
        popc.types <- popc.types[order(popc.types$posest),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)
      }
      else if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1035", "V1036", "V1037", "V1038", "posest", "posest_sxi") %in% names(data_pnadc)))) {
        if (!(FALSE %in% (sprintf("V1036%03d", seq(1:200)) %in% names(data_pnadc)))) {
          data_posterior <- survey::svrepdesign(data=data_pnadc, weight=~V1036, type="bootstrap", repweights="V1036[0-9]+", mse=TRUE, replicates=length(sprintf("V1036%03d", seq(1:200))), df=length(sprintf("V1036%03d", seq(1:200))))
        }
        else {
          data_prior <- survey::svydesign(ids=~UPA+ID_DOMICILIO, strata=~Estrato, data=data_pnadc, weights=~V1035, nest=TRUE)
          popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1037)))
          popc.types <- popc.types[order(popc.types$posest),]
          popi.types <- data.frame(posest_sxi=as.character(unique(data_pnadc$posest_sxi)), Freq=as.numeric(unique(data_pnadc$V1038)))
          popi.types <- popi.types[order(popi.types$posest_sxi),]
          pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
          data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        }
      }
      else if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1039", "V1040", "V1041", "V1042", "posest", "posest_sxi") %in% names(data_pnadc)))) {
        if (!(FALSE %in% (sprintf("V1040%03d", seq(1:200)) %in% names(data_pnadc)))) {
          data_posterior <- survey::svrepdesign(data=data_pnadc, weight=~V1040, type="bootstrap", repweights="V1040[0-9]+", mse=TRUE, replicates=length(sprintf("V1040%03d", seq(1:200))), df=length(sprintf("V1040%03d", seq(1:200))))
        }
        else {
          data_prior <- survey::svydesign(ids=~UPA+ID_DOMICILIO, strata=~Estrato, data=data_pnadc, weights=~V1039, nest=TRUE)
          popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1041)))
          popc.types <- popc.types[order(popc.types$posest),]
          popi.types <- data.frame(posest_sxi=as.character(unique(data_pnadc$posest_sxi)), Freq=as.numeric(unique(data_pnadc$V1042)))
          popi.types <- popi.types[order(popi.types$posest_sxi),]
          pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
          data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        }
      }
      else if (!(FALSE %in% (c("UPA", "ID_DOMICILIO", "Estrato", "V1035", "V1036", "V1037", "posest") %in% names(data_pnadc)))) {
        data_prior <- survey::svydesign(ids=~UPA, strata=~Estrato, data=data_pnadc, weights=~V1035, nest=TRUE)
        popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1037)))
        popc.types <- popc.types[order(popc.types$posest),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)
      }
      else {
        data_prior <- survey::svydesign(ids=~UPA, strata=~Estrato, data=data_pnadc, weights=~V1039, nest=TRUE)
        popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$V1041)))
        popc.types <- popc.types[order(popc.types$posest),]
        data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)
      }
    }
    else {
      message("Weight variables required for sample design are missing.\n")
      data_posterior <- data_pnadc
    }
  }
  else {
    message("The microdata object is not of the tibble class or sample design was already defined for microdata, so applying another design is not possible.\n")
    data_posterior <- data_pnadc
  }
  return(data_posterior)
}
