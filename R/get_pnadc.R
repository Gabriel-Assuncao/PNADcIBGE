#' Download, label, deflate and create survey design object for PNADC microdata
#' @description Core function of package. With this function only, the user can download a PNADC microdata from a year or quarter and get a sample design object ready to use with \code{survey} package functions.
#' @import survey readr dplyr magrittr projmgr httr RCurl utils timeDate readxl tibble
#' @param year The year of the data to be downloaded. Must be a number between 2012 and current year. Vector not accepted.
#' @param quarter The quarter of the year of the data to be downloaded. Must be number from 1 to 4. Vector not accepted. If \code{NULL}, \code{interview} or \code{topic} number must be provided.
#' @param interview The interview number of the data to be downloaded. Must be number from 1 to 5. Vector not accepted. Using this option will get annual per interview data. If \code{NULL}, \code{quarter} or \code{topic} number must be provided.
#' @param topic The quarter related to the topic of the data to be downloaded. Must be number from 1 to 4. Vector not accepted. Using this option will get annual per topic data. If \code{NULL}, \code{quarter} or \code{interview} number must be provided.
#' @param vars Vector of variable names to be kept for analysis. Default is to keep all variables.
#' @param defyear The year of the deflator data to be downloaded for annual microdata. Must be a number between 2017 and the last available year. Vector not accepted. If \code{NULL}, the deflator year will be defined as the last year available for interview microdata, or as equal to \code{year} for topic microdata. When \code{quarter} is defined, this argument will be ignored. This argument will be used only if \code{deflator} was set as \code{TRUE}.
#' @param defperiod The quarter period of the deflator data to be downloaded for annual per topic microdata. Must be number from 1 to 4. Vector not accepted. If \code{NULL}, the deflator period will be defined as equal to \code{topic}. When \code{quarter} or \code{interview} is defined, this argument will be ignored. This argument will be used only if \code{deflator} was set as \code{TRUE}.
#' @param labels Logical value. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary.
#' @param deflator Logical value. If \code{TRUE}, deflator variables will be available for use in the microdata.
#' @param design Logical value. If \code{TRUE}, will return an object of class \code{survey.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @param savedir Directory to save the downloaded data. Default is to use a temporary directory.
#' @return An object of class \code{survey.design} with the data from PNADC and its sample design, or a tibble with selected variables of the microdata, including the necessary survey design ones.
#' @note For more information, visit the survey official website <\url{https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html?=&t=o-que-e}> and consult the other functions of this package, described below.
#' @seealso \link[PNADcIBGE]{read_pnadc} for reading PNADC microdata.\cr \link[PNADcIBGE]{pnadc_labeller} for labelling categorical variables from PNADC microdata.\cr \link[PNADcIBGE]{pnadc_deflator} for adding deflator variables to PNADC microdata.\cr \link[PNADcIBGE]{pnadc_design} for creating PNADC survey design object.\cr \link[PNADcIBGE]{pnadc_example} for getting the path of the quarter PNADC example files.
#' @examples
#' \donttest{
#' pnadc.svy <- get_pnadc(year=2017, quarter=4, vars=c("VD4001","VD4002"), defyear=2017, defperiod=4,
#'                        labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' # Calculating proportion of employed and unemployed people
#' if (!is.null(pnadc.svy)) survey::svymean(x=~VD4002, design=pnadc.svy, na.rm=TRUE)
#' pnadc.svy2 <- get_pnadc(year=2017, interview=5, vars=c("V4112","V4121B"), defyear=2017, defperiod=4,
#'                         labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' # Calculating average hours dedicated to the care of people or household chores
#' if (!is.null(pnadc.svy2)) survey::svymean(x=~V4121B, design=pnadc.svy2, na.rm=TRUE)
#' pnadc.svy3 <- get_pnadc(year=2017, topic=4, vars=c("S07006","S07007"), defyear=2017, defperiod=4,
#'                         labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#' # Calculating proportion of cell phone for personal use with internet access
#' if (!is.null(pnadc.svy3)) survey::svymean(x=~S07007, design=pnadc.svy3, na.rm=TRUE)}
#' @export

get_pnadc <- function(year, quarter = NULL, interview = NULL, topic = NULL, vars = NULL, defyear = NULL, defperiod = NULL, 
                       labels = TRUE, deflator = TRUE, design = TRUE, savedir = tempdir())
{
  if (is.null(quarter) & is.null(interview) & is.null(topic)) {
    message("Quarter number or interview number or topic number must be provided.")
    return(NULL)
  }
  if ((!is.null(quarter) & !is.null(interview)) | 
      (!is.null(quarter) & !is.null(topic)) | 
      (!is.null(interview) & !is.null(topic)) | 
      (!is.null(quarter) & !is.null(interview) & !is.null(topic))) {
    message("Must be provided only one between quarter number, interview number and topic number.")
    return(NULL)
  }
  if (year < 2012) {
    message("Year must be greater or equal to 2012.")
    return(NULL)
  }
  if (year > timeDate::getRmetricsOptions("currentYear")) {
    message("Year cannot be greater than current year.")
    return(NULL)
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    message(paste0("The directory provided does not exist, so the directory was set to '", tempdir()), "'.")
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir)-1)
  }
  if (!is.null(quarter)) {
    if (quarter < 1 | quarter > 4) { 
      message("Quarter number must be an integer from 1 to 4.")
      return(NULL)
    }
    ftpdir <- ("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/")
    if (!projmgr::check_internet()) {
      message("The internet connection is unavailable.")
      return(NULL)
    }
    if (httr::http_error(httr::GET(ftpdir, httr::timeout(60)))) {
      message("The microdata server is unavailable.")
      return(NULL)
    }
    options(timeout=max(300, getOption("timeout")))
    ftpdata <- paste0(ftpdir, year, "/")
    datayear <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".zip"))
    dataname <- datayear[which(startsWith(datayear, paste0("PNADC_0", quarter, year)))]
    if (length(dataname) == 0) {
      message("Data unavailable for selected quarter and year.")
      return(NULL)
    }
    else {
      dataname <- paste0(dataname, ".zip")
    }
    utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
    utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
    docfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Documentacao/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".zip"))
    inputzip <- paste0(docfiles[which(startsWith(docfiles, "Dicionario_e_input"))], ".zip")
    utils::download.file(url=paste0(ftpdir, "Documentacao/", inputzip), destfile=paste0(savedir, "/Dicionario_e_input.zip"), mode="wb")
    utils::unzip(zipfile=paste0(savedir, "/Dicionario_e_input.zip"), exdir=savedir)
    microdataname <- dir(savedir, pattern=paste0("^PNADC_0", quarter, year, ".*\\.txt$"), ignore.case=FALSE)
    microdatafile <- paste0(savedir, "/", microdataname)
    microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),])[length(microdatafile)]
    inputname <- dir(savedir, pattern=paste0("^input_PNADC_trimestral.*\\.txt$"), ignore.case=FALSE)
    inputfile <- paste0(savedir, "/", inputname)
    inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),])[length(inputfile)]
    data_pnadc <- PNADcIBGE::read_pnadc(microdata=microdatafile, input_txt=inputfile, vars=vars)
    if (labels == TRUE) {
      if (exists("pnadc_labeller", where="package:PNADcIBGE", mode="function")) {
        dicname <- dir(savedir, pattern=paste0("^dicionario_PNADC_microdados_trimestral.*\\.xls$"), ignore.case=FALSE)
        dicfile <- paste0(savedir, "/", dicname)
        dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),])[length(dicfile)]
        data_pnadc <- PNADcIBGE::pnadc_labeller(data_pnadc=data_pnadc, dictionary.file=dicfile)
      }
      else {
        message("Labeller function is unavailable in package PNADcIBGE.")
      }
    }
    if (deflator == TRUE) {
      if (exists("pnadc_deflator", where="package:PNADcIBGE", mode="function")) {
        defzip <- paste0(docfiles[which(startsWith(docfiles, "Deflatores"))], ".zip")
        utils::download.file(url=paste0(ftpdir, "Documentacao/", defzip), destfile=paste0(savedir, "/Deflatores.zip"), mode="wb")
        utils::unzip(zipfile=paste0(savedir, "/Deflatores.zip"), exdir=savedir)
        defname <- dir(savedir, pattern=paste0("^deflator_PNADC_.*\\_trimestral_.*\\.xls$"), ignore.case=FALSE)
        deffile <- paste0(savedir, "/", defname)
        deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),])[length(deffile)]
        data_pnadc <- PNADcIBGE::pnadc_deflator(data_pnadc=data_pnadc, deflator.file=deffile)
      }
      else {
        message("Deflator function is unavailable in package PNADcIBGE.")
      }
    }
  }
  if (!is.null(interview)) {
    if (interview < 1 | interview > 5) {
      message("Interview number must be a integer from 1 to 5.")
      return(NULL)
    }
    ftpdir <- ("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/")
    if (!projmgr::check_internet()) {
      message("The internet connection is unavailable.")
      return(NULL)
    }
    if (httr::http_error(httr::GET(ftpdir, httr::timeout(60)))) {
      message("The microdata server is unavailable.")
      return(NULL)
    }
    options(timeout=max(300, getOption("timeout")))
    ftpdata <- paste0(ftpdir, "Visita_", interview, "/Dados/")
    datayear <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".zip"))
    dataname <- datayear[which(startsWith(datayear, paste0("PNADC_", year, "_visita", interview)))]
    if (length(dataname) == 0) {
      message("Data unavailable for selected interview and year.")
      return(NULL)
    }
    else {
      dataname <- paste0(dataname, ".zip")
    }
    utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
    utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
    docfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Visita_", interview, "/Documentacao/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".txt"))
    if (year < 2015) {
      inputpre <- paste0(docfiles[which(startsWith(docfiles, paste0("input_PNADC_2012_a_2014_visita", interview)))], ".txt")
    }
    else {
      inputpre <- paste0(docfiles[which(startsWith(docfiles, paste0("input_PNADC_", year, "_visita", interview)))], ".txt")
    }
    utils::download.file(url=paste0(ftpdir, "Visita_", interview, "/Documentacao/", inputpre), destfile=paste0(savedir, "/", inputpre), mode="wb")
    microdataname <- dir(savedir, pattern=paste0("^PNADC_", year, "_visita", interview, ".*\\.txt$"), ignore.case=FALSE)
    microdatafile <- paste0(savedir, "/", microdataname)
    microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),])[length(microdatafile)]
    inputname <- dir(savedir, pattern=paste0("^input_PNADC_.*\\_visita", interview, ".*\\.txt$"), ignore.case=FALSE)
    inputfile <- paste0(savedir, "/", inputname)
    inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),])[length(inputfile)]
    data_pnadc <- PNADcIBGE::read_pnadc(microdata=microdatafile, input_txt=inputfile, vars=vars)
    if (labels == TRUE) {
      if (exists("pnadc_labeller", where="package:PNADcIBGE", mode="function")) {
        dicfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Visita_", interview, "/Documentacao/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".xls"))
        if (year < 2015) {
          dicpre <- paste0(dicfiles[which(startsWith(dicfiles, paste0("dicionario_PNADC_microdados_2012_a_2014_visita", interview)))], ".xls")
        }
        else {
          dicpre <- paste0(dicfiles[which(startsWith(dicfiles, paste0("dicionario_PNADC_microdados_", year, "_visita", interview)))], ".xls")
        }
        utils::download.file(url=paste0(ftpdir, "Visita_", interview, "/Documentacao/", dicpre), destfile=paste0(savedir, "/", dicpre), mode="wb")
        dicname <- dir(savedir, pattern=paste0("^dicionario_PNADC_microdados_.*\\_visita", interview, ".*\\.xls$"), ignore.case=FALSE)
        dicfile <- paste0(savedir, "/", dicname)
        dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),])[length(dicfile)]
        data_pnadc <- PNADcIBGE::pnadc_labeller(data_pnadc=data_pnadc, dictionary.file=dicfile)
      }
      else {
        message("Labeller function is unavailable in package PNADcIBGE.")
      }
    }
    if (deflator == TRUE) {
      if (exists("pnadc_deflator", where="package:PNADcIBGE", mode="function")) {
        arcfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Documentacao_Geral/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".xls"))
        if (is.null(defyear)) {
          defyear <- timeDate::getRmetricsOptions("currentYear") - 1
          message(paste0("Deflator year was not provided, so deflator year was set to ", defyear, "."))
        }
        if (defyear < year) {
          defyear <- year
          message(paste0("Deflator year must be greater or equal to microdata year, so deflator year was changed to ", defyear, "."))
        }
        if (defyear < 2017 | defyear >= timeDate::getRmetricsOptions("currentYear")) {
          defyear <- timeDate::getRmetricsOptions("currentYear") - 1
          message(paste0("Deflator year must be greater or equal to 2017 and cannot be greater or equal than current year, so deflator year was changed to ", defyear, "."))
        }
        if (length(arcfiles[which(startsWith(arcfiles, paste0("deflator_PNADC_", defyear)))]) == 0) {
          defyear <- defyear - 1
          message(paste0("Deflator data unavailable for selected year, so deflator year was changed to ", defyear, "."))
        }
        defpre <- paste0(arcfiles[which(startsWith(arcfiles, paste0("deflator_PNADC_", defyear)))], ".xls")
        utils::download.file(url=paste0(ftpdir, "Documentacao_Geral/", defpre), destfile=paste0(savedir, "/", defpre), mode="wb")
        defname <- dir(savedir, pattern=paste0("^deflator_PNADC_", defyear, ".*\\.xls$"), ignore.case=FALSE)
        deffile <- paste0(savedir, "/", defname)
        deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),])[length(deffile)]
        data_pnadc <- PNADcIBGE::pnadc_deflator(data_pnadc=data_pnadc, deflator.file=deffile)
      }
      else {
        message("Deflator function is unavailable in package PNADcIBGE.")
      }
    }
  }
  if (!is.null(topic)) {
    if (topic < 1 | topic > 4) {
      message("Topic number must be a integer from 1 to 4.")
      return(NULL)
    }
    ftpdir <- ("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Trimestre/")
    if (!projmgr::check_internet()) {
      message("The internet connection is unavailable.")
      return(NULL)
    }
    if (httr::http_error(httr::GET(ftpdir, httr::timeout(60)))) {
      message("The microdata server is unavailable.")
      return(NULL)
    }
    options(timeout=max(300, getOption("timeout")))
    ftpdata <- paste0(ftpdir, "Trimestre_", topic, "/Dados/")
    datayear <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(ftpdata, dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".zip"))
    dataname <- datayear[which(startsWith(datayear, paste0("PNADC_", year, "_trimestre", topic)))]
    if (length(dataname) == 0) {
      message("Data unavailable for selected topic and year.")
      return(NULL)
    }
    else {
      dataname <- paste0(dataname, ".zip")
    }
    utils::download.file(url=paste0(ftpdata, dataname), destfile=paste0(savedir, "/", dataname), mode="wb")
    utils::unzip(zipfile=paste0(savedir, "/", dataname), exdir=savedir)
    docfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Trimestre_", topic, "/Documentacao/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".txt"))
    inputpre <- paste0(docfiles[which(startsWith(docfiles, paste0("input_PNADC_trimestre", topic)))], ".txt")
    utils::download.file(url=paste0(ftpdir, "Trimestre_", topic, "/Documentacao/", inputpre), destfile=paste0(savedir, "/", inputpre), mode="wb")
    microdataname <- dir(savedir, pattern=paste0("^PNADC_", year, "_trimestre", topic, ".*\\.txt$"), ignore.case=FALSE)
    microdatafile <- paste0(savedir, "/", microdataname)
    microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime),])[length(microdatafile)]
    inputname <- dir(savedir, pattern=paste0("^input_PNADC_trimestre", topic, ".*\\.txt$"), ignore.case=FALSE)
    inputfile <- paste0(savedir, "/", inputname)
    inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime),])[length(inputfile)]
    data_pnadc <- PNADcIBGE::read_pnadc(microdata=microdatafile, input_txt=inputfile, vars=vars)
    if (labels == TRUE) {
      if (exists("pnadc_labeller", where="package:PNADcIBGE", mode="function")) {
        dicfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Trimestre_", topic, "/Documentacao/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".xls"))
        dicpre <- paste0(dicfiles[which(startsWith(dicfiles, paste0("dicionario_PNADC_microdados_trimestre", topic)))], ".xls")
        utils::download.file(url=paste0(ftpdir, "Trimestre_", topic, "/Documentacao/", dicpre), destfile=paste0(savedir, "/", dicpre), mode="wb")
        dicname <- dir(savedir, pattern=paste0("^dicionario_PNADC_microdados_trimestre", topic, ".*\\.xls$"), ignore.case=FALSE)
        dicfile <- paste0(savedir, "/", dicname)
        dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime),])[length(dicfile)]
        data_pnadc <- PNADcIBGE::pnadc_labeller(data_pnadc=data_pnadc, dictionary.file=dicfile)
      }
      else {
        message("Labeller function is unavailable in package PNADcIBGE.")
      }
    }
    if (deflator == TRUE) {
      if (exists("pnadc_deflator", where="package:PNADcIBGE", mode="function")) {
        arcfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Documentacao_Geral/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".xls"))
        if (is.null(defyear) | is.null(defperiod)) {
          defyear <- year
          defperiod <- topic
          message(paste0("Deflator year or period was not provided, so deflator year was set to ", defyear, " and period was set to ", defperiod, "."))
        }
        if (defyear < year) {
          defyear <- year
          message(paste0("Deflator year must be greater or equal to microdata year, so deflator year was changed to ", defyear, "."))
        }
        if (defyear == 2016) {
          defyear <- 2017
          message(paste0("There is no Deflator data for 2016, so deflator year was changed to ", defyear, "."))
        }
        if (defyear < 2017 | defyear > timeDate::getRmetricsOptions("currentYear")) {
          defyear <- year
          message(paste0("Deflator year must be greater or equal to 2017 and cannot be greater than current year, so deflator year was changed to ", defyear, "."))
        }
        if (defyear == year & defperiod < topic) {
          defperiod <- topic
          message(paste0("For ", defyear, ", deflator period must be greater or equal to microdata topic, so deflator period was changed to ", defperiod, "."))
        }
        if (defperiod < 1 | defperiod > 4) {
          defperiod <- topic
          message(paste0("Deflator period must be greater or equal to 1 and cannot be greater than 4, so deflator period was changed to ", defperiod, "."))
        }
        perfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Trimestre_", defperiod, "/Documentacao/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".xls"))
        if (length(perfiles[which(startsWith(perfiles, paste0("deflator_PNADC_", defyear, "_trimestre", defperiod)))]) == 0) {
          defyear <- year
          defperiod <- topic
          perfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", "\n", RCurl::getURL(paste0(ftpdir, "Trimestre_", defperiod, "/Documentacao/"), dirlistonly=TRUE)), "\n")), "<a href=[[:punct:]]")), ".xls"))
          message(paste0("Deflator data unavailable for selected year and period, so deflator year was changed to ", defyear, " and period was changed to ", defperiod, "."))
        }
        defpre <- paste0(perfiles[which(startsWith(perfiles, paste0("deflator_PNADC_", defyear, "_trimestre", defperiod)))], ".xls")
        utils::download.file(url=paste0(ftpdir, "Trimestre_", defperiod, "/Documentacao/", defpre), destfile=paste0(savedir, "/", defpre), mode="wb")
        defname <- dir(savedir, pattern=paste0("^deflator_PNADC_", defyear, "_trimestre", defperiod, ".*\\.xls$"), ignore.case=FALSE)
        deffile <- paste0(savedir, "/", defname)
        deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime),])[length(deffile)]
        data_pnadc <- PNADcIBGE::pnadc_deflator(data_pnadc=data_pnadc, deflator.file=deffile)
      }
      else {
        message("Deflator function is unavailable in package PNADcIBGE.")
      }
    }
  }
  if (design == TRUE) {
    if (exists("pnadc_design", where="package:PNADcIBGE", mode="function")) {
      data_pnadc <- PNADcIBGE::pnadc_design(data_pnadc=data_pnadc)
    }
    else {
      message("Sample design function is unavailable in package PNADcIBGE.")
    }
  }
  return(data_pnadc)
}
