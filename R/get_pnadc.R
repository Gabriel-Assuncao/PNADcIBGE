#' Download and read PNADc microdata
#' @import  readr dplyr magrittr RCurl utils timeDate
#' @param quarter The quarter of the year of the data to be downloade. must be number from 1 to 4. Vector not accepted.
#' @param year the year of the data to be downloaded.must be a number between 2012 and current year. Vector not accepted.
#' @param vars Character vector of the name of the variables you want to keep for analysys. \code{default} is to keep all variables
#' @param savedir Directory for dowloading data. \code{default} is to use a temporary directory.
#' @return A tibble with the survey design variables and selected variables.
#' @examples
#' \dontrun{
#' pnadc.df <- get_pnadc(2,2016)
#' pnadc.df2 <- get_pnadc(1,2017,vars=c("VD4001","VD4002"))}
#'
#' @export
#'
get_pnadc=function(quarter, year, vars=NULL,savedir=tempdir()){
  if(year<2012) stop("Year must be greater or equal to 2012")
  if (quarter>4 | quarter<1) stop("Quarter must be a integer from 1 to 4")
  if (year>timeDate::getRmetricsOptions("currentYear")) stop("Year can't be greater than current year")
  ftpdir=("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/")
  ftpdata <- paste0(ftpdir,year,"/")
  datayear <- unlist(strsplit(RCurl::getURL(ftpdata, dirlistonly = TRUE),"\r\n"))
  dataname <- datayear[substr(datayear,1,12)==paste0("PNADC_0", quarter, year)]
  if(length(dataname)==0){
    stop('Data unavailable for selected quarter/year')
  }
  utils::download.file(paste0(ftpdir,"Documentacao/Dicionario_e_input.zip"),paste0(savedir,"/input.zip"))
  utils::unzip(paste0(savedir,"/input.zip"),exdir=savedir)
  if(year<2015 |(year == 2015 & quarter < 4)){
    input_txt <- "Input PNADC_1Tri_2012 a 3Tri_2015.txt"
  }
  else if(year < 2016 | (year == 2016 & quarter == 1)){
    input_txt <- "Input PNADC_4Tri_2015 a 1Tri_2016.txt"
  }
  else{
    filenames <- dir(savedir,pattern = "*.txt")
    input_txt <- filenames[substr(filenames, 1, 21) == "Input PNADC_2Tri_2016"]
  }
  utils::download.file(paste0(ftpdata,dataname),paste0(savedir,"/",dataname))
  utils::unzip(paste0(savedir,"/",dataname),exdir=savedir)
  microdataname <- paste0(savedir,"/PNADC_0",quarter,year,".txt")
  data_pnadc <- read_pnadc(microdataname,paste0(savedir,"/",input_txt),vars=vars)
  return(data_pnadc)
}
