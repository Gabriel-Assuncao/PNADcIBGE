#' Download, label and create design object for PNADc microdata
#' @description Core function of package. with this function only, the user can download a PNADc microdata from a year or quarter and get a desgin object ready to use with \code{survey} package functions.
#' @import  readr dplyr magrittr RCurl utils timeDate
#' @param year The year of the data to be downloaded. Must be a number between 2012 and current year. Vector not accepted.
#' @param quarter The quarter of the year of the data to be downloade. Must be number from 1 to 4. Vector not accepted. If \code{NULL}, \code{interview} number must be provided.
#' @param interview The interview number of the data to be downloade. Must be number from 1 to 5. Vector not accepted. Using this option will get annual data. If \code{NULL}, quarterly data will be downloaded instead.
#' @param vars Character vector of the name of the variables you want to keep for analysys. \code{default} is to keep all variables
#' @param labels \code{logical}. If \code{TRUE}, categorical variables will presented as factors with labels corresponding to the survey's dictionary. Not available for annual data.
#' @param savedir Directory for dowloading data. \code{default} is to use a temporary directory.
#' @param design \code{logical}. If \code{TRUE}, \code{get_pnadc} will return a object of class \code{survey.design}. It is strongly recommended to keep this parameter as \code{TRUE} for further analysis. If \code{FALSE}, only the microdata will be returned.
#' @return An object of class \code{survey.design} with the data from PNADc survey and its sample design or a tibble with the survey design variables and selected variables.
#' @examples
#' \dontrun{
#' pnadc.svy <- get_pnadc(2,2016)
#' pnadc.svy2 <- get_pnadc(1,2017,vars=c("VD4001","VD4002"))
#' survey::svymean(~VD4002, pnadc.svy2, na.rm=TRUE)
#' }
#'
#' @export
#'
get_pnadc=function(year, quarter=NULL, interview=NULL, vars=NULL,labels=T,design=T,savedir=tempdir()){
  if(is.null(quarter)&is.null(interview)) stop("Quarter or Interview number must be provided.")
  if(!is.null(quarter)&!is.null(interview)) stop("Must be provided ONLY quarter number or interview number.")
  if(year<2012) stop("Year must be greater or equal to 2012.")
  if(year>timeDate::getRmetricsOptions("currentYear")) stop("Year can't be greater than current year.")
  if(!is.null(quarter)){
    if(quarter>4 | quarter<1) stop("Quarter must be a integer from 1 to 4.")
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
    if(labels==T){
      dicnames <- dir(savedir,pattern = "*.xls")
      matches <- regmatches(dicnames, gregexpr("[[:digit:]]+", dicnames))
      for(i in 1:length(dicnames)){
        if(as.numeric(matches[[i]])[2]<=year & as.numeric(matches[[i]])[4]>=year){
          if(as.numeric(matches[[i]])[4]==year & as.numeric(matches[[i]])[3]>=quarter){
            dicfile=paste0(savedir,"/",dicnames[i])
          }
          else if(as.numeric(matches[[i]])[2]==year & as.numeric(matches[[i]])[1]<=quarter){
            dicfile=paste0(savedir,"/",dicnames[i])
          }
          else if (as.numeric(matches[[i]])[2]<year & as.numeric(matches[[i]])[4]>year){
            dicfile=paste0(savedir,"/",dicnames[i])
          }
        }
      }
      data_pnadc<-pnadc_labeller(data_pnadc,dicfile)
    }
  }
  #Anual data
  if(!is.null(interview)){
    if(interview>5 | interview<1) stop("interview must be a integer from 1 to 5.")
    ftpdir=("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/")
    ftpdata <- paste0(ftpdir,"Dados/")
    datayear <- unlist(strsplit(RCurl::getURL(ftpdata, dirlistonly = TRUE),"\r\n"))
    yrint_list <- regmatches(datayear, gregexpr("[[:digit:]]+", datayear))
    dataname=NULL
    for(i in 1:length(datayear)){
      if(as.numeric(yrint_list[[i]])[1]==year & as.numeric(yrint_list[[i]])[2]==interview){
        dataname=datayear[i]
      }
    }
    if(length(dataname)==0){
      stop('Data unavailable for selected interview/year')
    }
    if(year<2015){
      input_txt <- paste0("Input_PNADC_",interview,"entr_2012_a_2014.txt")
    }
    else {
      input_txt <- paste0("Input_PNADC_",interview,"entr_",year,".txt")
    }
    utils::download.file(paste0(ftpdata,dataname),paste0(savedir,"/",dataname))
    utils::download.file(paste0(ftpdir,"Documentacao/",input_txt),paste0(savedir,"/",input_txt))
    utils::unzip(paste0(savedir,"/",dataname),exdir=savedir)
    microdataname <- paste0(savedir,"/",dataname)
    data_pnadc <- read_pnadc(microdataname,paste0(savedir,"/",input_txt),vars=vars)
    # if(labels==T){
    #   dicnames <- unlist(strsplit(RCurl::getURL(paste0(ftpdir,"Documentacao/"), dirlistonly = TRUE),"\r\n"))
    #   dicnames <- dicnames[unlist(substr(dicnames,1,3))=="dic"]
    #   matches <- regmatches(dicnames, gregexpr("[[:digit:]]+", dicnames))
    #   for(i in 1:length(dicnames)){
    #     if(as.numeric(matches[[i]])[1]==interview){
    #       if(length(matches[[i]])==2){
    #         if(as.numeric(matches[[i]])[2]==year){
    #           dicfile=paste0(savedir,"/Dic_",interview,year,".xls")
    #           utils::download.file(paste0(ftpdir,"Documentacao/",dicnames[i]),dicfile)
    #         }
    #       }
    #       if(length(matches[[i]])>2){
    #         if(as.numeric(matches[[i]])[2]<=year & as.numeric(matches[[i]])[3]>=year){
    #           dicfile=paste0(savedir,"/Dic_",interview,year,".xls")
    #           utils::download.file(paste0(ftpdir,"Documentacao/",dicnames[i]),dicfile)
    #         }
    #       }
    #     }
    #   }
    #   data_pnadc<-pnadc_labeller(data_pnadc,dicfile)
    # }
  }
  if(design==TRUE){
    data_pnadc=pnadc_design(data_pnadc)
  }
  return(data_pnadc)
}
