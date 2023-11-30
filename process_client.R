suppressMessages({
  library("dnaMethyAge")
  library(stringr)
  library(GEOquery)
  library(data.table)
  library(dplyr)
  library(ChAMP)
  library(parallel)
  source('data_funcs.R')
  source('champ_import.R')
  library(readxl)
})

source("data_funcs.R")
#TODO Query IDATs, process and save
args <- commandArgs()

idatsPath <- "/home/thiago/workspaces/epigenica/data/base_completa/cliente" #args[6]
fList <- list.files(idatsPath)
fList <- fList[unlist(lapply(fList, function(x){
  str_ends(x, ".gz")
}))]

clientIdatsPath <- unlist(lapply(fList, function(x){
  paste0(idatsPath, "/", x)
}) )

metaPath <- "/home/thiago/workspaces/epigenica/data/base_completa/cliente/meta_01.xlsx" #args[7]


datasetList <- get_dataset_list()
listIds <- get_pessoas_id( NULL )

permIds <- sample(listIds$id, replace = F)
backIds <- permIds[1:29]


sample_sheet <- NULL

processDir <- "/home/thiago/workspaces/epigenica/data/base_completa/tmp"

suppressWarnings({
  unlink(  paste0(processDir, "/*"), force = T, recursive = T  )
  dir.create(processDir, recursive = T)
})


pessoaDf <- get_pessoas_com_meta(backIds)




sample_sheet <- NULL
for( i in seq(1, nrow(pessoaDf))){
  sentrixPosition <- NA #strsplit(pessoaDf$red_idat_fname[[i]], "_")[[1]][3]
  sentrixId <- NA#strsplit(pessoaDf$red_idat_fname[[i]], "_")[[1]][2]
  writeBin( pessoaDf$red_idat_raw[[i]],
            paste0( processDir, '/', pessoaDf$red_idat_fname[[i]] ), useBytes = T )
  writeBin( pessoaDf$green_idat_raw[[i]],
            paste0( processDir, '/', pessoaDf$green_idat_fname[[i]] ), useBytes = T )
  
  tmpSampleDf <- data.frame(
    "Index"=i,
    "Dataset"="Epigenica",
    "Sample_Name"=pessoaDf$geoid[i],
    "Time"=1,
    "Sample_Plate"=NA,
    "Sample_Group"=NA,
    "Pool_ID"=NA,
    "Project"=NA,
    "Sample_Well"=NA,
    "Sentrix_Position"=sentrixPosition,
    "Sentrix_ID"=sentrixId,
    "Age"=pessoaDf$AGE[i],
    "Gender"=pessoaDf$GENDER[i])
  
  if(is.null(sample_sheet)){
    sample_sheet <- tmpSampleDf
  }else{
    sample_sheet <- add_row(sample_sheet, tmpSampleDf)
    
  }
}

# INFORMACAO DO CLIENTE
clienteMetaDf <- read_xlsx(metaPath, col_names = F)
colnames(clienteMetaDf) <- c("KEY", "VALUE")
clienteMetaDf <- tidyr::pivot_wider(clienteMetaDf, values_from = "VALUE", names_from = "KEY")


clienteMetaDf$ID <- stringr::str_replace_all(
  stringr::str_replace_all( clienteMetaDf$ID, pattern = "[.]", ""),
  "[-]", "")


# !!! NOTA IMPORTANTE !!!
# Sample_Name tem que ser igual a primeira parte do nome do arquivo IDAT
# Por exemplo?
# Sample_Name="GSM3516794" para o arquivo GSM3516794_201236480160_R07C01_Grn

tmpSampleDf <- data.frame(
  "Index"=i,
  "Dataset"="Epigenica",
  "Sample_Name"=clienteMetaDf$ID,
  "Time"=1,
  "Sample_Plate"=NA,
  "Sample_Group"=NA,
  "Pool_ID"=NA,
  "Project"=NA,
  "Sample_Well"=NA,
  "Sentrix_Position"=sentrixPosition,
  "Sentrix_ID"=sentrixId,
  "Age"=clienteMetaDf$IDADE,
  "Gender"=clienteMetaDf$SEXO)
sample_sheet <- add_row(sample_sheet, tmpSampleDf)


file.copy(clientIdatsPath[1], paste0(processDir, "/", fList[1]) )
file.copy(clientIdatsPath[2], paste0(processDir, "/", fList[2]) )

write.csv(x = sample_sheet,  file = paste0(processDir, "/sample_sheet.csv"), )

suppressMessages({
  source("champ_import.R")  
  myImport = champ.import.local(directory=processDir, arraytype = "EPIC")  
})


suppressMessages({
  myLoad <- champ.filter(beta=myImport$beta,
                         M=NULL,
                         pd=myImport$pd,
                         intensity=NULL, #myImport$intensity,
                         Meth=NULL,
                         UnMeth=NULL,
                         detP=NULL, #myImport$detP,
                         beadcount=NULL, #myImport$beadcount,
                         autoimpute=FALSE,
                         filterDetP=TRUE,
                         ProbeCutoff=0,
                         SampleCutoff=0.1,
                         detPcut=0.01,
                         filterBeads=TRUE,
                         beadCutoff=0.05,
                         filterNoCG=TRUE,
                         filterSNPs=TRUE,
                         population=NULL,
                         filterMultiHit=TRUE,
                         filterXY=TRUE,
                         arraytype="EPIC")
  myNorm <- champ.norm(beta=myLoad$beta,
                       rgSet=myLoad$rgSet,
                       mset=myLoad$mset,
                       resultsDir=paste0(processDir, "/Norm"),
                       method="BMIQ",
                       plotBMIQ=F,
                       arraytype="EPIC",
                       cores=1 )
})




betas <- myLoad$beta[,30]

source("data_funcs.R")
create_table("EPIGENICA2023")

data_red <- readBin(clientIdatsPath[2], "raw", 10e6)
data_grn <- readBin(clientIdatsPath[1], "raw", 10e6)

pessoa_id <- add_pessoa("EPIC", "EPIGENICA2023", "Clientes Epigenica", 
                        clienteMetaDf$ID, 
                        data_red,
                        basename(clientIdatsPath[2]),
                        data_grn,
                        basename(clientIdatsPath[1]))


add_betas( "EPIGENICA2023", pessoa_id, 
           rownames(myLoad$beta), 
           myLoad$beta[,30], 
           myNorm[,30] )



dunedin <- methyAge(myNorm, clock='DunedinPACE', do_plot = F, inputation = FALSE )
tl <- methyAge(myNorm, clock='LuA2019', do_plot = F, inputation = FALSE )
horv <- methyAge(myNorm, clock='HorvathS2018', do_plot = F, inputation = FALSE )


add_metadados_single(pessoa_id, "DUNEDIN", dunedin$mAge[30])
add_metadados_single(pessoa_id, "TELOMERE", tl$mAge[30])
add_metadados_single(pessoa_id, "HORVATH", horv$mAge[30])


