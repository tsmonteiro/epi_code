#TODO Query IDATs, process and save
args <- commandArgs()

first <- as.numeric(args[6])
size_block <- as.numeric(args[7])-1
dataset <- args[8] #"GSE179325"


 # first <- 1
 # size_block <- 50-1
 # 
 # dataset <- "GSE210255"



timeStart <- Sys.time()

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
})




 
listIds <- get_pessoas_id( dataset )
sample_sheet <- NULL

processDir <- "/home/thiago/workspaces/epigenica/data/base_completa/tmp"  #tempdir()

suppressWarnings({
  unlink(  paste0(processDir, "/*"), force = T, recursive = T  )
  dir.create(processDir, recursive = T)
})

t0 <- first
tf <- first+size_block
if( tf > nrow(listIds)){
  tf <- nrow(listIds)
}



listIds <- listIds %>%
  dplyr::slice(t0:tf) %>%
  pull(id)

pessoaDf <- get_pessoas_com_meta(listIds)
options(width = 500)

if(nrow(pessoaDf) <= 1){
  stop("")
}

print("*********************************************************************")
print("Pessoas no lote")

printDf <- pessoaDf %>%
  select(-red_idat_raw) %>%
  select(-green_idat_raw)
print(listIds)


for(i in seq(1, nrow(pessoaDf))){
  if(i == 1){
    prmatrix(unname(as.matrix(printDf[i,])), rowlab=rep("",1), collab=colnames(printDf) )
  }else{
    
    
    prmatrix(unname(as.matrix(printDf[i,])), rowlab=rep("",1), collab=rep("",ncol(printDf)) )
  }
  
}

# NOTE
# Os ids abaixo estão com erro de processamento. Excluir da base!
pessoaDf <- pessoaDf %>% filter(geoid != "GSM4256802_1")

print("*********************************************************************")
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
    "Dataset"=dataset,
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



add_sondas(rownames(myLoad$beta))
#cg07881041

lapply(seq(1, nrow(pessoaDf)), function(i){
  add_betas( dataset, pessoaDf$id[i], rownames(myLoad$beta), myLoad$beta[,i], myNorm[,i] )
})


timeEnd <- Sys.time()

print("PROCESSAMENTO FINALIZADO")
print(timeEnd-timeStart)

