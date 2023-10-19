#TODO Query IDATs, process and save
args <- commandArgs()

first <- as.numeric(args[6])
size_block <- as.numeric(args[7])-1
dataset <- args[8] #"GSE179325"

# first <- 1
# size_block <- 50-1
# dataset <- "GSE210255"



timeStart <- Sys.time()

suppressMessages({
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


unlink(  paste0(processDir, "/*"), force = T, recursive = T  )
dir.create(processDir, recursive = T)

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

print("*********************************************************************")

for( i in seq(1, nrow(pessoaDf))){
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
    "Sentrix_Position"=NA,
    "Sentrix_ID"=NA,
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
  myImport = champ.import.local(directory=processDir, arraytype = "EPIC")  
})


suppressMessages({
  myLoad <- champ.filter(beta=myImport$beta,
                         M=NULL,
                         pd=myImport$pd,
                         intensity=myImport$intensity,
                         Meth=NULL,
                         UnMeth=NULL,
                         detP=myImport$detP,
                         beadcount=myImport$beadcount,
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

mclapply(seq(1, nrow(pessoaDf)), function(i){
  add_betas( pessoaDf$id[i], rownames(myLoad$beta), myLoad$beta[,i], myNorm[,i] )
}, mc.cores = 2)


timeEnd <- Sys.time()

print("PROCESSAMENTO FINALIZADO")
print(timeEnd-timeStart)





























for( i in seq(p0, pf)){
  
  
  tmpSampleDf <- data.frame(
    "Index"=i-p0+1,
    "Dataset"=dataset,
    "Sample_Name"=sampleIds[i],
    "Time"=1,
    "Sample_Plate"=NA,
    "Sample_Group"=NA,
    "Pool_ID"=NA,
    "Project"=NA,
    "Sample_Well"=NA,
    "Sentrix_Position"=NA,
    "Sentrix_ID"=NA,
    "Age"=age[i],
    "Gender"=gender[i])
  
  if(is.null(sample_sheet)){
    sample_sheet <- tmpSampleDf
  }else{
    sample_sheet <- rbind(sample_sheet, tmpSampleDf)
  }
}





colnames(betas) <- sample_sheet$Sample_Name
rownames(betas) <- unlist(probenames)

colnames(intensities) <- sample_sheet$Sample_Name
rownames(intensities) <- unlist(probenames)

colnames(detP) <- sample_sheet$Sample_Name
rownames(detP) <- unlist(probenames)
champ.import()

# message("\n  Generating beta Matrix")
# BetaValue <- M / (M + U + offset)
# message("  Generating M Matrix")
# MValue <- log2(M/U)
# message("  Generating intensity Matrix")
# intensity <-  M + U
# message("  Calculating Detect P value")
# detP <- matrix(NA,nrow=nrow(intensity),ncol=ncol(intensity))
# rownames(detP) <- rownames(intensity)
# colnames(detP) <- colnames(intensity)
# 
# type_II <- rownames(Anno$Annotation)[Anno$Annotation[,"Channel"] == "g+r"]
# type_II <- type_II[type_II %in% rownames(detP)]
# type_I.red <- rownames(Anno$Annotation)[Anno$Annotation[,"Channel"] == "r"]
# type_I.red <- type_I.red[type_I.red %in% rownames(detP)]
# type_I.grn <- rownames(Anno$Annotation)[Anno$Annotation[,"Channel"] == "g"]
# type_I.grn <- type_I.grn[type_I.grn %in% rownames(detP)]
# for(i in 1:ncol(detP))
# {
#   detP[type_II,i] <- 1 - pnorm(intensity[type_II,i], mean=rMu[i]+gMu[i], sd=rSd[i]+gSd[i])
#   detP[type_I.red,i] <- 1 - pnorm(intensity[type_I.red,i], mean=rMu[i]*2, sd=rSd[i]*2)
#   detP[type_I.grn,i] <- 1 - pnorm(intensity[type_I.grn,i], mean=gMu[i]*2, sd=gSd[i]*2)
# }
# if(sum(is.na(detP))) message("    !!! There are NA values in your detP matrix.\n")
myLoad <- champ.filter(beta=betas,
                       M=NULL,
                       pd=sample_sheet,
                       intensity=intensities,
                       Meth=NULL,
                       UnMeth=NULL,
                       detP=detP[1,],
                       beadcount=NULL,
                       autoimpute=FALSE,
                       filterDetP=TRUE,
                       ProbeCutoff=0,
                       SampleCutoff=0.1,
                       detPcut=0.01,
                       filterBeads=FALSE,
                       beadCutoff=0.05,
                       filterNoCG=TRUE,
                       filterSNPs=TRUE,
                       population=NULL,
                       filterMultiHit=TRUE,
                       filterXY=TRUE,
                       fixOutlier = FALSE,
                       arraytype="EPIC")

processDir <- "/home/thiago/workspaces/epigenica/data/base_completa/tmp" 

myNorm <- champ.norm(beta=myLoad$beta,
                     rgSet=myLoad$rgSet,
                     mset=myLoad$mset,
                     resultsDir=paste0(basepath, "/Norm"),
                     method="BMIQ",
                     plotBMIQ=T,
                     arraytype="EPIC",
                     cores=2 )  




source('meta_funcs_B.R')
# setwd('/home/thiago/workspaces/epigenica/projects/data_retrieval')

metaDf <- get_meta_info(geoMeta, dataset)

sampleIds <- metaDf %>% filter(KEY=="ID") %>% pull(VALUE)
metaDf <- metaDf %>% filter(KEY != "ID")
#FIXME duplicando nome de sonda
add_sondas(rownames(myLoad$beta))

for( i in seq(1, size_block)){
  
  
  pessoa_id <- add_pessoa("EPIC", dataset, dataset_desc, sampleIds[i], NULL, NULL, NULL, NULL)
  add_metadados(pessoa_id, metaDf)
  
  add_betas( pessoa_id, rownames(myLoad$beta), myLoad$beta[,i], myNorm[,i] )
}