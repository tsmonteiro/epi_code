args <- commandArgs()


# GSE198904 <- Multiple files...

# first <- as.numeric(args[6])
# size_block <- as.numeric(args[7])
#GSE152026

first <- as.numeric(args[6])
size_block <- as.numeric(args[7])-1
geoid <- args[8] #"GSE179325"
geodesc <- args[9] #"GSE179325"


first <- 1
size_block <- 20

# main_intensities first block geoid geodesc inputFile 
inputFile <- "/home/thiago/workspaces/epigenica/data/signal_intensity_files/GSE196696/GSE196696_signal_intensities.tsv"
inputFile <- "/home/thiago/workspaces/epigenica/data/signal_intensity_files/GSE198904/GSE198904_DHRC_Matrix_signal_intensities.txt"


geoid <- 'GSE198904'
geodesc <- "EpiMatch DNA Methylation Resource"
geodesc <- "Meta-analysis of Epigenome Wide Association Studies of Major Depressive Disorder"
platform <- "EPIC2"

suppressMessages({
  library(stringr)
  library(GEOquery)
  library(data.table)
  library(dplyr)
  library(ChAMP)
})

uline <- 1
mline <- 2
pline <- 3
ncolsFile <- as.numeric(system(paste0("awk -F'\t' '{print NF}' ", inputFile, " | sort -nu | tail -n 1"), intern = TRUE))


#cols <- lapply(seq(1, ncolsFile), function(x){NULL})
#cols[1] <- "character"
#sondas = read.csv(inputFile, sep = "\t", colClasses = cols)
sondas = fread(cmd=paste0("cut -f1 -d'\t' ", inputFile))
#cols[1] <- NULL

p0 <- first+1
pf <- p0 + (size_block*3-3) + 1 +1
N <- (pf-p0+1)/3
# cols[p0:pf] <- "numeric"

cmdStr = 'cut -f'
for(i in seq(p0, pf)){
  cmdStr <- paste0(cmdStr, i)
  if( i < pf ){
    cmdStr <- paste0(cmdStr, ',')  
  }
}

cmdStr <- paste0(cmdStr, " -d'\t' ", inputFile)  

sig = fread(cmd=cmdStr)


# TODO FIXME
sourceSig <- unique(unlist(lapply(colnames(sig), function(x){
  strsplit(x, "\\.")[[1]][1]
})))
# Source name é onde tá info pra bater com o sinal....
# Daí pega unique rowname do sig, e acha qual os idx preseents nos metas....

# source_name = gdata$GSE196696_series_matrix.txt.gz$source_name_ch1
#save('source_name', file = "/home/thiago/workspaces/epigenica/data/signal_intensity_files/GSE196696/source_name.rda")

#TODO Ajustar os nomes aqui baseado no dataset
if( file.exists("/home/thiago/workspaces/epigenica/data/signal_intensity_files/GSE196696/gdata.rda")){
  load("/home/thiago/workspaces/epigenica/data/signal_intensity_files/GSE196696/gdata.rda")
}else{
  gdata <- GEOquery::getGEO(geoid)  
  save('gdata', file = "/home/thiago/workspaces/epigenica/data/signal_intensity_files/GSE196696/gdata.rda")
}


# Pro dataset GSE198904, é preciso fazer o match primeiro com o campo gdata[[1]]$characteristics_ch1.11
# 
source('meta_funcs_B.R')
source("data_funcs.R")
if( p0 == 2){

  
  nsub <- length(gdata[[1]]$characteristics_ch1)
  for(i in seq(1, nsub)){
    sample_id <- gdata[[1]]$geo_accession[i]
    mdf <- get_meta_info(gdata[[1]], i, geoid)
    # browser()
    pessoa_id <- add_pessoa(platform, geoid, geodesc, sample_id, NULL,"",NULL,"")
    add_metadados(pessoa_id, mdf)
  }

}

sampleIds <- get_pessoas_dataset(geoid)

uline <- 1
mline <- 2
pline <- 3
betas <- NULL
detP <- NULL
intensities <- NULL
# CONTINUAR DAQUI

# anno <- read.csv(inputAnno, skip = 7)
# 
# annoSondas <- intersect(anno$Name, sondas$ID_REF)
# annoIdx <- which(annoSondas %in% anno$Name)
# 
# annoColor <- anno$Color_Channel[annoIdx]
# annoType <- anno$Infinium_Design_Type[annoIdx]

blockSampleIds <- c()
for(i in seq(1, ncol(sig)-1, by=3)){
  uidx <- i + uline - 1
  midx <- i + mline - 1
  pidx <- i + pline - 1
  blockSampleIds <- append(blockSampleIds, gdata[[1]]$geo_accession[p0-1 + i])
  
  b <- sig[,..midx] / (sig[,..midx] + sig[,..uidx] + 100)
  mval <- log2(sig[,..midx] /  sig[,..midx]  )

  
  if(is.null(betas)){
    betas <- data.frame(b)
    intensities <- data.frame(mval+b)
    detP <- data.frame(sig[,..pidx])
    
  }else{
    betas <- betas %>% #add_row( data.frame(t(sig[,p0+midx] / (sig[,p0+midx] + sig[,p0+midx] + 100))) ) 
      cbind(data.frame(b) )
    intensities <- intensities %>% #add_row( data.frame(t(sig[,p0+midx] / (sig[,p0+midx] + sig[,p0+midx] + 100))) ) 
      cbind(data.frame(mval+b) )
    detP <- detP %>% 
      cbind(data.frame(sig[,..pidx]))
  }
  #offset <- 100
  # BetaValue <- M / (M + U + offset)
  # message("  Generating M Matrix")
  # MValue <- log2(M/U)
  # message("  Generating intensity Matrix")
  # intensity <-  M + U
  # message("  Calculating Detect P value")
  # detP <- matrix(NA,nrow=nrow(intensity),ncol=ncol(intensity))
  
}

source('data_funcs.R')


sampleIds <- blockSampleIds 


# age <- df %>% pull(AGE)
# gender <- df %>% pull(GENDER)

sample_sheet <- NULL



for( i in seq(1, dim(betas)[2])){
  tmpSampleDf <- data.frame(
    "Index"=i,
    "Dataset"=geoid,
    "Sample_Name"=sampleIds[i],
    "Time"=1,
    "Sample_Plate"=NA,
    "Sample_Group"=NA,
    "Pool_ID"=NA,
    "Project"=NA,
    "Sample_Well"=NA,
    "Sentrix_Position"=NA,
    "Sentrix_ID"=NA)
  
  if(is.null(sample_sheet)){
    sample_sheet <- tmpSampleDf
  }else{
    sample_sheet <- rbind(sample_sheet, tmpSampleDf)
  }
}





colnames(betas) <- sample_sheet$Sample_Name
rownames(betas) <- unlist(sondas)

colnames(intensities) <- sample_sheet$Sample_Name
rownames(intensities) <- unlist(sondas)

colnames(detP) <- sample_sheet$Sample_Name
rownames(detP) <- unlist(sondas)


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
                     resultsDir=paste0(processDir, "/Norm"),
                     method="BMIQ",
                     plotBMIQ=N,
                     arraytype="EPIC",
                     cores=2 )  




# source('meta_funcs_B.R')
# setwd('/home/thiago/workspaces/epigenica/projects/data_retrieval')


add_sondas(rownames(myLoad$beta))
pessoasDf <- get_pessoas_id_2(blockSampleIds)

lapply(seq(1, dim(betas)[[2]]), function(i){
  #add_betas( dataset, pessoaDf$id[i], rownames(myLoad$beta), myLoad$beta[,i], myNorm[,i] )
  add_betas(geoid, pessoasDf$id[i], rownames(myLoad$beta), myLoad$beta[,i], myNorm[,i] )
})

