# Load a batch of clients for processing
library(dplyr)
library(tidyr)
library(tibble)


batch <- seq(1,10)

source('data_funcs.R')
pessoasDf <- get_pessoa_batch(batch, c("AGE", "GENDER"))
metaDf <- pessoasDf[[2]]


metaDf <- tidyr::pivot_wider(metaDf, names_from=c("id_metadado"), values_from = "val")

metaDf <- metaDf %>%
  mutate_at("AGE", as.numeric) %>%
  mutate("GENDER" = replace(GENDER, GENDER == "F", "Female")) %>%
  mutate("GENDER" = replace(GENDER, GENDER == "M", "Male")) #%>%
  #mutate("RACE" = replace(RACE, RACE == "African American (AA)", "AfricanAmerican"))

pessoasDf <- pessoasDf[[1]]
# mutate(var = replace(var, var != "Candy", "Not Candy"))

metaDf$geoid <-unlist(lapply(pessoasDf$red_idat_fname, function(x){
  paste0(strsplit(x, "_")[[1]][1], "_1")
}))

processDir <- "/home/thiago/workspaces/epigenica/data/base_completa/tmp"  #tempdir()
unlink(  processDir, force = T  )
dir.create(processDir, recursive = T)

sample_sheet <- NULL

for( i in seq(1, nrow(pessoasDf))){
  writeBin( pessoasDf$red_idat_raw[[i]], 
          paste0( processDir, '/', pessoasDf$red_idat_fname[[i]] ) )
  writeBin( pessoasDf$green_idat_raw[[i]], 
            paste0( processDir, '/', pessoasDf$green_idat_fname[[i]] ) )
  
  # "Index","Dataset","Sample_Name","Time","Sample_Plate","Sample_Group","Pool_ID","Project","Sample_Well","Sentrix_Position","Sentrix_ID","Age","Gender","Health","COVID","Death"
  # "1",4,"GSE179325","GSM5414278",1,NA,NA,NA,NA,NA,"R03C01","204379110031",103,"F","Severe","Positive","0"
  tmpSampleDf <- data.frame(
    "Index"=i,
    "Dataset"=metaDf$id_dataset[i],
    "Sample_Name"=metaDf$geoid[i],
    "Time"=1,
    "Sample_Plate"=NA,
    "Sample_Group"=NA,
    "Pool_ID"=NA,
    "Project"=NA,
    "Sample_Well"=NA,
    "Sentrix_Position"=NA,
    "Sentrix_ID"=NA,
    "Age"=metaDf$AGE[i],
    "Gender"=metaDf$GENDER[i])
  
  if(i == 1){
    sample_sheet <- tmpSampleDf
  }else{
    sample_sheet <- rbind(sample_sheet, tmpSampleDf)
  }
}

write.csv(sample_sheet, 
          file=paste0( processDir, '/sample_sheet.csv'))

# Criar DIR
# Copiar arquivos de maneira organizada (sample sheet) para o dir
# criar Sample sheet
# processar

# install.packages(c("BiocManager", "remotes))
# BiocManager::install(c("ChAMP","ChAMPdata","Illumina450ProbeVariants.db","sva","IlluminaHumanMethylation450kmanifest","limma","RPMM","DNAcopy","preprocessCore","impute","marray","wateRmelon","goseq","plyr","GenomicRanges","RefFreeEWAS","qvalue","isva","doParallel","bumphunter","quadprog","shiny","shinythemes","plotly","RColorBrewer","DMRcate","dendextend","IlluminaHumanMethylationEPICmanifest","FEM","matrixStats","missMethyl","combinat"))
# remotes::install_github("perishky/meffil")
library(ChAMP)
source('champ_import.R')

# My import contem sinal metilado, nao metilado e os betas. Valores parecem ter um "drift"
myImport <- champ.import.local(processDir, arraytype="EPIC")

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
                     plotBMIQ=T,
                     arraytype="EPIC",
                     cores=2)  


# TODO Create beta/probes table and add the batch results there

source('data_funcs.R')
add_sondas(rownames(myNorm))

source('data_funcs.R')
t0 <- Sys.time()
add_betas(metaDf$id, myLoad$beta, myNorm)
tf <- Sys.time()
print(tf-t0)


source('data_funcs.R')
all_probe_names <- get_sondas()
all_pessoas <- get_lista_pessoas()
id_pessoas <- unlist(as.list(all_pessoas["id"]))
id_sondas <- unlist(as.list(all_probe_names["id"]))

betas <- get_betas_sonda( id_sondas[1] )

tmp_df <- data.frame(rep(NA, nrow(all_probe_names)))

betas <- tmp_df
betas <- cbind(betas, tmp_df)
# add_betas(metaDf$geoid myLoad$beta, myNorm)

