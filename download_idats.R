# Uma parcela grande de participantes não possui IDAT disponíveis (e nem terão: https://www.biostars.org/p/182566/)
# Baixar intensities
args <- commandArgs()

s0 <- as.numeric(args[6])
sf <- as.numeric(args[7])



geoid <- args[8]
geodesc <- args[9]



# geoid <- "GSE210255"
# geodesc <- "COVID-19 DNA methylation analysis"
# s0 <- 801
# sf <- 1000


print(paste0("Downloading batch from ", s0, " to ", sf ))
suppressMessages({
  library(tidyr)
  library(dplyr)
  library(tibble)
  # library(RCurl)
  library(curl)
  library(readxl)
  library(stringr)
  library(GEOquery)  
  library(data.table)  
})




MAX_TRIES <- 15

geoid_with_n  <- function(geoid){
  nnn_ac <- geoid
  id_n <- nchar(geoid)
  for( n in seq(id_n, id_n-2,-1)){
    substr(nnn_ac, n, n) <- 'n'
  }

  return(nnn_ac)
}


download_idat <- function(url, max_tries=10){

  print(url)
  data <- NULL
  for( t in seq(1, max_tries)){
    tryCatch({
      data <- curl_fetch_memory(url)
      data <- data$content
      # data <- RCurl::getBinaryURL(url )
      break
    },
    error=function(cond) {
      Sys.sleep(10)
    })
  }

  if( t == max_tries){
    stop("Failed to get data")
  }else{
    print("Data downloaded successfuly")
  }

  return(data)

}

# geoid <- 'GSE210256'
# geodesc <- "DNA methylation profiling of whole blood from Han Chinese individuals: identification of age-related CpG sites for forensic use"
platform <- "EPIC"
geoid_n <- geoid_with_n(geoid)
base_url <- paste0('https://ftp.ncbi.nlm.nih.gov/geo/series/',
                   geoid_n,
                   '/',
                   geoid,
                   '/suppl/')

file_list <- RCurl::getURL(paste0(base_url, '/filelist.txt'),
                           dirlistonly = FALSE)
file_df <- read.csv(text=file_list, sep='\t') %>%
      dplyr::filter(Type == "IDAT") 

if( s0 > (nrow(file_df)/2)){
  stop(paste0(s0, " > number of sample (", (nrow(file_df)/2), ")"))
}

grn_df <- file_df %>% 
    filter(str_detect(Name, "Grn")) %>%
    dplyr::slice(s0:sf)
red_df <- file_df %>% 
  filter(str_detect(Name, "Red")) %>%
  dplyr::slice(s0:sf)

  
  

# browser()
source('data_funcs.R')
source('meta_funcs.R')

if( sf > nrow(red_df)){
  sf <- nrow(red_df)
}


# start_batch()
# for(i in seq(1, nrow(file_df), by=2)){
for(i in seq(1, sf)){
  # i <- 1
  sample_id <- strsplit(red_df[i,"Name"], "_")[[1]][1]
  
  # redFile <- red_df %>% filter(str_detect(Name, sample_id)) %>% pull(Name)
  # grnFile <- grn_df %>% filter(str_detect(Name, sample_id)) %>% pull(Name)
  
  
  sample_id_n <- geoid_with_n(sample_id)
  
  for( t in seq(1, MAX_TRIES)){
    tryCatch({
      gdata <- GEOquery::getGEO(sample_id)
      mdata <- GEOquery::Meta(gdata)
      break
    },
    error=function(cond) {
      
      Sys.sleep(30)
    })
  }

  if( t == MAX_TRIES){
    stop("Failed to get data getGEO")
  }else{
    # print("Data downloaded successfuly")
  }


  grnFile <- data.frame(Name=mdata$supplementary_file) %>% filter(str_detect(Name, "Grn"))
  redFile <- data.frame(Name=mdata$supplementary_file) %>% filter(str_detect(Name, "Red"))
  
  processDir <- "/home/thiago/workspaces/epigenica/data/base_completa/tmp"  #tempdir()
  
  data_grn <- download_idat(stringr::str_replace(grnFile, "ftp:", "https:"))
  #tmpFile <- paste0(processDir, "/", strsplit(grnFile$Name,"/")[[1]][9]  )
  #tmpFile <- paste0(tempfile(), ".gz")

  #writeBin( object = as.raw(data_grn), con=tmpFile )
  #out <-system(paste0("gzip -t ", tmpFile), intern = TRUE)
  #unlink(tmpFile)
  # 
  # if( length(out) > 0){
  #   print("ERROR in downloaded file")
  #   next
  # }
  
  data_red <- download_idat(stringr::str_replace(redFile, "ftp:", "https:"))
  #tmpFile <- paste0(tempfile(), ".gz")
  # tmpFile <- paste0(processDir, "/", strsplit(redFile$Name,"/")[[1]][9]  )
  
  #writeBin( object = as.raw(data_red), con=tmpFile )
  #out <-system(paste0("gzip -t ", tmpFile), intern = TRUE)
  #unlink(tmpFile)
  
  # if( length(out) > 0){
  #   print("ERROR in downloaded file")
  #   next
  # }
  

  mdf <- get_meta_dict( mdata$characteristics_ch1, geoid)

  timeDf <- mdf %>% filter(KEY=="TIME")

  if(nrow(timeDf) == 0){
    sample_id <- paste0(sample_id, "_1" )
  }else{
    sample_id <- paste0(sample_id, "_", timeDf %>% pull(VALUE) )
  }

  
  pessoa_id <- add_pessoa(platform, geoid, geodesc, sample_id, data_red,
                          basename(redFile$Name),
                          data_grn,
                          basename(grnFile$Name))




  add_metadados(pessoa_id, mdf)


  rm(list=c("gdata", "mdata", "data_grn", "data_red", "con", "pessoa_id",
            "mdf"))


  gc(reset = T, full = T)

}
# end_batch()


