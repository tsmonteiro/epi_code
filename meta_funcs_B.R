get_meta_info <- function(gdata, i, dataset){

  if(dataset == "GSE196696"){
    geoMeta <- get_meta_info_GSE196696(gdata, i)
  }
  
  return(geoMeta)
}


get_meta_info_GSE196696 <- function(gdata, i){
  
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(gdata$`age:ch1`[i]) )
  df <- df %>% add_row("KEY"=c("GENDER"), "VALUE"=c(gdata$`Sex:ch1`[i])) 
  df <- df %>% add_row("KEY"=c("TIME"), "VALUE"=c(gdata$`batch:ch1`[i]) ) 

  return(df)
}

get_meta_info_GSE198904 <- function(geoMeta){
  diag <- geoMeta$`diagnosis:ch1`
  diag <- unlist(lapply(diag, function(x){
    if(x == "MDD"){
      "1"
    }else{
      "0"
    }
  }))
  
  
  nrows <- length(geoMeta$`age:ch1`)
  df <- NULL
  for(i in seq(1, nrows)){
    if(is.null(df)){
      df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(geoMeta$`age:ch1`[i]) )
      df <- df %>% add_row("KEY"=c("GENDER"), "VALUE"=c(geoMeta$`gender:ch1`[i])) 
      df <- df %>% add_row("KEY"=c("MAJOR_DEPRESSION"), "VALUE"=c(diag[i]) ) 
      df <- df %>% add_row("KEY"=c("ID"), "VALUE"=c(geoMeta$geo_accession[i]) ) 
    }else{
      df <- df %>% add_row("KEY"=c("AGE"), "VALUE"=c(geoMeta$`age:ch1`[i])) 
      df <- df %>% add_row("KEY"=c("GENDER"), "VALUE"=c(geoMeta$`gender:ch1`[i])) 
      df <- df %>% add_row("KEY"=c("MAJOR_DEPRESSION"), "VALUE"=c(diag[i]) ) 
      df <- df %>% add_row("KEY"=c("ID"), "VALUE"=c(geoMeta$geo_accession[i]) ) 
    }
  }
  

  return(df)
}