# [1] "race: African American (AA)" "gender: F"                   "age(yrs): 54.787132101"      "plate: WG5125128-MSA4"      
# [5] "column: 1"                   "row: A"                      "pedigreeid: pedid_63"        "subjectid: subjectid_107"   
# [9] "phase: 1"     
# GSE210255


get_meta_dict <- function( meta_array, geoid ){
  if( geoid == 'GSE210255') {
    return(get_meta_dict_GSE210255(meta_array))
  }
  
  if( geoid == 'GSE167202') {
    return(get_meta_dict_GSE167202(meta_array))
  }
  
  if( geoid == 'GSE197676') {
    return(get_meta_dict_GSE197676(meta_array))
  }
  
  if( geoid == 'GSE147740') {
    return(get_meta_dict_GSE147740(meta_array))
  }
  
  if( geoid == 'GSE179325') {
    return(get_meta_dict_GSE179325(meta_array))
  }
  
  if( geoid == 'GSE197675') {
    return(get_meta_dict_GSE197675(meta_array))
  }
  
  if( geoid == 'GSE164056') {
    return(get_meta_dict_GSE164056(meta_array))
  }
  
  if( geoid == 'GSE199700') {
    return(get_meta_dict_GSE199700(meta_array))
  }
  
  if( geoid == 'GSE155426') {
    return(get_meta_dict_GSE155426(meta_array))
  }
  
  if( geoid == 'GSE123914') {
    return(get_meta_dict_GSE123914(meta_array))
  }
  
  if( geoid == 'GSE143307') {
    return(get_meta_dict_GSE143307(meta_array))
  }
  
  if( geoid == 'GSE154683') {
    return(get_meta_dict_GSE154683(meta_array))
  }
  
  if( geoid == 'GSE141682') {
    return(get_meta_dict_GSE141682(meta_array))
  }
  
  if( geoid == 'GSE132203') {
    return(get_meta_dict_GSE132203(meta_array))
  }
  
  
  
  
  
  
  
}

# NOTA
# Esse dataset contém participantes do estudo GENOA, que foi feito em duas fases
# participantes da segunda fase são os mesmos da primeira com uma diferença de aproximadamente 7 anos
# Existem dados clínicos e de estilo de vida que precisam ser requisitados e não foram incluidos como metadados aqui
get_meta_dict_GSE210255 <- function( meta_array ){
  
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  df <- data.frame( "KEY"=c("RACE"), "VALUE"=c(mdict[2]) )
  
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) ) )
  #plate
  
  mdict <- strsplit( meta_array[[9]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("TIME"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[7]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("PEDIGREEID"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[8]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GENOAID"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("PLATE"), "VALUE"=c(mdict[2]) ) )
  
  # mdict <- strsplit( meta_array[[9]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("HYPERTENSION"), "VALUE"="1") )
  
  return(df)
  
}


get_meta_dict_GSE147740 <- function( meta_array ){
  
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("AIRWAVE_ID"), "VALUE"=c(mdict[2]) ) )

  
  return(df)
}

get_meta_dict_GSE179325 <- function( meta_array ){
  # browser()
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  covid <- "0"
  
  if( str_detect(string = mdict[2], pattern = "positive")  ){
    covid<-"1"
  }
  df <- rbind( df, data.frame( "KEY"=c("COVID"), "VALUE"=c(covid) ) )
  

  mdict <- strsplit( meta_array[[5]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("COVID_STATUS"), "VALUE"=c(mdict[2]) ) )
  
  return(df)
  
}

get_meta_dict_GSE197675 <- function( meta_array ){
  
  if(length(meta_array) == 2 ){
    mdict <- strsplit( meta_array[[1]], ": " )[[1]]
    df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
    
    
    mdict <- strsplit( meta_array[[2]], ": " )[[1]]
    
    val <- str_replace(c(mdict[1]) , "ale", "")
    val <- str_replace(val , "em", "")
    
    df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(val) ) )
    
  }else{
    mdict <- strsplit( meta_array[[2]], ": " )[[1]]
    df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
    
    
    mdict <- strsplit( meta_array[[1]], ": " )[[1]]
    
    val <- str_replace(c(mdict[2]) , "ale", "")
    val <- str_replace(val , "em", "")
    
    df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(val) ) )
    
    mdict <- strsplit( meta_array[[3]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("ANTHRACYCLINES"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[4]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("ALKYLATING"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[5]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("CORTICOSTEROIDS"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[6]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("EPIPODOPHYLLOTOXINS"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[7]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("VINCRISTINE"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[8]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("PLATINUM"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[9]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("BRAINRADIOTH"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[10]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("CHESTRADIOTH"), "VALUE"=c(mdict[2]) ) )
    
    mdict <- strsplit( meta_array[[11]], ": " )[[1]]
    df <- rbind( df, data.frame( "KEY"=c("ABD_PELVIC_RADIOTH"), "VALUE"=c(mdict[2]) ) )
    
  }

  return(df)
  
}


get_meta_dict_GSE164056 <- function( meta_array ){
  # ELA: early life adversity
  # SAD: 	Social anxiety disorder
  # LSAS: Leibowitz Social Anxiety Scale
  # CTQ: Childhood Trauma Questionnaire
  #"age: 32"        "Sex: F"         "sad: yes"       "ela: yes"       "lsas_total: 60" "ctq_total: 92" 
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("SAD"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("ELA"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[5]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("LSAS"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[6]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("CTQ"), "VALUE"=c(mdict[2]) ) )
  
  return(df)
  
}


get_meta_dict_GSE199700 <- function( meta_array ){
  # ELA: early life adversity
  # SAD: 	Social anxiety disorder
  # LSAS: Leibowitz Social Anxiety Scale
  # CTQ: Childhood Trauma Questionnaire
  #"age: 32"        "Sex: F"         "sad: yes"       "ela: yes"       "lsas_total: 60" "ctq_total: 92" 
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  
  mdict1 <- strsplit( meta_array[[2]], ": " )[[1]]
  mdict2 <- strsplit( meta_array[[3]], ": " )[[1]]
  bpressure <- paste0(mdict1[2], '/', mdict2[2])
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(bpressure) ) )
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("STATURE"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[5]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("MASS"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[6]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("WAIST"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[7]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("HIP"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[13]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("URIC ACID"), "VALUE"=c(mdict[2]) ) )
  
  
  mdict <- strsplit( meta_array[[14]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("CHOLESTEROL"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[15]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GLUCOSE"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[16]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("HDL"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[17]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("LDL"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[18]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("TRIGLYCERIDES"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[19]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("INSULIN"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[27]], ": " )[[1]]
  t <- "1"
  
  if(mdict[2]!="PRE"){
    t<-"2"
  }
  df <- rbind( df, data.frame( "KEY"=c("TIME"), "VALUE"=c(t) ) )
  
  mdict <- strsplit( meta_array[[26]], ": " )[[1]]
  prediabetes <- "0"
  
  if(mdict[2]!="PG"){
    prediabetes<-"1"
  }
  df <- rbind( df, data.frame( "KEY"=c("TIME"), "VALUE"=c(prediabetes) ) )

  
  return(df)
  
}
get_meta_dict_GSE132203 <- function( meta_array ){
  # [1] "gender: Male"                                "age: 49"                                     "race: AA"                                   
  # [4] "mergedcapsandpsswinthin30days: 0"            "tei_total_types_experienced_somewitness: 10" "childabphyssexemot_ctq_01modandsev: 0"      
  # [7] "pc1: -0.00295382"                            "pc2: -7.97E-06"                              "pc3: -0.00211974"                           
  # [10] "cd8t: 0.175498251"                           "cd4t: 0.179390646"                           "nk: 0.0576606"                              
  # [13] "bcell: 0.06262448"                           "mono: 0.091617955"                           "neu: 0.433208068"                           
  # [16] "age acceleration: -4.094807929"  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  
  val <- str_replace(c(mdict[2]) , "ale", "")
  val <- str_replace(val , "em", "")
  val <- str_to_upper(val)
  
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=toupper(c(val) )) )
  
  
  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("RACE"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c(toupper("mergedcapsandpsswinthin30days")), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c(toupper("tei_total_types_experienced_somewitness")), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c(toupper("childabphyssexemot_ctq_01modandsev")), "VALUE"=c(mdict[2]) ) )
  
  
  
  return(df)  
}

get_meta_dict_GSE155426 <- function( meta_array ){
  # tbi: Traumatic Brain Injury
  # blast_tbi: Traumatic Brain Injury from explosive blast
  # breaching history: Blast explosure. High exposure arbitrarily defined as >= 40
  
  # [1] "tissue: Whole blood"         "session: Session 1 (site 3)" "time: pre"                   "age: 38"                     "Sex: male"                  
  # [6] "tbi: 1"                      "blast_tbi: 1"                "breaching history: 10-39"   
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  
  mdict <- strsplit( meta_array[[5]], ": " )[[1]]
  
  val <- str_replace(c(mdict[2]) , "ale", "")
  val <- str_replace(val , "em", "")
  val <- str_to_upper(val)
  
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=toupper(c(val) )) )
  
  
  
  mdict <- strsplit( meta_array[[6]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("TBI"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[7]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("BLAST_TBI"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[8]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("BREACHING_HISTORY"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  t <- "1"
  
  if(mdict[2]!="pre"){
    t<-"2"
  }
  df <- rbind( df, data.frame( "KEY"=c("TIME"), "VALUE"=c(t) ) )
  
  
  return(df)
  
}

get_meta_dict_GSE123914 <- function( meta_array ){

  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c("F") ) )
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  t <- "1"
  
  if(mdict[2]!="2014"){
    t<-"2"
  }
  df <- rbind( df, data.frame( "KEY"=c("TIME"), "VALUE"=c(t) ) )
  
  return(df)

}

get_meta_dict_GSE143307 <- function( meta_array ){
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  
  t <- "1"
  if(mdict[2]!="FALSE"){
    t <- "0"
  }
  df <- rbind( df, data.frame( "KEY"=c("VIT_K_RESPONDER"), "VALUE"=c(t) ) )
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("BASELINE_VIT_K"), "VALUE"=c(mdict[2]) ) )
  
  return(df)
  
}


get_meta_dict_GSE154683 <- function( meta_array ){
  
  mdict <- strsplit( meta_array[[5]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  mdict <- strsplit( meta_array[[6]], ": " )[[1]]
  
  val <- str_replace(c(mdict[2]) , "ale", "")
  val <- str_replace(val , "em", "")
  val <- str_to_upper(val)
  
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(val) ) )

  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  
  t <- "0"
  if(mdict[2]!="2"){
    t <- "1"
  }
  df <- rbind( df, data.frame( "KEY"=c("HEART_DISEASE"), "VALUE"=c(t) ) )
  
  t <- "0"
  if(mdict[2]!="2"){
    t <- "1"
  }
  df <- rbind( df, data.frame( "KEY"=c("OUTPATIENT_SURGERIES"), "VALUE"=c(t) ) )
  
  mdict <- strsplit( meta_array[[7]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("RACE"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[8]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("GENOTYPE"), "VALUE"=c(mdict[2]) ) )
  
  return(df)
  
}


get_meta_dict_GSE141682 <- function( meta_array ){
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  
  val <- str_replace(c(mdict[2]) , "ale", "")
  val <- str_replace(val , "em", "")
  val <- str_to_upper(val)
  
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(val) ) )
  

  
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("RACE"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[4]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("DISEASE_STATE"), "VALUE"=c(mdict[2]) ) )
  
  return(df)
  
}


get_meta_dict_GSE197676 <- function( meta_array ){
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  
  val <- str_replace(c(mdict[2]) , "ale", "")
  val <- str_replace(val , "em", "")
  val <- str_to_upper(val)
  
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=c(val) ) )
  
  
  return(df)
  
}

get_meta_dict_GSE167202 <- function(meta_array){
  mdict <- strsplit( meta_array[[3]], ": " )[[1]]
  df <- data.frame( "KEY"=c("AGE"), "VALUE"=c(mdict[2]) )
  
  
  mdict <- strsplit( meta_array[[2]], ": " )[[1]]
  
  val <- str_replace(c(mdict[2]) , "ale", "")
  val <- str_replace(val , "em", "")
  val <- str_to_upper(val)
  
  df <- rbind( df, data.frame( "KEY"=c("GENDER"), "VALUE"=toupper(c(val) )) )

  mdict <- strsplit( meta_array[[1]], ": " )[[1]]
  covid <- "1"
  
  if( str_detect(string = mdict[2], pattern = "negative")  ){
    covid<-"1"
  }
  df <- rbind( df, data.frame( "KEY"=c("COVID"), "VALUE"=c(covid) ) )
  
  mdict <- strsplit( meta_array[[28]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("DAYS_IN_HOSPITAL"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[26]], ": " )[[1]]
  icu <- "1"
  
  if( str_detect(string = mdict[2], pattern = "No")  ){
    icu<-"0"
  }
  df <- rbind( df, data.frame( "KEY"=c("ICU_ADMISSION"), "VALUE"=c(icu) ) )
  
  mdict <- strsplit( meta_array[[29]], ": " )[[1]]
  df <- rbind( df, data.frame( "KEY"=c("DISCHARGED_TO"), "VALUE"=c(mdict[2]) ) )
  
  mdict <- strsplit( meta_array[[25]], ": " )[[1]]
  asy <- "0"
  
  if( str_detect(string = mdict[2], pattern = "Yes")  ){
    asy<-"1"
  }
  df <- rbind( df, data.frame( "KEY"=c("ASYMPTOMATIC"), "VALUE"=c(asy) ) )
  
  return(df)  
}
