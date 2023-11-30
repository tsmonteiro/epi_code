library(RMariaDB)
library(DBI)
library(openssl)
library(dplyr)
library(tidyr)
library(data.table)

con <- NULL
isBatch <- FALSE

start_batch <- function(){
  # Espaco global!
  con <<- get_connection()
  isBatch <<- TRUE
}

end_batch <- function(){
  close_connection(con)
  rm("con")
}

get_connection <- function(){
  con <- dbConnect(RMariaDB::MariaDB(), group = "epigenica_db2", user='epi_admin', 
                        # password=rstudioapi::askForPassword("Database password"),
                        password="admin", 
                        host = "127.0.0.1", port="3306", dbname="betas")  


  return(con)
}

close_connection <- function(con){
  dbDisconnect(con)
  con <- NULL
}


print_dataset_counts <- function(){
  con <- get_connection()
  
  stm <- paste0("SELECT id
                FROM Dataset ;")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  
  listIds <- c()
  total <- 0
  for(id in resDf %>% pull(id)){
    stm <- paste0("SELECT DISTINCT
                  id_pessoa
                FROM Pessoa_Sonda_", id, ";")
    res <- dbSendQuery(con, stm)
    dsDf <- dbFetch(res)
    dbClearResult(res)
    
    print(paste0(id, ": ", nrow(dsDf)))
    total <- nrow(dsDf) + total
    listIds <- append(listIds, dsDf %>% pull(id_pessoa))

  }
  total

  close_connection(con)
  
}

create_table <- function( dataset ){
  #"create table Pessoa_Sonda_${GEOID} (id_pessoa SMALLINT UNSIGNED,
  #      id_sonda MEDIUMINT UNSIGNED, norm_beta_val MEDIUMINT UNSIGNED );"
  con <- get_connection()
  stm <- paste0("SELECT EXISTS (
    SELECT TABLE_NAME
    FROM information_schema.TABLES
    WHERE
    TABLE_NAME = 'Pessoa_Sonda_",
    dataset, 
    "');" )
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)

  names(resDf) <- "EXISTS"
  
  

  
  if( resDf$EXISTS[1] == 0){
    print(paste0("Creating table for ", dataset))
    stm <- paste0("Create table Pessoa_Sonda_",
                  dataset, " (id_pessoa SMALLINT UNSIGNED,
                   id_sonda MEDIUMINT UNSIGNED, 
                  norm_beta_val MEDIUMINT UNSIGNED );" )
    res <- dbSendQuery(con, stm)
    dbClearResult(res)
  }else{
    print(paste0("Table for ", dataset, " already exists"))
  }

  close_connection(con)
}
get_full_pessoas_full_meta <- function(  id_dataset ){
  # browser()
  con <- get_connection()


  stm <- paste0("SELECT DISTINCT
                  Pessoa.id as ID_PESSOA,
                  Pessoa.geoid as GEOID_PESSOA,
                  Pessoa.id_dataset as ID_DATASET,
                  Pessoa_MetaDado.id_metadado as METADADO,
                  Pessoa_MetaDado.val as VALUE
                FROM Pessoa 
                LEFT JOIN Pessoa_MetaDado 
                ON Pessoa.id = Pessoa_MetaDado.id_pessoa
                WHERE Pessoa.id_dataset = ?
                ORDER BY ID_PESSOA;")
  res <- dbSendQuery(con, stm, params=id_dataset)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  
  
  pessoaDf <- resDf %>%
    group_by(METADADO) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider( names_from = c("METADADO"), values_from = c("VALUE") ) %>%
    select(-row)
  
  
  dupId <- which(duplicated(pessoaDf$GEOID_PESSOA))
  if( length(dupId) > 0){
    pessoaDf <- pessoaDf %>%
      slice(-dupId)  
  }
  
  
  close_connection(con)
  
  return(pessoaDf)
  
}

get_full_pessoas_com_meta <- function( metas, id_dataset=NULL ){
  # browser()
  con <- get_connection()
  metas <- append(metas, "TIME")
  # NOTA: Mais facil limitar depois de fazer o pivot dos dados...
  if( !is.null(id_dataset)){
    stm <- paste0("SELECT DISTINCT
                    Pessoa.id as ID_PESSOA,
                    Pessoa.geoid as GEOID_PESSOA,
                    Pessoa.id_dataset as ID_DATASET,
                    Pessoa_MetaDado.id_metadado as METADADO,
                    Pessoa_MetaDado.val as VALUE
                  FROM Pessoa 
                  LEFT JOIN Pessoa_MetaDado 
                  ON Pessoa.id = Pessoa_MetaDado.id_pessoa
                  WHERE Pessoa_MetaDado.id_metadado in (?) 
                  ORDER BY ID_PESSOA;")
    res <- dbSendQuery(con, stm, params=list(metas))
  }else{
    stm <- paste0("SELECT DISTINCT
                    Pessoa.id as ID_PESSOA,
                    Pessoa.geoid as GEOID_PESSOA,
                    Pessoa.id_dataset as ID_DATASET,
                    Pessoa_MetaDado.id_metadado as METADADO,
                    Pessoa_MetaDado.val as VALUE
                  FROM Pessoa 
                  LEFT JOIN Pessoa_MetaDado 
                  ON Pessoa.id = Pessoa_MetaDado.id_pessoa
                  ORDER BY ID_PESSOA;")
    res <- dbSendQuery(con, stm)
    
  }
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  
  
  pessoaDf <- resDf %>%
    group_by(METADADO) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider( names_from = c("METADADO"), values_from = c("VALUE") ) %>%
    select(-row)
  
  if( !is.null(id_dataset)){
    pessoaDf <- pessoaDf %>%
      filter(ID_DATASET == id_dataset)
  }
  
  dupId <- which(duplicated(pessoaDf$GEOID_PESSOA))
  if( length(dupId) > 0){
    pessoaDf <- pessoaDf %>%
      slice(-dupId)  
  }
  

  close_connection(con)
  
  return(pessoaDf)
}


get_pessoas_id_2 <- function( geoids ){
  #select * from Pessoa Left Join Pessoa_MetaDado on Pessoa.id = Pessoa_MetaDado.id_pessoa Limit 3 Offset 3;
  con <- get_connection()
  
  # NOTA: Mais facil limitar depois de fazer o pivot dos dados...
  stm <- paste0("SELECT DISTINCT
                  Pessoa.id,
                  Pessoa.geoid
                FROM Pessoa 
                WHERE Pessoa.geoid = ? ORDER BY Pessoa.id;")
  res <- dbSendQuery(con, stm, params=list(geoids))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  close_connection(con)
  
  return(resDf)
}

get_pessoas_id <- function( id_dataset ){
  #select * from Pessoa Left Join Pessoa_MetaDado on Pessoa.id = Pessoa_MetaDado.id_pessoa Limit 3 Offset 3;
  con <- get_connection()
  
  if(!is.null(id_dataset)){

    stm <- paste0("SELECT DISTINCT
                  Pessoa.id,
                  Pessoa.geoid
                FROM Pessoa 
                WHERE Pessoa.id_dataset = ? ORDER BY Pessoa.id;")
    res <- dbSendQuery(con, stm, params=list(id_dataset))
    resDf <- dbFetch(res)
  }else{

    stm <- paste0("SELECT DISTINCT
                  Pessoa.id,
                  Pessoa.geoid
                FROM Pessoa")
    res <- dbSendQuery(con, stm)
    resDf <- dbFetch(res)
  }

  dbClearResult(res)
  
  close_connection(con)
  
  return(resDf)
}

get_dataset_list <- function(  ){
  #select * from Pessoa Left Join Pessoa_MetaDado on Pessoa.id = Pessoa_MetaDado.id_pessoa Limit 3 Offset 3;
  con <- get_connection()
  
  # NOTA: Mais facil limitar depois de fazer o pivot dos dados...
  stm <- paste0("SELECT *
                FROM Dataset;")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  close_connection(con)
  
  return(resDf)
}


get_list_pessoas_id <- function( id_dataset, off=0, n=1 ){
  #select * from Pessoa Left Join Pessoa_MetaDado on Pessoa.id = Pessoa_MetaDado.id_pessoa Limit 3 Offset 3;
  con <- get_connection()
  
  # NOTA: Mais facil limitar depois de fazer o pivot dos dados...
  stm <- paste0("SELECT DISTINCT
                  Pessoa.id,
                  Pessoa.geoid
                FROM Pessoa 
                LEFT JOIN Pessoa_MetaDado 
                ON Pessoa.id = Pessoa_MetaDado.id_pessoa
                WHERE Pessoa.id_dataset = ? ORDER BY Pessoa.id 
                LIMIT ", off, ",", off+n, ";")
  res <- dbSendQuery(con, stm, params=list(id_dataset))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  close_connection(con)
  
  return(resDf)
}


get_pessoas_com_meta <- function( pessoas_id ){
  #select * from Pessoa Left Join Pessoa_MetaDado on Pessoa.id = Pessoa_MetaDado.id_pessoa Limit 3 Offset 3;
  con <- get_connection()
  
  # NOTA: Mais facil limitar depois de fazer o pivot dos dados...
  stm <- paste0("SELECT 
                  *
                FROM Pessoa 
                LEFT JOIN Pessoa_MetaDado 
                ON Pessoa.id = Pessoa_MetaDado.id_pessoa
                WHERE Pessoa.id IN (?);")
  res <- dbSendQuery(con, stm, params=list(pessoas_id))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  pessoaDf <- tidyr::pivot_wider(data = resDf, names_from = c("id_metadado"), values_from = c("val"))
  
  
  
  stm <- paste0("SELECT 
                  Pessoa.id, 
                  cast(PessoaRawData.red_idat as char) as red_idat, 
                  PessoaRawData.red_idat_fname, 
                  cast(PessoaRawData.green_idat as char) as green_idat, 
                  PessoaRawData.green_idat_fname
                FROM Pessoa 
                LEFT JOIN PessoaRawData 
                ON Pessoa.id = PessoaRawData.id_pessoa
                WHERE Pessoa.id IN (?) ;")
  res <- dbSendQuery(con, stm, params=list(pessoas_id))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  for(i in seq(1, nrow(resDf)) ){
    # browser()
    resDf$red_idat_raw[i] <- list(openssl::base64_decode(  resDf$red_idat[i] ) )
    resDf$green_idat_raw[i] <- list(openssl::base64_decode(  resDf$green_idat[i] ) )
  }
  
  resDf$red_idat <- NULL
  resDf$green_idat <- NULL
  
  df <- pessoaDf %>%
    left_join(resDf, by="id")
  
  close_connection(con)
  
  return(df)
}
# 
# get_pessoas_com_meta <- function( id_dataset, n=100, offset=0 ){
#  #select * from Pessoa Left Join Pessoa_MetaDado on Pessoa.id = Pessoa_MetaDado.id_pessoa Limit 3 Offset 3;
#   con <- get_connection()
#   
#   # NOTA: Mais facil limitar depois de fazer o pivot dos dados...
#   stm <- paste0("SELECT 
#                   *
#                 FROM Pessoa 
#                 LEFT JOIN Pessoa_MetaDado 
#                 ON Pessoa.id = Pessoa_MetaDado.id_pessoa
#                 WHERE Pessoa.id_dataset = ?;")
#   res <- dbSendQuery(con, stm, params=list(id_dataset))
#   resDf <- dbFetch(res)
#   dbClearResult(res)
#  
#   pessoaDf <- tidyr::pivot_wider(data = resDf, names_from = c("id_metadado"), values_from = c("val"))
#   
#   t0 <- 1 + offset
#   tf <- t0 + n
#   if( tf > nrow(pessoaDf)){
#     tf <- nrow(pessoaDf)
#   }
#   
#   pessoaDf <- pessoaDf %>%
#     dplyr::slice(t0:tf)
#   
#   ids <- pessoaDf$id
#   
#   
#   
#   
#   stm <- paste0("SELECT 
#                   Pessoa.id, 
#                   cast(PessoaRawData.red_idat as char) as red_idat, 
#                   PessoaRawData.red_idat_fname, 
#                   cast(PessoaRawData.green_idat as char) as green_idat, 
#                   PessoaRawData.green_idat_fname
#                 FROM Pessoa 
#                 LEFT JOIN PessoaRawData 
#                 ON Pessoa.id = PessoaRawData.id_pessoa
#                 WHERE Pessoa.id IN (?) ;")
#   res <- dbSendQuery(con, stm, params=list(ids))
#   resDf <- dbFetch(res)
#   dbClearResult(res)
#   
#   for(i in seq(1, nrow(resDf)) ){
#     # browser()
#     resDf$red_idat_raw[i] <- list(openssl::base64_decode(  resDf$red_idat[i] ) )
#     resDf$green_idat_raw[i] <- list(openssl::base64_decode(  resDf$green_idat[i] ) )
#   }
#   
#   resDf$red_idat <- NULL
#   resDf$green_idat <- NULL
#   
#   df <- pessoaDf %>%
#     left_join(resDf, by="id")
#    
#   close_connection(con)
#   
#   return(df)
# }

# select * from Pessoa Left Join Pessoa_MetaDado on Pessoa.id = Pessoa_MetaDado.id_pessoa Limit 3 Offset 3
get_pessoa_batch <- function( id_list, meta_list){
  con <- get_connection()

  stm <- paste0("SELECT 
                  Pessoa.id, 
                  cast(PessoaRawData.red_idat as char) as red_idat, 
                  PessoaRawData.red_idat_fname, 
                  cast(PessoaRawData.green_idat as char) as green_idat, 
                  PessoaRawData.green_idat_fname
                FROM Pessoa 
                LEFT JOIN PessoaRawData 
                ON Pessoa.id = PessoaRawData.id_pessoa
                LIMIT 10;")
  
  
  # res <- dbSendQuery(con, stm,
  #                    params=list(id_list))
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)

  id_list <- resDf$id
  
  
  stm <- paste0("SELECT 
                    Pessoa.id,
                    Pessoa.geoid,
                    Pessoa.id_dataset,
                    Pessoa_MetaDado.id_metadado,
                    Pessoa_MetaDado.val
                FROM Pessoa 
                LEFT JOIN Pessoa_MetaDado
                ON Pessoa.id = Pessoa_MetaDado.id_pessoa
                WHERE Pessoa.id IN (", 
                  paste(rep("?", length(id_list)), collapse = ",")
                ,") AND
                   Pessoa_MetaDado.id_metadado IN (", 
                   paste(rep("?", length(meta_list)), collapse = ",")
                ,");")
  res <- dbSendQuery(con, stm,
                     params=append(id_list, meta_list))
  resDf2 <- dbFetch(res)
  dbClearResult(res)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }
  
  for(i in seq(1, nrow(resDf)) ){
    # browser()
    resDf$red_idat_raw[i] <- list(openssl::base64_decode(  resDf$red_idat[i] ) )
    resDf$green_idat_raw[i] <- list(openssl::base64_decode(  resDf$green_idat[i] ) )
  }
  
  resDf$red_idat <- NULL
  resDf$green_idat <- NULL

  return(list(resDf, resDf2))
}

get_pessoas_dataset <- function(id_dataset){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT * FROM Pessoa Where id_dataset = ?;")
  res <- dbSendQuery(con, stm, params=list(id_dataset))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }
  
  return(resDf)
}

get_dataset_list <- function(){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT * FROM Dataset;")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }
  
  return(resDf)
}


update_meta_dados <- function(id_pessoa, meta_dados){
  # Verificar se existe, daí altera, ou insere
}

get_pessoa <- function(id_pessoa){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT id_pessoa FROM PessoaRawData WHERE id_pessoa = '", 
                id_pessoa, "';")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }

  return(resDf)
}

get_pessoa <- function(id_pessoa){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT id_pessoa FROM PessoaRawData WHERE id_pessoa = '", 
                id_pessoa, "';")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }
  
  return(resDf)
}

get_idat <- function(id_pessoa){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT * FROM PessoaRawData WHERE id_pessoa = '", 
                id_pessoa, "';")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }

  resDf$green_idat <- list(openssl::base64_decode(resDf$green_idat[[1]]))
  resDf$red_idat <- list(openssl::base64_decode(resDf$red_idat[[1]]))
  
  return(resDf)
  
}


clear_dataset_data <- function(id_dataset){
  con <- get_connection()
  
  stm <- paste0("SELECT id FROM Pessoa WHERE id_dataset = '", 
                id_dataset, "';")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if(nrow(resDf) == 0){
    return(0)
  }

  stm <- paste0("DELETE FROM Pessoa WHERE id = '?';")
  res <- dbSendQuery(con, stm, params=list(resDf$id))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  stm <- paste0("DELETE FROM Pessoa_MetaDado WHERE id_pessoa = '?';")
  res <- dbSendQuery(con, stm, params=list(resDf$id))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  stm <- paste0("DELETE FROM Pessoa_Sonda WHERE id_pessoa = '?';")
  res <- dbSendQuery(con, stm, params=list(resDf$id))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  close_connection(con)

  
  return(resDf)
}


add_pessoa <- function(platform, id_dataset, dataset_desc, id, red_idat, fname_red, green_idat, fname_green){
  #SELECT LAST_INSERT_ID();
  # browser()
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  dbBegin(con)
  
  stm <- paste0("SELECT id FROM Dataset WHERE id = '", 
                id_dataset, "';")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if( nrow(resDf) == 0){
    stm <- paste0('INSERT INTO ', 
                  'Dataset(id, description, platform)',
                  ' VALUES (?, ?, ?);' )
    res <- dbSendStatement(con, stm, 
                           params=list(
                             id_dataset,
                             dataset_desc,
                             platform
                           ))
    dbClearResult(res)
  }
  
  # stm <- paste0("SELECT count(id) FROM Pessoa WHERE id = '", 
  #               geoid, "';")
  # res <- dbSendQuery(con, stm)
  # resDf <- dbFetch(res)
  # dbClearResult(res)

  # if( resDf$`count(id)` > 0 ){
  #   id <- paste0( id, '_', (resDf$`count(id)`+1)  )
  # }else{
  #   id <- paste0( id, '_1'  )
  # }
  
  
  stm <- paste0('INSERT INTO ', 
                'Pessoa(geoid, id_dataset)',
                ' VALUES (?, ?);')
  res <- dbSendStatement(con, stm,
                         params=list(id, id_dataset))
  dbClearResult(res)

  res <- dbSendStatement(con, 'SELECT LAST_INSERT_ID();')
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  iid <- resDf$`LAST_INSERT_ID()`  
  if( !is.null(red_idat)){
    stm <- paste0('INSERT INTO ', 
                  'PessoaRawData(id_pessoa, red_idat, red_idat_fname,  green_idat, green_idat_fname)',
                  ' VALUES ( ?, ?, ?, ?, ? );' )
                  
   
    res<- dbSendStatement(con, stm, params=list(iid, 
                                          openssl::base64_encode(as.raw(red_idat)),
                                          fname_red,
                                          openssl::base64_encode(as.raw(green_idat)),
                                          fname_green
                                          ))
    
    dbClearResult(res)
  }
  dbCommit(con)
  
  # res <- dbSendQuery(con, "SELECT * FROM Pessoa WHERE sexo = 'G'")
  # dbFetch(res)
  # dbClearResult(res)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }
  return(iid)
}

add_metadados_single <- function(sample_id, key, value){
  con <- get_connection()

  stm <- "SELECT * FROM Pessoa_MetaDado WHERE id_pessoa = ? AND id_metadado = ?;"
  res<- dbSendStatement(con, stm, params=list(sample_id, key))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  if( is.null(resDf) || nrow(resDf) == 0){
    stm <- paste0('INSERT INTO ', 
                  'Pessoa_MetaDado(id_pessoa, id_metadado, val)',
                  ' VALUES (?,?,?);' )
    
    res <- dbSendStatement(con, stm, params=list(sample_id, key, value))  
  }else{
    stm <- paste0('UPDATE Pessoa_MetaDado ', 
                  'SET val=? ',
                  'WHERE id_pessoa=? AND id_metadado=?;' )
    
    res <- dbSendStatement(con, stm, params=list(value, sample_id, key ))  
  }

  dbClearResult(res)
  
  close_connection(con)
}

add_metadados <- function(sample_id, meta_df){
  
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  
  dbBegin(con)
  # for( meta_key in meta_df$KEY){
  #   stm <- paste0("SELECT id FROM MetaDado WHERE id = ?;")
  #   res <- dbSendQuery(con, stm, params=list(meta_key))
  #   resDf <- dbFetch(res)
  #   dbClearResult(res)
  #   
  #   if(nrow(resDf) == 0){
  #     stm <- paste0('INSERT INTO ', 
  #                   'MetaDado(id)',
  #                   ' VALUES (?);' )
  # 
  #     res <- dbSendStatement(con, stm, params=list(meta_key))
  #     dbClearResult(res)
  #   }
  #   
  # }
  
  
  for(i in seq(1,nrow(meta_df))){
    stm <- paste0('INSERT INTO ', 
                  'Pessoa_MetaDado(id_pessoa, id_metadado, val)',
                  ' VALUES (?,?,?);' )

    res <- dbSendStatement(con, stm, params=list(sample_id, meta_df$KEY[i], meta_df$VALUE[i]))  
    dbClearResult(res)
  }
  
  
  
  dbCommit(con)
  
  if( isBatch == FALSE ){
    close_connection(con)
  }
}

get_lista_pessoas <- function(){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT id, geoid FROM Pessoa;")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  return(resDf)
}


get_sondas_by_names <- function(name_list){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT * FROM Sonda WHERE name_sonda IN (?);")
  res <- dbSendQuery(con, stm, params=list(name_list))
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  return(resDf)
}


get_sondas <- function(){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT * FROM Sonda;")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)
  
  return(resDf)
}



add_sondas <- function( probe_names ){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT name_sonda FROM Sonda;")
  res <- dbSendQuery(con, stm)
  resDf <- dbFetch(res)
  dbClearResult(res)

  newProbes <- setdiff(probe_names, resDf$name_sonda)

  if(length(newProbes) == 0){
    return()
  }
  
  dbBegin(con)

  stm <- paste0("INSERT INTO
                    Sonda(name_sonda)
                    VALUES (?);")
  res <- dbSendStatement(con, stm, params=list(newProbes))
    
  dbClearResult(res)
  
  dbCommit(con)
  if( isBatch == FALSE ){
    close_connection(con)
  }
}


# Adiciona todos os betas de um contidos nos parametros
add_betas <- function( dataset_id, pessoa_id, pessoa_sondas, betas, betas_norm ){
  all_probe_names <- get_sondas()

  all_probe_ids <- unname(unlist(as.list(all_probe_names["id"])))
  all_probe_names <- unname(unlist(as.list(all_probe_names["name_sonda"])))

  con <- get_connection()

  names(all_probe_ids) <- all_probe_names

  iprobes <- intersect(all_probe_names, pessoa_sondas)
  iprobeids <- unname(all_probe_ids[iprobes])
  missing_probes <- all_probe_ids[! all_probe_names %in% iprobes]

  
  
  
  # BATCH transaction (less log space used)
  stp <- 10000 #length(iprobes) 
  p0 <- 1
  pf <- p0 + stp -1
  # dbBegin(con)
  dbSendStatement(con, "START TRANSACTION")
  while(TRUE){
    if( p0 >= pf){
      break
    }
      
    tmp_sondas <- all_probe_names[p0:pf]
    tmp_sondas_ids <- all_probe_ids[p0:pf]

    stm <- paste0("INSERT INTO
                  Pessoa_Sonda_", dataset_id, " (id_pessoa, id_sonda, norm_beta_val)
                  VALUES (?,?,?);")      

    res <- dbSendStatement(con, stm, 
                           params=list(
                             replicate(length(tmp_sondas), pessoa_id, simplify = TRUE),
                             tmp_sondas_ids,
                             as.integer( betas_norm[tmp_sondas] * 16777213 ) )
                           )
    dbClearResult(res)
    
      
    p0 <- p0 + stp
    pf <- pf + stp
      
    if( pf > length(iprobes)){
      pf <- length(iprobes)
    }
    
  }
  dbSendStatement(con, "COMMIT")
  # dbCommit(con)
  if(length(missing_probes) > 0 ){
    stm <- paste0("INSERT INTO
                  Pessoa_Sonda_", dataset_id, "(id_pessoa, id_sonda, norm_beta_val)
                  VALUES (?,?,?);")      
    res <- dbSendStatement(con, stm, 
                           params=list(
                             replicate(length(missing_probes), pessoa_id, simplify = TRUE),
                             missing_probes,
                             seq(1, length(missing_probes)) * 0
                           ))
    dbClearResult(res)
    

  }
  
  

  if( isBatch == FALSE ){
    close_connection(con)
  }

}





get_betas_sondas <- function( dataset_list, sondasDf,  normalized=T ){
  resDt <- NULL
  id_sondas <- sondasDf$id
  d <- dataset_list[1]
  
  for( d in dataset_list){
    # TODO <- Do this person-wise
    # if(d != "GSE155426" ){
    #   next
    #   
    # }
    
   tryCatch({  
      pessoasId = get_pessoas_dataset(d)
      if( nrow(pessoasId) == 0){
        print(paste0(d, " is empty. Skipping"))
        next
      }
      con <- get_connection()
      
      stp <- 100
      p0  <- 1
      pf  <- p0 + stp -1
      pDf <- NULL
      while(TRUE){
        if( pf >= length(pessoasId$id) ){
          pf <- length(pessoasId$id)
        }
        
        if(p0 >= pf){
          break
        }
        # Read participants here
        print(paste0("Reading ", d, " (", p0, " - ", pf, "/", length(pessoasId$id) , ")"))
        stm <- paste0("SELECT  
                  id_sonda, id_pessoa, norm_beta_val
                FROM Pessoa_Sonda_", d, " WHERE id_pessoa IN (")  
        ak <-pf-p0+1
        for( k in seq(1,ak)){
          #stm <- paste0(stm, '?')
          stm <- paste0(stm, pessoasId$id[p0+k-1])
          
          if(k<ak){
            stm <- paste0(stm, ',')  
          }
        }
        stm <- paste0(stm, ');')
        
        
        
        res <- dbSendQuery(con, stm)
        resDf <- dbFetch(res)
        dbClearResult(res)
        
        if( is.null(pDf)){
          pDf <- resDf %>%
            filter(id_sonda %in% sondasDf$id) %>%
            drop_na()
          
        }else{
          
          pDf <- rbind(pDf, 
                       resDf %>%
                         filter(id_sonda %in% sondasDf$id) %>%
                         drop_na())
        }
        
        p0 <- p0 + stp
        pf <- pf + stp
      }
      
      print("DONE reading. Processing.")
      if( nrow(pDf) == 0){
        print(paste0(d, " nao tem sondas processadas. Pulando"))
        next
      }
      
      rm(resDf)
      
      #occurences<-table(unlist(tmp$id_pessoa))
      
      
      names(sondasDf) <- c("id_sonda", "name_sonda")
      t0 <- Sys.time()
      tvalsNA <- rep(NA, length(sondasDf$id_sonda))
      
      # dt <- as.data.frame(matrix(unlist(lapply( unique(pDf$id_pessoa), function(id){
      #   #t0 <- Sys.time()
      #   tvals <- tvalsNA
      #   names(tvals) <- sondasDf$name_sonda
      #   
      #   tvals_tmp <- pDf %>% filter(id_pessoa == id) %>% select(id_sonda, norm_beta_val) %>% inner_join(sondasDf, by = "id_sonda")
      #   tvals[tvals_tmp$name_sonda] <- (tvals_tmp$norm_beta_val)
      #   tvals
      # } )), nrow = length( unique(pDf$id_pessoa) )))
      
      print("Building df.")
      dt <- data.frame(t(matrix(unlist(lapply( unique(pDf$id_pessoa), function(id){
        #t0 <- Sys.time()
        tvals <- tvalsNA
        names(tvals) <- sondasDf$name_sonda
        
        tvals_tmp <- pDf %>% filter(id_pessoa == id) %>% select(id_sonda, norm_beta_val) %>% inner_join(sondasDf, by = "id_sonda")
        tvals[tvals_tmp$name_sonda] <- as.numeric(tvals_tmp$norm_beta_val) / 16777213
        tvals
      } )), nrow = length(sondasDf$id_sonda) )))
      
      names(pDf) <- c("id_sonda", "id", "norm_beta_val")
      pids <- pessoasId %>% inner_join(pDf, by="id")
      rownames(dt) <- pids %>% filter(id_dataset == d) %>% pull(geoid) %>% unique
      
      print("DONE.")
      if(is.null(resDt)){
        resDt <- dt
      }else{
        
        cols <- intersect(colnames(resDt), colnames(dt))
        resDt <- rbind(resDt[cols], dt[cols])
      }
      
      tf <- Sys.time()
      print(paste0("Done in ", tf-t0))
      rm(dt)
      rm(pDf)
      gc()
      dbDisconnect(con)      
    },
    error=function(cond){
      if(!is.null(con)){
        dbDisconnect(con)
      }

      print(paste0(d, " FAILED"))
      print(cond)
    })

  } #END for dataser
  colnames(resDt)<- sondasDf$name_sonda
  return(resDt)

} # END funcion

get_betas <- function( id_pessoa, normalized=T ){
  if( isBatch == FALSE ){
    con <- get_connection()
  }
  stm <- paste0("SELECT 
                  norm_beta_val
                FROM Pessoa_Sonda 
                WHERE id_pessoa = ?;")
  res <- dbSendQuery(con, stm,
                     params=id_pessoa)
  resDf <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
}
