library(RMariaDB)
library(DBI)
 #rstudioapi::askForPassword("Database password")
# docker : "172.18.0.2"
con <- dbConnect(RMariaDB::MariaDB(), group = "epigenica_db2", user='root', 
                 password="root",
                 host = "127.0.0.1", port="3306")

dbExecute(con, "DROP DATABASE betas")
dbExecute(con, "DROP DATABASE betas_test")
dbExecute(con, "CREATE DATABASE betas")
dbExecute(con, "CREATE DATABASE betas_test")
#
dbExecute(con, paste0("drop user epi_admin;"))
dbExecute(con, paste0("drop user admin;"))

dbExecute(con, paste0("flush privileges;"))

dbExecute(con, paste0("CREATE USER 'epi_admin' IDENTIFIED BY 'admin';"))

dbExecute(con, paste0("GRANT ALL PRIVILEGES ON betas.* TO epi_admin"))
#dbExecute(con, paste0("grant file on *.* to epi_admin identified by 'epi_admin'"))



dbExecute(con, paste0("flush privileges;"))

dbDisconnect(con)


con <- dbConnect(RMariaDB::MariaDB(), group = "epigenica_db2", user='epi_admin', 
                 password="admin",
                 host = "127.0.0.1", port="3306", dbname="betas")

res <- dbSendStatement(con, "CREATE TABLE Dataset (
                   id varchar(20) NOT NULL UNIQUE PRIMARY KEY,
                   description varchar(300) NOT NULL,
                   platform varchar(10) NOT NULL
)")
dbClearResult(res)

res <- dbSendStatement(con, "CREATE TABLE Pessoa (
                   id MEDIUMINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
                   geoid varchar(20) NOT NULL ,
                   id_dataset varchar(20) NOT NULL,
                   FOREIGN KEY (id_dataset) REFERENCES Dataset (id)
                      ON DELETE CASCADE
)")
dbClearResult(res)



res <- dbSendStatement(con, "CREATE TABLE PessoaRawData (
                   id MEDIUMINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
                   id_pessoa MEDIUMINT UNSIGNED NOT NULL,
                   red_idat MEDIUMBLOB NOT NULL,
                   red_idat_fname varchar(64) NOT NULL,
                   green_idat MEDIUMBLOB NOT NULL,
                   green_idat_fname varchar(64) NOT NULL,
                   FOREIGN KEY (id_pessoa) REFERENCES Pessoa (id)
                      ON DELETE CASCADE
)")

# affRows <- dbGetRowsAffected(res = res)
dbClearResult(res)


res <- dbSendStatement(con, "CREATE TABLE Pessoa_MetaDado (
                   id_pessoa MEDIUMINT UNSIGNED NOT NULL,
                   id_metadado varchar(50),
                   val varchar(50),
                   FOREIGN KEY (id_pessoa) REFERENCES Pessoa (id)
                      ON DELETE CASCADE
)")

dbClearResult(res)


res <- dbSendStatement(con, "CREATE TABLE Sonda (
                   id MEDIUMINT UNSIGNED AUTO_INCREMENT NOT NULL UNIQUE PRIMARY KEY,
                   name_sonda varchar(20)
)")

dbClearResult(res)




res <- dbSendStatement(con, "CREATE TABLE Pessoa_Sonda (
                   id_pessoa MEDIUMINT UNSIGNED NOT NULL,
                   id_sonda MEDIUMINT UNSIGNED NOT NULL,
                   beta_val FLOAT,
                   norm_beta_val FLOAT,
                   FOREIGN KEY (id_pessoa) REFERENCES Pessoa (id)
                      ON DELETE CASCADE,
                   FOREIGN KEY (id_sonda) REFERENCES Sonda (id)
                      ON DELETE CASCADE
)")

dbClearResult(res)

dbDisconnect(con)

