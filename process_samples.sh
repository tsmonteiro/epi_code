#!/bin/bash
# 
# +-----------+-------------+----------+
# | id        | description | platform |
# +-----------+-------------+----------+
# | GSE123914 | Variation   | EPIC     |69
# | GSE132203 | DNA         | EPIC     |150
# | GSE141682 | DNA         | EPIC     |
# | GSE143307 | Epigenomics | EPIC     |
# | GSE147740 | DNA         | EPIC     |
# | GSE155426 | DNA         | EPIC     |
# | GSE164056 | DNA         | EPIC     |
# | GSE167202 | Host        | EPIC     |
# | GSE179325 | COVID-19    | EPIC     |
# | GSE197675 | Genome-wide | EPIC     |
# | GSE210255 | Methylation | EPIC     |
# +-----------+-------------+----------+

# O codigo comentado serve pra apagar as tabelas ao processar os dados de novo, caso necessário
#for GEOID in GSE154683 #GSE123914 GSE132203 GSE141682 GSE143307 GSE147740 GSE155426 GSE164056 GSE167202 GSE179325 GSE197675 GSE210255
#do
#   mysql --user="root" --password="root" --database="betas" --execute="drop table Pessoa_Sonda_${GEOID};"
#   mysql --user="root" --password="root" --database="betas" --execute="create table Pessoa_Sonda_${GEOID} (id_pessoa SMALLINT UNSIGNED,
#      id_sonda MEDIUMINT UNSIGNED, norm_beta_val MEDIUMINT UNSIGNED );"
#   # mysql --user="root" --password="root" --database="betas" --execute="ALTER TABLE Pessoa_Sonda_${GEOID} ADD INDEX (id_pessoa);"
#	 # mysql --user="root" --password="root" --database="betas" --execute="ALTER TABLE Pessoa_Sonda_${GEOID} ADD INDEX (id_sonda);"
#done

for I in {1..40}
do

    let "S = I * 50 +1"
    echo "${I}/50 [$S]"
    for GEOID in GSE210255 GSE147740 #GSE154683 GSE123914 GSE132203 GSE141682 GSE143307 GSE147740 GSE155426 GSE164056 GSE167202 GSE179325 GSE197675 GSE210255
    do
      echo "${GEOID}"
      LOG_OUT=$(printf "/home/thiago/workspaces/epigenica/projects/data_retrieval/logs/${GEOID}_PROCESS_%03d.log" $S)
      Rscript process_samples.R $S 50 ${GEOID} > $LOG_OUT
    done

done

