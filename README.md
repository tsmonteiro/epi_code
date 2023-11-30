## Descrição Arquivos

### Notas Gerais

Arquivos *.sh são scripts executados na linha de comando em sistemas Linux 
(potencialmente Apple, mas nunca tive oportunidade de testar). 

O R ou alguma das bibliotecas utilizada no download tem algum probleminha para
liberar memória. Consequentemente, foi preciso quebrar o download em blocos. 
Se o bloco for grande demais, corre-se o risco de ficar sem memória.

### I. Baixar IDAT's de Bases Públicas

Função realizada por dois arquivos: `download_idats.R` e `download_idats.sh`. 

O `download_idats.sh` é um script que executa `download_idats.R` para múltiplos 
ID's do GEOquery, além de já fazer a serapração do download por blocos.

O `download_idats.R` pode ser chamado pela linha de comando:
```R
Rscript download_idats.R ${I} ${J} ${GEOID} $GEODESC 

Exemplo:
Rscript download_idats.R 1 200 GSE210255 "Esse é o dataset A"
```

I é o primeiro índice a ser salvo e J o último. GEOID é o ID do GEOQuery e GEODESC
uma string com a descrição para o GEOID. 

Para inserção dos dados, é necessário que exista uma função correspondente no arquivo
`meta_funcs.R`

### II. Processar Dados em Bloco

Função realizada por dois arquivos: `process_samples.R` e `process_samples.sh`. 

O `process_samples.sh` é um script que executa `process_samples.R` para múltiplos 
ID's do GEOquery, além de já fazer a serapração do processamento por blocos.

O `process_samples.R` pode ser chamado pela linha de comando:
```R
Rscript process_samples.R $S ${BS} ${GEOID}

Exemplo:
Rscript process_samples.R 1 50 GSE210255 
```

S é o primeiro índice do processamento, BS o tamanho do bloco de processamento e 
GEOID o id do dataset para processar.


### III. Calcular Idade Epigenética

TODO


### IV. Processar Cliente

TODO


### Acesso ao Banco de Dados

Todas as funções para acessar o banco de dados estão contidas no arquivo `data_funcs.R`.

TODO Descrever funções principais

## Instalação

TODO



## Setting up mariadb

## FOR local mariadb
Install from apt (version 10.11)

CREATE root passw
sudo systemctl stop mariadb

sudo mysqld_safe --skip-grant-tables --skip-networking &
#export MYSQL_HISTFILE=/dev/null  <- executar essa linah se quiser que o passw do root nao aparece nos logs do sistema
mysql -u root

FLUSH PRIVILEGES;
ALTER USER 'root'@'localhost' IDENTIFIED BY 'root';
FLUSH PRIVILEGES;
exit;

ps -ef | grep mysqld # then kill all processes on the list

sudo systemctl start mariadb


docker network create epigenica
docker rm epigenica_mariadb

docker run --detach --network epigenica  -v /home/thiago/workspaces/epigenica/epdb:/var/lib/mysql --name  epigenica_mariadb2 --env MARIADB_USER=admin --env MARIADB_PASSWORD=admin --env MARIADB_ROOT_PASSWORD=root mariadb:10.11 


docker run -it --network epigenica --rm mariadb:10.11 mysql -hepigenica_mariadb2 -uroot -p

set global max_allowed_packet=100000000;

# Exportar backup
docker exec epigenica_db2 mysqldump --user root betas > /media/thiago/76E8-CACF/epigenica_db/betas_db_bkp.sql

# Importar 
# NOTE DB betas precisa ter sido criado já
docker exec epigenica_mariadb mysql --user root betas < /home/thiago/workspaces/epigenica/data/betas_db_bkp.sql


