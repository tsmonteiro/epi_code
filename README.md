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
##docker run --name mariadbtest -e MYSQL_ROOT_PASSWORD=mypass -p 3306:3306 -d docker.io/library/mariadb:10.3

# Create cassandra Network
docker network create epigenica

# USe run to create
docker rm epigenica_mariadb
# not mounting this garbage....
# /home/thiago/workspaces/epigenica/epdb

docker run --detach --network epigenica  -v /home/thiago/workspaces/epigenica/epdb:/var/lib/mysql --name  epigenica_mariadb2 --env MARIADB_USER=admin --env MARIADB_PASSWORD=admin --env MARIADB_ROOT_PASSWORD=root mariadb:10.11 

 docker start epigenica_mariadb2

#/media/thiago/BackupDisk/epigenica_db


 # Accessing the db
docker run -it --network epigenica --rm mariadb:10.11 mysql -hepigenica_mariadb2 -uroot -p

set global max_allowed_packet=100000000;
# Exportar backup

docker exec epigenica_mariadb mysqldump --user root betas > /home/thiago/workspaces/epigenica/data/betas_db_bkp.sql

# Importar 
# NOTE DB betas precisa ter sido criado já
docker exec epigenica_mariadb mysql --user root betas < /home/thiago/workspaces/epigenica/data/betas_db_bkp.sql


#set global ulimit nofile=262144:262144;

# Finding the IP address
docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' epigenica_mariadb