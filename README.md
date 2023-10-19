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
