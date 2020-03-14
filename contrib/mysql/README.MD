## Export d'une base GeneWeb v6 dans une base MariaDB

### Création de la base MariaDB
```
sudo mysql -u root -p
```
Renseigner le mot de passe puis valider avec `Entrée`
Puis, dans la console `MariaDB>` :
```
create database geneweb character set UTF8;
create user 'gw'@'localhost' identified by 'gw_pw';
grant all privileges on geneweb.* to 'gw'@'localhost';
exit
```

### Compilation

> make

> cd contrib/mysql

> ./compile.sh

### GeneWeb vers MariaDB

> ./run.sh

> ./run2.sh

### Tests

> ./mysql.sh -t < test.sql

> ./mysql.sh -t < test-asc.sql
