## Export d'une base GeneWeb dans une base MySQL

À force d'étudier des axes d'amélioration de GeneWeb, je me suis rendu compte qu'il est nécessaire de revoir le modèle de données. Un des axes d'amélioration est d'utiliser un système de gestion de base de données (SGBD) afin d'avoir une plus grande souplesse d'évolution. J'ai retenu MySQL qui est déjà présent sur mon ordinateur mais il conviendra plutôt d'utiliser MariaDB qui est resté open source. N'étant pas encore prêt à coder un module d'échange entre GeneWeb et MySQL, j'ai opté pour une étape intermédiaire : créer un outil de migration d'une base GeneWeb dans une base MySQL. Avec ce clone sous MySql, il sera ainsi plus facile d'étudier différentes options :
* d'amélioration des performances. L'imposante base Roglo (http://roglo.eu/roglo) n'est pas fonctionnelle avec GeneWeb v7 car GeneaNet, mainteneur officiel, n'a pas su faire évoluer le modèle gwc2 qui avait été mis au point par Daniel de Rauglaudre en v5 pour des besoins de performances.
* d'ajout de nouvelles fonctionnalités. J'ai l'espoir de me rapprocher du modèle GenTech (https://www.ngsgenealogy.org/history/) qui malgré ses 20 ans ne semble pas avoir été adopté par les principaux logiciels de généalogie alors qu'il est évident que ce modèle est pertinent.

### Création de la base MySQL
```
mysql -u root -p
```
Renseigner le mot de passe puis valider avec `Entrée`
Puis, dans la console `mysql>` :
```
create database geneweb;
create user 'gw'@'localhost' identified by 'gw_pw';
grant all privileges on geneweb.* to 'gw'@'localhost';
exit
```

### Lancer la migration

Prè-requis :
```
opam install mysql
```

Lancer le script ```run.sh``` qui va enchaîner les étapes suivantes :
* Compilation de l'extracteur Ocaml (j'utilise la version 4.07)
* Création des tables dans la base MySQL (écrase les donnnées issues des précédentes migrations)
* Exécution de l'extracteur (durée constatée de 45 minutes avec ma base de presque 250000 personnes)
* Exécution des scripts de transformations (actuellement changePlaces.sql et changeSources.sql)
