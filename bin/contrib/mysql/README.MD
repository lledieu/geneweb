## Export d'une base GeneWeb dans une base MariaDB

À force d'étudier des axes d'amélioration de GeneWeb, je me suis rendu compte qu'il est nécessaire de revoir le modèle de données. Un des axes d'amélioration est d'utiliser un système de gestion de base de données (SGBD) afin d'avoir une plus grande souplesse d'évolution. J'ai retenu MariaDB mais il doit être possible de transposer à d'autres SGBD. En parallèle de cet export, un nouveau backend a également été ébauché (au moins pour la partie lecture). Avec ce clone sous MariaDB, il est plus facile d'étudier différentes options :
* d'amélioration des performances. L'imposante base [Roglo](http://roglo.eu/roglo) n'est pas fonctionnelle avec GeneWeb v7 car GeneaNet, mainteneur officiel, n'a pas su faire évoluer le modèle gwc2 qui avait été mis au point par Daniel de Rauglaudre en v5 pour des besoins de performances.
* d'ajout de nouvelles fonctionnalités. J'ai l'espoir de me rapprocher du modèle [GenTech](https://www.ngsgenealogy.org/history/) qui malgré ses 20 ans ne semble pas avoir été adopté par les principaux logiciels de généalogie alors qu'il est évident que ce modèle est pertinent.

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

### Lancer la migration

Pré-requis :

> opam install batteries

Lancer le script `./run.sh` qui va enchaîner les étapes suivantes :
* Compilation de l'extracteur Ocaml (j'utilise la version 4.07)
* Création des tables dans la base MariaDB (écrase les donnnées issues des précédentes migrations)
* Exécution de l'extracteur
* Exécution des scripts de transformations (actuellement occupationsChange.sql)

Lancer le script `./run2.sh` qui va enchaîner les étapes suivantes :
* Compilation de l'extracteur Ocaml (j'utilise la version 4.07)
* Création des tables dans la base MariaDB pour la partie historique (écrase les donnnées issues des précédentes migrations)
* Exécution de l'extracteur
* Exécution des scripts de transformations (actuellement changeHistory.sql)
* Chargement de l'ancien système d'histique (via load_old_history.sql)

J'ai même un script `./all.sh` mais ce script intègre beaucoup de traitements spécifiques à ma base de données.

Pour info, j'ai préparé quelques scripts SQL que vous pouvez lancer en remplaçant l'identifiant de l'objet (e.g.: <p_id> pour identifiant d'une personne) :

```
./mysql.sh -t
MariaDB> set @id = <p_id> ; \. p_id2person.sql
MariaDB> set @id = <p_id> ; \. p_id2asc.sql
MariaDB> set @id = <s_id> ; \. s_id2pkey2.sql
MariaDB> set @id = <n_id> ; \. n_id2pkey2.sql
MariaDB> set @id = <e_id> ; \. e_id2pkey2.sql
```

### État du modèle
![Modèle](modele.png)

Les principales tables sont :
* persons : une ligne pour chaque personne
  * person_event : permet de lier les événements aux personnes avec un attribut permettant de connaître le rôle de la personne. Il faudrait également ajouter un lien vers names pour mémoriser le nom utilisé lors de l'événement.
  * person_group : permet de lier les groupes (familles) aux personnes avec un attribut rôle qui distingue parent et enfant. Le champ seq pour un parent permet de trier les unions dans l'ordre. Le champ seq pour un enfant permet de trier les enfants dans la famille.
  * person_media : permet de lier les medias
  * person_name : permet de lister les variantes de noms
* groups : une ligne pour chaque famille
  * group_event : permet de lier les événements aux familles. 
* events : une ligne pour chaque événement qu'ils soient de niveau personne ou de niveau famille. Certains événements nécessitent un complément :
  * occupation_details : permet de préciser la profession
  * title_details : la gestion des titres est anormalement complexe dans GeneWeb ! Il conviendrait de remonter l'information place au niveau events pour la standardiser.
  * death_details : pour gérer la raison du décès qui est spécifique à ce type d'événement (pas de ligne pour une raison inconnue pour diminuer le volume de la table). Douple emploi avec event_values ?
  * events_dmy2 : lorsque la précision requière une deuxième date, elle est stockée ici (cette découpe devrait permettre de réduire le volume de données)
  * events_values : certains événements sont porteurs de données variées. On stocke ça au format JSON
* names : toutes les variantes de noms
  * names_givn : les prénoms (contient également la forme minuscule sans accent pour la recherche approchée)
  * names_nick : les surnoms
  * names_surn : les patronymes (contient également la forme minuscule sans accent pour le recherche approchée, ainsi 
  * names_crush : noms érodés pour la recherche approchée
* occupations : une ligne pour chaque profession
* places : une ligne pour chaque lieu. Il faut prévoir de hiérarchiser les lieux, d'intégrer des données de géolocalisation, de traduire certains lieux, etc.
* sources : une ligne par source. Il faut prévoir de hiérarchiser les sources afin de pouvoir les regrouper par registre, série d'archive, livre et autres.
* notes : une ligne par note. Il conviendrait que je découpe certaines notes et que je transforme certaines en événements. Voir par exemple cette [fiche](https://lledieu.org/?p=robert&n=vainet&oc=1) pour comprendre l'intérêt.
* medias : une ligne par medias. Actuellement, il s'agit des portraits. Il faut que je complète avec les liens m=IM qui peuvent traîner dans les notes pour des signatures comme sur cette [fiche](https://lledieu.org/?p=stanislas&n=gamot&oc=5).
* linked_notes : une ligne pour chaque note liée. Le lien de cette table avec persons et group n'est pas satisfaisant. En réalité, la liaison devrait passer via notes ou sources...
  * linked_notes_nt : lien vers une autre note liée (via la clé nkey)
  * linked_notes_ind : liens vers une personne (via la clé pkey). Note de role ajouté pour PgPhp avec lequel j'arrive à distinguer les rôles principal et témoin.
  * N.B.: j'ai ajouté le type PgPhp pour intégrer l'ancêtre des notes liées que je gère toujours sous PHP. Je n'ai toujours pas migré dans GeneWeb (car je ne suis pas satisfait du stockage et de l'obligation de référencement)
* users : une ligne pour chaque magicien
* transactions : une ligne par transaction d'historique. La notion de transaction permet de recouper l'ancien historique avec le nouveau en prenant comme clé h_date et u_id que je suppose unique.
  * old_history : il s'agit des données issues du fichier history de la base dans lequel j'ai des données depuis 2011
  * history : il s'agit des données issues du répertoire history_d de la base dans lerquel j'ai des données depuis que j'utilise la version 7 de GeneWeb (vers mai 2019) /!\ La stabilité des données n'est pas garantie dans GeneWeb : il faudrait remplacer iper par fn.oc.sn pour améliorer ça !
  * history_details : détails extraits du répertoire history_d dans lequel je n'ai conservé que les différences
* caches : pour des raisons de performances, il est nécessaire de pré-construire des tableaux (actuellement : ascends et couples). Il faudrait y intégrer tstab, tstab_visitor et restrict.
* representations : notion GenTech associée aux sources. L'usage intensif de macro dans GeneWeb me permet de retrouver cette notion dans ma base GeneWeb.
