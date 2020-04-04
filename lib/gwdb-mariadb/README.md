## Backend MariaDB pour GeneWeb

Suite la PR de refactorisation GeneWeb pour mise en place de nouveaux backend, je me suis lancé dans une version MariaDB afin d'exploiter l'export que j'ai proposé dans cette [branche](https://github.com/lledieu/geneweb/tree/MySQL/bin/contrib/mysql)

Pour compiler avec ce backend :
```
ocaml configure.ml --api --gwdb-mariadb --sosa-zarith
make distrib
```

J'ai fait l'effort de mettre en place un système de traces qui permet de scruter les appels faits en base. À chaque requête, on obtient une synthèse dont voici un exemple :

```
=> open_base(23142)
= gwdb_driver ====================nbr=====time===cache===array=
                     sou->surn     339 0.000092    4970       0
         (re)load_ascend_array       1 0.059836       0       0
                     sou->givn     477 0.000078    2944       0
               persons_of_name       1 0.000188       0       0
                 get_statement      24 0.000140   19318       0
                      spi_find       1 0.001200       0       0
                    sou->quest      89                0       0
                       eq_istr     850                0       0
                 person_of_key       1 0.000143       0       0
                    get_couple       0                0    3792
       base_strings_of_surname       1 0.000229       0       0
                    sou->empty    1407                0       0
            persons_of_surname       1                0       0
        (re)load_couples_array       1 0.033879       0       0
                     get_union     846 0.000156       0       0
               is_empty_string    2818                0       0
                    get_ascend       0                0    5677
                   get_descend     506 0.000169       3       0
                 nb_of_persons       1 0.000345       1       0
                    get_person    1799 0.001151    2704       0
                    get_family     506 0.000437       3       0
=> close_base(23142) 2.715196
```

Pour l'instant seule la partie lecture a été mise en place. Les notes liées ne sont pas encore prises en compte. Il faut également que je retravaille la recherche par noms qui est encore incomplète.
