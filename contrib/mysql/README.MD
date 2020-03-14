## Export d'une base GeneWeb v6 dans une base MariaDB

### Compilation

> make

> cd contrib/mysql

> ./compile.sh

### GeneWeb vers MariaDB

> ./run.sh

> ./run2.sh

### Tests

> ./mysql -t < test.sql

> ./mysql -t < test-asc.sql
