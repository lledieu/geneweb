#!/bin/bash

DIR=/var/www/home.lledieu.org/site/src/
MYSQL=./mysql.sh

echo "Extract PHP notes..."
cd $DIR
grep -n '.' *ctl | sed -e "s/\.ctl:/:/" -e "s/+/_/g" -e "s/ /_/g" -e "s/:\([^:]*\):\([^:]*\):\([^:]*\)$/:\L\1.\3.\L\2/" > $OLDPWD/txt/php_notes.txt
cd -

echo "Load PHP notes..."
$MYSQL << EOF
DROP TABLE IF EXISTS tmp_php_notes;

CREATE TABLE tmp_php_notes (
	pni_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	nkey	VARCHAR(100) NOT NULL,
	role	enum( '', 'Main' ),
	pkey	VARCHAR(100) NOT NULL,
	pos	tinyint NOT NULL
);

LOAD DATA
 LOCAL INFILE 'txt/php_notes.txt'
 INTO TABLE tmp_php_notes
 FIELDS TERMINATED BY ':'
 (nkey, pos, pkey)
 set pni_id = null, role = ''
;

update tmp_php_notes
set role = 'Main'
where nkey rlike concat('.*-',pkey,'(-.*)?$');

insert into linked_notes
select distinct null, 'PgPhp', nkey, null, null
from tmp_php_notes;

insert into linked_notes_ind
select null,
 (select ln_id from linked_notes where ln_type = 'PgPhp' and nkey = tmp_php_notes.nkey),
 pkey, null, '', pos, role
from tmp_php_notes;

DROP TABLE tmp_php_notes;
EOF
