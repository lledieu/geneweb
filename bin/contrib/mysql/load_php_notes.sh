#!/bin/bash

DIR=/var/www/home.lledieu.org/site/src/
MYSQL=./mysql.sh

echo "Extract PHP notes..."
cd $DIR
grep '.' *ctl | sed -e "s/\.ctl:/:/" -e "s/+/_/g" -e "s/ /_/g" -e "s/:\([^:]*\):\([^:]*\):\([^:]*\)/:\L\1.\3.\L\2/" > $OLDPWD/php_notes.txt
cd -

echo "(Re)create tables an load them..."
$MYSQL << EOF
DROP TABLE IF EXISTS php_notes_ind;
DROP TABLE IF EXISTS php_notes;

CREATE TABLE php_notes (
	pn_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	nkey	VARCHAR(100) NOT NULL,
	UNIQUE(nkey)
);

CREATE TABLE php_notes_ind (
	pni_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	nkey	VARCHAR(100) NOT NULL,
	pn_id	INTEGER UNSIGNED,
	role	enum( 'Main', 'Witness' ),
	pkey	VARCHAR(100) NOT NULL,
	p_id	INTEGER UNSIGNED,
	FOREIGN KEY (pn_id) REFERENCES php_notes(pn_id),
	FOREIGN KEY (p_id) REFERENCES persons(p_id)
);

LOAD DATA
 LOCAL INFILE 'php_notes.txt'
 INTO TABLE php_notes_ind
 FIELDS TERMINATED BY ':'
 (nkey, pkey)
 set pni_id = null, role = 'Witness'
;

update php_notes_ind
set role = 'Main'
where nkey like concat('%',pkey,'%');

insert into php_notes
select distinct null, nkey from php_notes_ind;

update php_notes_ind
inner join php_notes using(nkey)
set php_notes_ind.pn_id = php_notes.pn_id;

alter table php_notes_ind drop column nkey;
EOF
