#!/bin/bash

CFG=.run.cfg
MYSQL=./mysql.sh

if [ ! -f $CFG ]
then
	echo -n "Path to GeneWeb databases : "
	read BDDIR
	if [ ! -d "$BDDIR" ]
	then
		echo "ERROR $BDDIR not found."
		exit
	fi
	echo -n "GeneWeb database : "
	read BD
	echo "BDDIR=$BDDIR" > $CFG
	echo "BD=$BD" >> $CFG
else
	. $CFG
fi

echo "Compiling..."
dune build gw2mysql.exe

if [ $? != 0 ]
then
  exit -1
fi

echo "(Re)creating tables..."
$MYSQL < create_tables.sql

if [ $? != 0 ]
then
  exit -1
fi

mkdir -p txt
cd $BDDIR
echo "Migrating GeneWeb -> MariaDB..."
echo BEGIN $(date '+%FT%T')
$OLDPWD/../../../_build/default/bin/contrib/mysql/gw2mysql.exe -noHistory $OLDPWD/txt $BD
res=$?
cd -

if [ $res != 0 ]
then
  exit -1
fi

echo "Loading data..."
$MYSQL << EOF
LOAD DATA
 LOCAL INFILE 'txt/notes.txt'
 INTO TABLE notes
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (n_id, note)
;

LOAD DATA
 LOCAL INFILE 'txt/sources.txt'
 INTO TABLE sources
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (s_id, source)
;

LOAD DATA
 LOCAL INFILE 'txt/persons.txt'
 INTO TABLE persons
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (p_id, pkey, pkey2, occ, death, @n_id, @s_id, consang, sex, access)
 set n_id = nullif(@n_id, '__NULL__'),
     s_id = nullif(@s_id, '__NULL__')
;

LOAD DATA
 LOCAL INFILE 'txt/events.txt'
 INTO TABLE events
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (e_id, e_type, t_name, d_prec, d_cal1, dmy1_d, dmy1_m, dmy1_y, d_text, place, @n_id, @s_id)
 set n_id = nullif(@n_id, '__NULL__'),
     s_id = nullif(@s_id, '__NULL__')
;

LOAD DATA
 LOCAL INFILE 'txt/death_details.txt'
 INTO TABLE death_details
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (e_id, reason)
;

LOAD DATA
 LOCAL INFILE 'txt/event_dmy2.txt'
 INTO TABLE event_dmy2
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (e_id, d_cal2, dmy2_d, dmy2_m, dmy2_y)
;

LOAD DATA
 LOCAL INFILE 'txt/event_values.txt'
 INTO TABLE event_values
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (e_id, attr)
;

LOAD DATA
 LOCAL INFILE 'txt/occupation_details.txt'
 INTO TABLE occupation_details
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (e_id, name)
;

LOAD DATA
 LOCAL INFILE 'txt/title_details.txt'
 INTO TABLE title_details
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (e_id, ident, place, nth, main, name, d1_prec, d1_cal, d1_dmy1_d, d1_dmy1_m, d1_dmy1_y, d1_dmy2_d, d1_dmy2_m, d1_dmy2_y, d1_text, d2_prec, d2_cal, d2_dmy1_d, d2_dmy1_m, d2_dmy1_y, d2_dmy2_d, d2_dmy2_m, d2_dmy2_y, d2_text)
;

LOAD DATA
 LOCAL INFILE 'txt/person_event.txt'
 INTO TABLE person_event
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (e_id, p_id, role)
 set pe_id = 0
;

LOAD DATA
 LOCAL INFILE 'txt/medias.txt'
 INTO TABLE medias
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (m_id, fname)
;

LOAD DATA
 LOCAL INFILE 'txt/person_media.txt'
 INTO TABLE person_media
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (p_id, m_id)
 set pm_id = 0
;

LOAD DATA
 LOCAL INFILE 'txt/person_name.txt'
 INTO TABLE person_name
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (p_id, givn, nick, surn, n_type)
 set pn_id = 0, npfx = '', spfx = '', nsfx = ''
;

LOAD DATA
 LOCAL INFILE 'txt/groups.txt'
 INTO TABLE groups
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (g_id, @n_id, @s_id, origin)
 set n_id = nullif(@n_id, '__NULL__'),
     s_id = nullif(@s_id, '__NULL__')
;

LOAD DATA
 LOCAL INFILE 'txt/person_group.txt'
 INTO TABLE person_group
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (g_id, p_id, role, seq)
 set pg_id = 0
;

LOAD DATA
 LOCAL INFILE 'txt/linked_notes.txt'
 INTO TABLE linked_notes
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (ln_id, ln_type, nkey, @p_id, @g_id)
 set p_id = nullif(@p_id, '__NULL__'),
     g_id = nullif(@g_id, '__NULL__')
;

LOAD DATA
 LOCAL INFILE 'txt/linked_notes_nt.txt'
 INTO TABLE linked_notes_nt
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (ln_id, nkey)
 set lnn_id = 0
;

LOAD DATA
 LOCAL INFILE 'txt/linked_notes_ind.txt'
 INTO TABLE linked_notes_ind
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (ln_id, pkey, text, pos)
 set lni_id = 0
;

EOF
res=$?
echo END $(date '+%FT%T')

if [ $res != 0 ]
then
  exit -1
fi

echo "Adjusting places..."
echo BEGIN $(date '+%FT%T')
$MYSQL < changePlaces.sql
echo END $(date '+%FT%T')

if [ $res != 0 ]
then
  exit -1
fi

echo "Adjusting sources..."
echo BEGIN $(date '+%FT%T')
$MYSQL < changeSources.sql
echo END $(date '+%FT%T')

if [ $res != 0 ]
then
  exit -1
fi

echo "Adjusting occupations..."
echo BEGIN $(date '+%FT%T')
$MYSQL < changeOccupations.sql
echo END $(date '+%FT%T')

if [ $res != 0 ]
then
  exit -1
fi

echo "Adjusting names..."
echo BEGIN $(date '+%FT%T')
$MYSQL < changeNames.sql
echo END $(date '+%FT%T')
