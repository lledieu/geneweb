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
$MYSQL < create_tables_history.sql

if [ $? != 0 ]
then
  exit -1
fi

mkdir -p txt
if [ -d "$BDDIR/$BD.gwb/history_d" ]
then

cd $BDDIR
echo "Migrating GeneWeb -> MariaDB..."
echo BEGIN $(date '+%FT%T')
$OLDPWD/../../../_build/default/bin/contrib/mysql/gw2mysql.exe -noCurrent $OLDPWD/txt $BD
res=$?
cd -

if [ $res != 0 ]
then
  exit -1
fi

echo "Loading data..."
$MYSQL << EOF
LOAD DATA
 LOCAL INFILE 'txt/history.txt'
 INTO TABLE history
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (h_id, h_date, wizard, pkey)
;

LOAD DATA
 LOCAL INFILE 'txt/history_details.txt'
 INTO TABLE history_details
 CHARACTER SET UTF8
 FIELDS TERMINATED BY '££' ENCLOSED BY '$'
 (h_id, data, old, new)
 set hd_id = 0
;
EOF
res=$?
echo END $(date '+%FT%T')

if [ $res != 0 ]
then
  exit -1
fi

else
	echo "Nothing to do : missing history_d !"
fi

if [ -f "$BDDIR/$BD.gwb/history" ]
then
	echo "Formating old history..."
	sed "s/\(.*\) \[\(.*\)\] \(..\) \(.*\)\$/\1||\2||\3||\4||/" $BDDIR/$BD.gwb/history > txt/old_history.txt
	$MYSQL < load_old_history.sql
else
	echo "Nothing to do : missing history !"
fi

echo "Adjusting history..."
echo BEGIN $(date '+%FT%T')
$MYSQL < changeHistory.sql
echo END $(date '+%FT%T')
