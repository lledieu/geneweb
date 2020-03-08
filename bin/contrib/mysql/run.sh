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

cd $BDDIR
echo "Migrating GeneWeb -> MySql..."
echo BEGIN $(date '+%FT%T')
$OLDPWD/../../../_build/default/bin/contrib/mysql/gw2mysql.exe -noHistory $BD
res=$?
echo END $(date '+%FT%T')
cd -

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
