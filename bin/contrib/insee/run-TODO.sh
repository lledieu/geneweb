#!/bin/bash

EXE=A_RENSEIGNER/geneweb/_build/default/bin/contrib/insee/insee.exe
BDIR=A_RENSEIGNER
BASE=A_RENSEIGNER

MYSQL=./mysql.sh

if [ -x "${EXE}" ]
then
	echo "Get data from GeneWeb..."
	cd $BDIR
	$EXE $BASE > $OLDPWD/TODO.lst
	cd -
elif [ ! -f "TODO.lst" ]
then
	echo "ERROR GeneWeb export is not configured and TOLO.lst is missing."
	echo "N.B.: You can create TODO.lst yourself if you are not using GeneWeb."
	exit
fi

echo
echo "(Re)create table TODO..."
$MYSQL << EOF
drop table if exists TODO;

create table TODO (
	Id INTEGER UNSIGNED auto_increment primary key,
	Nom VARCHAR(80) not null,
	Prenom VARCHAR(80) not null,
	Sexe CHAR(1) not null,
	NaissanceY CHAR(4) not null,
	NaissanceM CHAR(2) not null,
	NaissanceD CHAR(2) not null,
	NaissancePlace VARCHAR(500),
	DecesY CHAR(4) not null,
	DecesM CHAR(2) not null,
	DecesD CHAR(2) not null,
	DecesPlace VARCHAR(500),
	Cle VARCHAR(100) not null,
	Etat INTEGER not null default 0,
	NbMatch INTEGER,
	Score INTEGER,
	IdInsee INTEGER UNSIGNED,
	Msg VARCHAR(1000)
);
EOF

echo
echo "Loading TODO..."
$MYSQL << EOF
load data
 local infile 'TODO.lst'
 ignore
 into table TODO
 character set utf8
 fields terminated by '|'
 (Nom, Prenom, Sexe, NaissanceD, NaissanceM, NaissanceY, NaissancePlace, DecesD, DecesM, DecesY, DecesPlace, Cle)
 set
  Id = null
;
EOF

echo
echo "Comparing TODO -> INSEE..."
echo BEGIN $(date '+%FT%T')
$MYSQL -N << EOF
call processTodo();
EOF
echo END $(date '+%FT%T')

$MYSQL -t << EOF
select
 Etat,
 case Etat
	when -5 then 'Score faible D'
	when -4 then 'Score faible NP'
	when -3 then 'Non trouvé'
	when -2 then 'Indécis'
	when -1 then 'Vivant ?'
	when 0 then 'À traiter'
	when 1 then 'Identique'
	when 2 then 'Voir RESULT.txt'
 end as "Libellé",
 score,
 count(*) as "Nbr"
 from TODO
group by 1,3;
EOF

echo
./rapportFormate.sh > RESULT.txt
echo "Bilan disponisble dans le fichier RESULT.txt"
