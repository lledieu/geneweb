#!/bin/bash

./run.sh
./run2.sh
# Following is specific
./load_php_notes.sh
./MediaSignature.sh
./mysql.sh -N < changeNotes.sql
#./mysql.sh -N < changeRepresentations.sql
