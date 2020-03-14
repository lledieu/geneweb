#!/bin/bash

SRC=../../src
MY_BIN=gw2mysql
MY_OBJS="$SRC/adef.cmx $SRC/secure.cmx $SRC/progrBar.cmx $SRC/buff.cmx $SRC/name.cmx $SRC/iovalue.cmx $SRC/mutil.cmx $SRC/notesLinks.cmx $SRC/db2.cmx $SRC/db2disk.cmx $SRC/futil.cmx $SRC/dutil.cmx $SRC/btree.cmx $SRC/database.cmx $SRC/gwdb.cmx $SRC/lock.cmx $SRC/argl.cmx"

ocamlopt -I $SRC unix.cmxa $MY_OBJS $MY_BIN.ml -o $MY_BIN.exe
