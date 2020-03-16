DROP TABLE IF EXISTS representations;

CREATE TABLE representations (
	r_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	s_id	INTEGER UNSIGNED NOT NULL,
	r_type	enum (
		'', -- n_id
		'Links', -- 
		'Files', -- m_id
		'Notes', -- ln_id
		'Paper' -- 
        ) NOT NULL DEFAULT '',
	attr	VARCHAR(2000),
	attr_macro VARCHAR(10) AS (JSON_VALUE(attr, '$.macro')),
	n_id	INTEGER UNSIGNED,
	FOREIGN KEY (s_id) REFERENCES sources(s_id),
	FOREIGN KEY (n_id) REFERENCES notes(n_id),
	CHECK( attr IS NULL OR JSON_VALID(attr) )
);

DROP PROCEDURE IF EXISTS geneweb.test;

delimiter $$
CREATE PROCEDURE geneweb.test()
BEGIN
	DECLARE done INT DEFAULT FALSE;
	DECLARE cnt INTEGER UNSIGNED DEFAULT 1;
	DECLARE id INTEGER UNSIGNED;
	DECLARE s, new, r TEXT;
	DECLARE cur CURSOR FOR
		select s_id, source
		from sources
		where source like '%\%%'
		   or source like '%[[[%'
		   or source like '%<a%'
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

	SELECT 'Parsing sources (-> representations)...';

	WHILE cnt > 0 DO
		SET cnt = 0;
		SET done = FALSE;

		OPEN cur;
b:		LOOP
			FETCH cur INTO id, s;

			IF done THEN
				LEAVE b;
			END IF;

			IF s RLIKE '^[^%]*%vAD59_.:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD59_.:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD59_(.):([^:;]*):([^:;]*):[^:;]*:[^:;]*:([^:;]*);.*', '{"macro":"AD59_\\1","cote":"\\2","max":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD59:[^;:]*:[^;:]*:[^;:]*:[^;:]*:[^;:]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD59:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD59:([^:;]*):([^:;]*):[^:;]*:[^:;]*:([^:;]*):([^:;]*);.*', '{"macro":"AD59","cote":"\\1","max":"\\2","vue":"\\3","text":"\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD62_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD62_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD62_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD62_\\1","ref":"\\2","cote":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD62:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD62:[^:;]*:([^:;]*):[^:;]*:[^:;]*;', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD62:([^:;]*):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD62","ref":"\\1","text":"\\2","cote":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD80_.:[^:;]*:1:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD80_.:[^:;]*:1:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD80_(.):([^:;]*):1:([^:;]*);.*', '{"macro":"AD80_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD80:[^:;]*:1:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD80:[^:;]*:1:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD80:([^:;]*):1:([^:;]*):([^:;]*)*;.*', '{"macro":"AD80","ref":"\\2","vue":"\\3","text":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD44_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD44_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD44_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD44_\\1","c":"\\2","r":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD44:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD44:[^:;]*:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD44:([^:;]*):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD44","c":"\\1","r":"\\2","vue":"\\3","text":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAEB_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAEB_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAEB_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AEB_\\1","p1":"\\2","p2":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAEB:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAEB:[^:;]*:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAEB:([^:;]*):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AEB","p1":"\\1","p2":"\\2","vue":"\\3","text":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD34_.:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD34_.:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD34_(.):([^:;]*):([^:;]*);.*', '{"macro":"AD34_\\1","vta":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD34_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD34_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD34_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD34_\\1","vta":"\\2","vue":"\\3","p":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vBnF:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vBnF:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vBnF:([^:;]*):([^:;]*);.*', '{"macro":"BnF","ref":"\\1","f":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vSGA:[^/;]*/[^/;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vSGA:[^/;]*/[^/;]*;[ ,]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vSGA:([^/;]*)/([^/;]*);.*', '{"macro":"SGA","id_fiche":"\\1","id_scan":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vSGA:[^/;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vSGA:[^/;]*;[ ,]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vSGA:([^/;]*);.*', '{"macro":"SGA","id_fiche":"\\1"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*<a href="%vsrc;%sm=SRC;v=[^"]*">[^<]*</a>' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)<a href="%vsrc;%sm=SRC;v=[^"]*">([^<]*)</a>', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*<a href="%vsrc;%sm=SRC;v=([^"]*)">([^<]*)</a>.*', '{"ln_typ":"PgPhp","nkey":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Notes', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vLVN:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vLVN:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vLVN:([^:;]*);.*', '{"macro":"LVN","ref":"\\1"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vLVN2:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vLVN2:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vLVN2:([^:;]*);.*', '{"macro":"LVN2","ref":"\\1"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD62_RP:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD62_RP:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD62_RP:([^:;]*):([^:;]*);.*', '{"macro":"AD62_RP","ref":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD75_.:.:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD75_.:.:[^:;]*:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD75_(.):(.):([^:;]*):([^:;]*):([^:;]*):[^:;]*;.*', '{"macro":"AD75_\\1","type":"\\2","arr":"\\3","date":"\\4","n":"\\5"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD75:.:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD75:.:[^:;]*:[^:;]*:[^:;]*:([^:;]*):[^:;]*;', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD75:(.):([^:;]*):([^:;]*):([^:;]*):([^:;]*):[^:;]*;.*', '{"macro":"AD75","type":"\\1","arr":"\\2","date":"\\3","n":"\\4","text":"\\5"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD03_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD03_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD03_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD03_\\1","ref":"\\2","v":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAM44_.:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAM44_.:[^:;]*:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAM44_(.):([^:;]*):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AM44_\\1","p1":"\\2","p2":"\\3","p3":"\\4","vue":"\\5"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vgB:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vgB:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vgB:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"gB","ref":"\\1","pg":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD02_.:[^:;]*/daogrp/0/[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD02_.:[^:;]*/daogrp/0/[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD02_(.):([^:;]*)/daogrp/0/([^:;]*);.*', '{"macro":"AD02_\\1","vta":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD02_.:[^:;]*/daogrp/0/[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD02_.:[^:;]*/daogrp/0/[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD02_(.):([^:;]*)/daogrp/0/([^:;]*):[^:;]*;.*', '{"macro":"AD02_\\1","vta":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD02:[^:;]*/daogrp/0/[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD02:[^:;]*/daogrp/0/[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD02:([^:;]*)/daogrp/0/([^:;]*):[^:;]*:([^:;]*);.*', '{"macro":"AD02","vta":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD51_.:[^:;]*:1:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD51_.:[^:;]*:1:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD51_(.):([^:;]*):1:([^:;]*);.*', '{"macro":"AD51_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD78_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD78_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD78_(.):([^:;]*);.*', '{"macro":"AD78_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD78:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD78:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD78:([^:;]*):([^:;]*);.*', '{"macro":"AD78","ref":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAM62119_.:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAM62119_.:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAM62119_(.):([^:;]*):([^:;]*);.*', '{"macro":"AM62119_\\1","cote":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAM62119:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAM62119:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAM62119:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AM62119","cote":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAC59465:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAC59465:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAC59465:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AC59465","a":"\\1","n1":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAC59465bis:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAC59465bis:[^:;]*:[^:;]*:([^:;]*):[^:;]*:[^:;]*;', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAC59465bis:([^:;]*):([^:;]*):([^:;]*):[^:;]*:([^:;]*);.*', '{"macro":"AC59465","a":"\\1","n1":"\\2","n2":"\\4","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAC59465ter:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAC59465ter:[^:;]*:[^:;]*:([^:;]*):[^:;]*:[^:;]*:[^:;]*:[^:;]*;', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAC59465ter:([^:;]*):([^:;]*):([^:;]*):[^:;]*:([^:;]*):[^:;]*:([^:;]*);.*', '{"macro":"AC59465","a":"\\1","n1":"\\2","n2":"\\4","n3":"\\5","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
			ELSEIF s RLIKE '^[^%]*%vAD34:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD34:[^:;]*:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD34:([^:;]*):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD34","vta":"\\1","vue":"\\2","p":"\\3","text":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAM59350_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAM59350_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAM59350_(.):([^:;]*);.*', '{"macro":"AM59350_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAM59178_.:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAM59178_.:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAM59178_(.):([^:;]*):([^:;]*);.*', '{"macro":"AM59178_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAM59178:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAM59178:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAM59178:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AM59178","ref":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vael_[A-Z]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vael_[A-Z]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vael_([A-Z]*):([^:;]*);.*', '{"macro":"ael_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vael:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vael:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vael:([^:;]*):([^:;]*);.*', '{"macro":"ael","ref":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD77:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD77:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD77:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD77","ref":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vacr:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vacr:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vacr:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"acr","cote":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD60_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD60_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD60_(.):([^:;]*);.*', '{"macro":"AD60_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD60:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD60:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD60:([^:;]*):([^:;]*);.*', '{"macro":"AD60","ref":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD76_.:[^:;]*:1:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD76_.:[^:;]*:1:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD76_(.):([^:;]*):1:([^:;]*);.*', '{"macro":"AD76_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vBSD:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vBSD:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vBSD:([^:;]*):([^:;]*);.*', '{"macro":"BSD","ref":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vFS:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vFS:[^:;]*:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vFS:([^:;]*):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"FS","p1":"\\1","p2":"\\2","p3":"\\3","text":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vFS_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vFS_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vFS_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"FS_\\1","p1":"\\2","p2":"\\3","p3":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD49_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD49_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD49_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD49_\\1","p1":"\\2","p2":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD54_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD54_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD54_(.):([^:;]*);.*', '{"macro":"AD54_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD54:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD54:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD54:([^:;]*):([^:;]*);.*', '{"macro":"AD54","ref":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD58_.:[^:;]*:1:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD58_.:[^:;]*:1:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD58_(.):([^:;]*):1:([^:;]*);.*', '{"macro":"AD58_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD48_.:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD48_.:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD48_(.):([^:;]*):([^:;]*);.*', '{"macro":"AD48_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD48:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD48:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD48:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD48","ref":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD21_.:[^:;]*/[^/:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD21_.:[^:;]*/[^/:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD21_(.):([^:;]*)/([^/:;]*);.*', '{"macro":"AD21_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vTdF:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vTdF:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vTdF:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"TdF","p1":"\\1","p2":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vNGT:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vNGT:[^:;]*:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vNGT:[^:;]*:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"NGT","tome":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vol:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vol:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vol:([^:;]*):([^:;]*);.*', '{"macro":"ol","ref":"\\1","page":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vNN:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vNN:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vNN:([^:;]*);.*', '{"macro":"NN","ref":"\\1"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD45_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD45_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD45_(.):([^:;]*);.*', '{"macro":"AD45_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD94_.:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD94_.:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD94_(.):([^:;]*):([^:;]*);.*', '{"macro":"AD94_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD50_.:[^:;]*:1:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD50_.:[^:;]*:1:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD50_(.):([^:;]*):1:([^:;]*);.*', '{"macro":"AD50_\\1","ref":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD72_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD72_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD72_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD72_\\1","p1":"\\2","p2":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD95_.:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD95_.:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD95_(.):([^:;]*):([^:;]*);.*', '{"macro":"AD95_\\1","p1":"\\2","p2":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD95_.:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD95_.:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD95_(.):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD95_\\1","p1":"\\2","p2":"\\3","vue":"\\4"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vHT:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vHT:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vHT:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"HT","ref":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD18_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD18_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD18_(.):([^:;]*);.*', '{"macro":"AD18_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD12_v2_.:[^:;]*:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD12_v2_.:[^:;]*:[^:;]*:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD12_v2_(.):([^:;]*):([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD12_\\1","vta":"\\2","id":"\\3","cote":"\\4","vue":"\\5"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vPersee:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vPersee:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vPersee:([^:;]*);.*', '{"macro":"Persee","ref":"\\1"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vGNr:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vGNr:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vGNr:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"GNr","ref":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD27_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD27_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD27_(.):([^:;]*);.*', '{"macro":"AD27_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD08_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD08_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD08_(.):([^:;]*);.*', '{"macro":"AD08_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD82_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD82_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD82_(.):([^:;]*);.*', '{"macro":"AD82_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD55_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD55_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD55_(.):([^:;]*);.*', '{"macro":"AD55_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD52:[^:;]*:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD52:[^:;]*:[^:;]*:([^:;]*);', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD52:([^:;]*):([^:;]*):([^:;]*);.*', '{"macro":"AD52","ref":"\\1","vue":"\\2","text":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAM80021_.:[^:;]*:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAM80021_.:[^:;]*:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAM80021_(.):([^:;]*):([^:;]*);.*', '{"macro":"AD80021_\\1","cote":"\\2","vue":"\\3"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*%vAD86_.:[^:;]*;' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)%vAD86_.:[^:;]*;[, ]*', '\\1');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*%vAD86_(.):([^:;]*);.*', '{"macro":"AD86_\\1","ref":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*\\[\\[\\[[^/\\]]*/[^/\\]]*\\]\\]\\]' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)\\[\\[\\[[^/\\]]*/([^/\\]]*)\\]\\]\\]', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*\\[\\[\\[([^/\\]]*)/[^/\\]]*\\]\\]\\].*', '{"ln_typ":"PgMisc","nkey":"\\1"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Notes', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			ELSEIF s RLIKE '^[^%]*<a href="[^"]*"[^>]*>[^<]*</a>' THEN
				set new = regexp_replace(s collate utf8_bin, '^([^%]*)<a href="[^"]*"[^>]*>([^<]*)</a>', '\\1\\2');
				set r = regexp_replace(s collate utf8_bin, '^[^%]*<a href="([^"]*)"[^>]*>([^<]*)</a>.*', '{"url":"\\1","text":"\\2"}');
				INSERT INTO representations (s_id, r_type, attr) values (id, 'Links', r);
				UPDATE sources set source = new where s_id = id;
				set cnt = cnt + 1;
			END IF;

		END LOOP;

		CLOSE cur;

		SELECT 'Updated', cnt;

	END WHILE;

END$$

delimiter ;

call test();

DROP PROCEDURE geneweb.test;

-- Ajustement de données
update representations
set attr = json_replace(attr, '$.cote', regexp_replace( json_value(attr, '$.cote'), '^ ', ''))
where attr_macro like 'AD59%'
  and json_value(attr, '$.cote') rlike '^ 5 Mi';

update representations
set attr = json_replace(attr, '$.cote', regexp_replace( json_value(attr, '$.cote'), '^(5 Mi )(.. R ...)$', '\\10\\2'))
where attr_macro like 'AD59%'
  and json_value(attr, '$.cote') rlike '^5 Mi .. R ...$';

update representations
set attr = json_replace(attr, '$.cote', '5 Mi 047 R 001')
where attr_macro like 'AD59%'
  and json_value(attr, '$.cote') = 'Mi 047 R 001';

update representations
set attr = json_replace(attr, '$.cote', 'AC 017 GG01')
where attr_macro like 'AD59%'
  and json_value(attr, '$.cote') = 'AC 017 GG1';

update representations
set attr = json_replace(attr, '$.cote', regexp_replace( json_value(attr, '$.cote'), '^(AC 586 E)(.)$', '\\10\\2'))
where attr_macro like 'AD59%'
  and json_value(attr, '$.cote') rlike '^AC 586 E.$';

update representations
set attr = json_replace(attr, '$.cote', 'AC 599 1GG091')
where attr_macro like 'AD59%'
  and json_value(attr, '$.cote') = 'AC 599 1GG91';

update representations
set attr = json_replace(attr, '$.cote', regexp_replace( json_value(attr, '$.cote'), '^(AC 599 1GG)(.)$', '\\100\\2'))
where attr_macro like 'AD59%'
  and json_value(attr, '$.cote') rlike '^AC 599 1GG.$';
