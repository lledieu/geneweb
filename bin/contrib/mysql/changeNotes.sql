DROP PROCEDURE IF EXISTS geneweb.test;
DROP PROCEDURE IF EXISTS geneweb.clean;

delimiter $$
CREATE PROCEDURE geneweb.clean()
BEGIN
	DECLARE done INT DEFAULT FALSE;
	DECLARE id1, id2 INTEGER UNSIGNED;
	DECLARE n, new TEXT;
	DECLARE cur CURSOR FOR
		select n_id, note, p_id
		from notes
		inner join persons using(n_id)
		where note rlike '(?s).*\\n$' or note rlike '(?s)^\\n.*'
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

	SELECT 'Cleaning...';

	OPEN cur;
b:	LOOP
		FETCH cur INTO id1, n, id2;

		IF done THEN
			LEAVE b;
		END IF;

		IF n RLIKE '(?s)^.*\\n$' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)^(.*)\\n$', '\\1');
			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s)^\\n.*$' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)^\\n(.*)$', '\\1');
			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSE
			select 'WARN', id1, n;
		END IF;

	END LOOP;

	CLOSE cur;

END$$

CREATE PROCEDURE geneweb.test()
BEGIN
	DECLARE done INT DEFAULT FALSE;
	DECLARE id1, id2, id3 INTEGER UNSIGNED;
	DECLARE n, new, v TEXT;
	DECLARE cur CURSOR FOR
		select n_id, note, p_id
		from notes
		inner join persons using(n_id)
		where note rlike '(?s).*%vFB2?:.*'
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
	DECLARE EXIT HANDLER FOR SQLSTATE '23000' BEGIN
		select 'ERROR', id1, v;
        END;

	SELECT 'Parsing notes (vFB -> events)...';

	OPEN cur;
b:	LOOP
		FETCH cur INTO id1, n, id2;

		IF done THEN
			LEAVE b;
		END IF;

		IF n RLIKE '(?s).*\\*%vFB:.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)\\*%vFB:[^:;]*;[^\\n]*\n?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s)^.*\\*%vFB:([^:;]*);([^\\n]*).*$', '{"ref_v1":"\\1","text":"\\2"}');
			set v= regexp_replace(v collate utf8_bin, ',"text":""', '');
			INSERT INTO events (e_type,t_name) values ('FACT','FB');
			set id3 = last_insert_id();
			INSERT INTO person_event (e_id, p_id, role) values (id3, id2, 'Main');
			INSERT INTO event_values (e_id, attr) values (id3, v);
			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*\\*%vFB2:.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)\\*%vFB2:[^:;]*;[^\\n]*\n?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s)^.*\\*%vFB2:([^:;]*);([^\\n]*).*$', '{"ref_v2":"\\1","text":"\\2"}');
			set v= regexp_replace(v collate utf8_bin, ',"text":""', '');
			INSERT INTO events (e_type,t_name) values ('FACT','FB');
			set id3 = last_insert_id();
			INSERT INTO person_event (e_id, p_id, role) values (id3, id2, 'Main');
			INSERT INTO event_values (e_id, attr) values (id3, v);
			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSE
			select 'WARN', id1, n;
		END IF;

	END LOOP;

	CLOSE cur;

END$$

delimiter ;

call test();
call clean();

DROP PROCEDURE geneweb.test;

delimiter $$
CREATE PROCEDURE geneweb.test()
BEGIN
	DECLARE done INT DEFAULT FALSE;
	DECLARE id1, id2, id3 INTEGER UNSIGNED;
	DECLARE nbr INTEGER;
	DECLARE n, new, v TEXT;
	DECLARE cur CURSOR FOR
		select n_id, note, p_id
		from notes
		inner join persons using(n_id)
		where note rlike '(?s).*\\*Témoins? \\+:.*'
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

	SELECT 'Parsing notes (Témoin + -> events)...';

	OPEN cur;
b:	LOOP
		FETCH cur INTO id1, n, id2;

		IF done THEN
			LEAVE b;
		END IF;

		IF n RLIKE '(?s).*\\*Témoins? \\+:.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)\\*Témoins? \\+:[^\\n]*\n?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s)^.*(\\*Témoins? \\+:[^\\n]*).*$', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			UPDATE events
			INNER JOIN person_event using(e_id)
			SET n_id = id3
			where p_id = id2 and e_type = 'DEAT' and n_id is null;
			set nbr = ROW_COUNT();
			IF nbr <> 1 THEN
				select 'WARN', id1, 'affected', nbr;
			END IF;
			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSE
			select 'WARN', id1, n;
		END IF;

	END LOOP;

	CLOSE cur;

END$$

delimiter ;

call test();
call clean();

DROP PROCEDURE geneweb.test;

delimiter $$
CREATE PROCEDURE geneweb.test()
BEGIN
	DECLARE done INT DEFAULT FALSE;
	DECLARE id1, id2, id3 INTEGER UNSIGNED;
	DECLARE nbr INTEGER;
	DECLARE n, new, v TEXT;
	DECLARE cur CURSOR FOR
		select n_id, note, p_id
		from notes
		inner join persons using(n_id)
		where note rlike '(?s).*\\*Témoins? °:.*'
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

	SELECT 'Parsing notes (Témoin ° -> events)...';

	OPEN cur;
b:	LOOP
		FETCH cur INTO id1, n, id2;

		IF done THEN
			LEAVE b;
		END IF;

		IF n RLIKE '(?s).*\\*Témoins? °:.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)\\*Témoins? °:[^\\n]*\n?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s)^.*(\\*Témoins? °:[^\\n]*).*$', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			UPDATE events
			INNER JOIN person_event using(e_id)
			SET n_id = id3
			where p_id = id2 and e_type = 'BIRT' and n_id is null;
			set nbr = ROW_COUNT();
			IF nbr <> 1 THEN
				select 'WARN', id1, 'affected', nbr;
			END IF;
			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSE
			select 'WARN', id1, n;
		END IF;

	END LOOP;

	CLOSE cur;

END$$

delimiter ;

call test();
call clean();

DROP PROCEDURE geneweb.test;

delimiter $$
CREATE PROCEDURE geneweb.test()
BEGIN
	DECLARE done INT DEFAULT FALSE;
	DECLARE id1, id2, id3, id4 INTEGER UNSIGNED;
	DECLARE nbr INTEGER;
	DECLARE n, new, v TEXT;
	DECLARE cur CURSOR FOR
		select n_id, note, p_id
		from notes
		inner join persons using(n_id)
		where note rlike '(?s).*\\*[PM]arrain.*'
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

	SELECT 'Parsing notes (Parrain/Marraine -> events)...';

	OPEN cur;
b:	LOOP
		FETCH cur INTO id1, n, id2;

		IF done THEN
			LEAVE b;
		END IF;

		IF n RLIKE '(?s).*\\*Parrains?:[^\\n]*\nMarraines?:[^\\n]*(\\n\\*.*)?$' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)(.*)\\*Parrain[^\\n]*\nMarraine[^\\n]*((\\n\\*.*)?)$', '\\1\\2');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(\\*Parrain[^\\n]*\nMarraine[^\\n]*)(\\n\\*.*)?$', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();

			SELECT count(*) into @nbr
			FROM events
			INNER JOIN person_event USING(e_id)
			WHERE p_id = id2 and e_type = 'BAPT'
			;

			UPDATE events
			INNER JOIN person_event using(e_id)
			SET n_id = id3
			where p_id = id2 and e_type = 'BAPM' and n_id is null;
			set nbr = ROW_COUNT();
			IF nbr = 0 THEN
				INSERT events (e_type, n_id) values ('BAPM', id3);
				set id4 = last_insert_id();
				INSERT person_event (p_id, e_id, role) values (id2, id4, 'Main');
			ELSEIF nbr > 1 THEN
				select 'WARN', id1, 'affected', nbr;
			END IF;

			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*\\*[PM]arraine?s?:[^\\n]*(\\n\\*.*)?$' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)(.*)\\*[PM]arrain[^\\n]*((\\n\\*.*)?)$', '\\1\\2');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(\\*[PM]arrain[^\\n]*)(\\n\\*.*)?$', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();

			SELECT count(*) into @nbr
			FROM events
			INNER JOIN person_event USING(e_id)
			WHERE p_id = id2 and e_type = 'BAPT'
			;

			UPDATE events
			INNER JOIN person_event using(e_id)
			SET n_id = id3
			where p_id = id2 and e_type = 'BAPM' and n_id is null;
			set nbr = ROW_COUNT();
			IF nbr = 0 THEN
				INSERT events (e_type, n_id) values ('BAPM', id3);
				set id4 = last_insert_id();
				INSERT person_event (p_id, e_id, role) values (id2, id4, 'Main');
			ELSEIF nbr > 1 THEN
				select 'WARN', id1, 'affected', nbr;
			END IF;

			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*\\*Parrains?:[^\\n]*\nMarraines?:[^\\n]*\nTémoins? °:[^\\n]*(\\n\\*.*)?$' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)(.*)\\*Parrain[^\\n]*\nMarraine[^\\n]*\nTémoins? °:[^\\n]*((\\n\\*.*)?)$', '\\1\\2');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(\\*Parrain[^\\n]*\nMarraine[^\\n]*\nTémoins? °:[^\\n]*)(\\n\\*.*)?$', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();

			SELECT count(*) into @nbr
			FROM events
			INNER JOIN person_event USING(e_id)
			WHERE p_id = id2 and e_type = 'BAPT'
			;

			UPDATE events
			INNER JOIN person_event using(e_id)
			SET n_id = id3
			where p_id = id2 and e_type = 'BAPM' and n_id is null;
			set nbr = ROW_COUNT();
			IF nbr = 0 THEN
				INSERT events (e_type, n_id) values ('BAPM', id3);
				set id4 = last_insert_id();
				INSERT person_event (p_id, e_id, role) values (id2, id4, 'Main');
			ELSEIF nbr > 1 THEN
				select 'WARN', id1, 'affected', nbr;
			END IF;

			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*\\*[PM]arraine?s?:[^\\n]*\nTémoins? °:[^\\n]*(\\n\\*.*)?$' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)(.*)\\*[PM]arrain[^\\n]*\nTémoins? °:[^\\n]*((\\n\\*.*)?)$', '\\1\\2');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(\\*[PM]arrain[^\\n]*\nTémoins? °:[^\\n]*)(\\n\\*.*)?$', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();

			SELECT count(*) into @nbr
			FROM events
			INNER JOIN person_event USING(e_id)
			WHERE p_id = id2 and e_type = 'BAPT'
			;

			UPDATE events
			INNER JOIN person_event using(e_id)
			SET n_id = id3
			where p_id = id2 and e_type = 'BAPM' and n_id is null;
			set nbr = ROW_COUNT();
			IF nbr = 0 THEN
				INSERT events (e_type, n_id) values ('BAPM', id3);
				set id4 = last_insert_id();
				INSERT person_event (p_id, e_id, role) values (id2, id4, 'Main');
			ELSEIF nbr > 1 THEN
				select 'WARN', id1, 'affected', nbr;
			END IF;

			IF new = '' THEN
				UPDATE persons SET n_id = NULL WHERE p_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes set note = new where n_id = id1;
			END IF;
		ELSE
			select 'WARN', id1, n;
		END IF;

	END LOOP;

	CLOSE cur;

END$$

delimiter ;

call test();
call clean();

DROP PROCEDURE geneweb.test;

delimiter $$
CREATE PROCEDURE geneweb.test()
BEGIN
	DECLARE done INT DEFAULT FALSE;
	DECLARE id1, id2, id3, id4 INTEGER UNSIGNED;
	DECLARE n, new, v TEXT;
	DECLARE d, m, y VARCHAR(20);
	DECLARE cur CURSOR FOR
		select n_id, note, g_id
		from notes
		inner join groups using(n_id)
		where note rlike '(?s).*CM .*'
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;
	DECLARE EXIT HANDLER FOR SQLSTATE '22001' BEGIN
		select 'ERROR', id1, n, d, m, y;
        END;

	SELECT 'Parsing notes (CM -> events)...';

	OPEN cur;
b:	LOOP
		FETCH cur INTO id1, n, id2;

		IF done THEN
			LEAVE b;
		END IF;

		IF n RLIKE '(?s).*CM [0-9]{2}/[0-9]{2}/[0-9]{4} [^,]*\\[[^\\]]*\\].*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*\\[[^\\]]*\\][^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*\\[[^\\]]*\\][^,]*)(, )?.*', '\\1');
			set d = regexp_replace(n collate utf8_bin, '(?s).*CM ([0-9]{2})/[0-9]{2}/[0-9]{4} [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			set m = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/([0-9]{2})/[0-9]{4} [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[0-9]{2}/([0-9]{4}) [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, dmy1_d, dmy1_m, dmy1_y) values ('MARC',id3, d, m, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\([0-9]{2}/[0-9]{2}/[0-9]{4}\\) [^,]*\\[[^\\]]*\\].*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*\\[[^\\]]*\\][^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*\\[[^\\]]*\\][^,]*)(, )?.*', '\\1');
			set d = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\(([0-9]{2})/[0-9]{2}/[0-9]{4}\\) [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			set m = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\([0-9]{2}/([0-9]{2})/[0-9]{4}\\) [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\([0-9]{2}/[0-9]{2}/([0-9]{4})\\) [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, d_cal1, dmy1_d, dmy1_m, dmy1_y) values ('MARC', id3, 'French', d, m, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*CM [0-9]{2}/[0-9]{4} [^,]*\\[[^\\]]*\\].*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*\\[[^\\]]*\\][^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*\\[[^\\]]*\\][^,]*)(, )?.*', '\\1');
			set m = regexp_replace(n collate utf8_bin, '(?s).*CM ([0-9]{2})/[0-9]{4} [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/([0-9]{4}) [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, dmy1_d, dmy1_m, dmy1_y) values ('MARC',id3, 0, m, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*CM [0-9]{4} [^,]*\\[[^\\]]*\\].*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*\\[[^\\]]*\\][^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*\\[[^\\]]*\\][^,]*)(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM ([0-9]{4}) [^,]*\\[[^\\]]*\\][^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, dmy1_d, dmy1_m, dmy1_y) values ('MARC',id3, 0, 0, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*CM [0-9]{2}/[0-9]{2}/[0-9]{4}[^,]*.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*)(, )?.*', '\\1');
			set d = regexp_replace(n collate utf8_bin, '(?s).*CM ([0-9]{2})/[0-9]{2}/[0-9]{4}[^,]*(, )?.*', '\\1');
			set m = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/([0-9]{2})/[0-9]{4}[^,]*(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[0-9]{2}/([0-9]{4})[^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, dmy1_d, dmy1_m, dmy1_y) values ('MARC',id3, d, m, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\([0-9]{2}/[0-9]{2}/[0-9]{4}\\) [^,]*.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*)(, )?.*', '\\1');
			set d = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\(([0-9]{2})/[0-9]{2}/[0-9]{4}\\) [^,]*(, )?.*', '\\1');
			set m = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\([0-9]{2}/([0-9]{2})/[0-9]{4}\\) [^,]*(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/[A-Z]{2}/an[0-9]{2} \\([0-9]{2}/[0-9]{2}/([0-9]{4})\\) [^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, d_cal1, dmy1_d, dmy1_m, dmy1_y) values ('MARC', id3, 'French', d, m, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*CM [0-9]{2}/[0-9]{4}[^,]*.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*)(, )?.*', '\\1');
			set m = regexp_replace(n collate utf8_bin, '(?s).*CM ([0-9]{2})/[0-9]{4}[^,]*(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM [0-9]{2}/([0-9]{4})[^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, dmy1_d, dmy1_m, dmy1_y) values ('MARC',id3, 0, m, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSEIF n RLIKE '(?s).*CM [0-9]{4}[^,]*.*' THEN
			set new = regexp_replace(n collate utf8_bin, '(?s)CM [^,]*(, )?', '');
			set v = regexp_replace(n collate utf8_bin, '(?s).*(CM [^,]*)(, )?.*', '\\1');
			set y = regexp_replace(n collate utf8_bin, '(?s).*CM ([0-9]{4})[^,]*(, )?.*', '\\1');
			INSERT INTO notes (note) values (v);
			set id3 = last_insert_id();
			INSERT INTO events (e_type, n_id, dmy1_d, dmy1_m, dmy1_y) values ('MARC',id3, 0, 0, y);
			set id4 = last_insert_id();
			INSERT INTO group_event (e_id, g_id) values (id4, id2 );
			IF new = '' THEN
				UPDATE groups SET n_id = NULL WHERE g_id = id2;
				DELETE FROM notes WHERE n_id = id1;
			ELSE
				UPDATE notes SET note = new WHERE n_id = id1;
			END IF;
		ELSE
			select 'WARN', id1, n;
		END IF;

	END LOOP;

	CLOSE cur;

END$$

delimiter ;

call test();

DROP PROCEDURE geneweb.test;
DROP PROCEDURE geneweb.clean;
