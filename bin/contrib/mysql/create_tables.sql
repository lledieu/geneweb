DROP TABLE IF EXISTS linked_notes_ind;
DROP TABLE IF EXISTS linked_notes_nt;
DROP TABLE IF EXISTS linked_notes;
DROP TABLE IF EXISTS person_media;
DROP TABLE IF EXISTS medias;
DROP TABLE IF EXISTS person_group;
DROP TABLE IF EXISTS group_event;
DROP TABLE IF EXISTS groups;
DROP TABLE IF EXISTS person_event;
DROP TABLE IF EXISTS title_details;
DROP TABLE IF EXISTS occupation_details;
DROP TABLE IF EXISTS death_details;
DROP TABLE IF EXISTS event_dmy2;
DROP TABLE IF EXISTS event_values;
DROP TABLE IF EXISTS events;
DROP TABLE IF EXISTS person_name;
DROP TABLE IF EXISTS names;
DROP TABLE IF EXISTS names_givn;
DROP TABLE IF EXISTS names_nick;
DROP TABLE IF EXISTS names_surn;
DROP TABLE IF EXISTS names_crush;
DROP TABLE IF EXISTS persons;
DROP TABLE IF EXISTS representations;
DROP TABLE IF EXISTS sources;
DROP TABLE IF EXISTS notes;
DROP TABLE IF EXISTS places;
DROP TABLE IF EXISTS occupations;
DROP TABLE IF EXISTS origins;
DROP TABLE IF EXISTS particles;
DROP TABLE IF EXISTS caches;

CREATE TABLE places (
	pl_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	place	VARCHAR(120) collate utf8_bin NOT NULL UNIQUE
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE notes (
	n_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	note	TEXT
);

CREATE TABLE sources (
	s_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	source	TEXT
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE persons (
	p_id	INTEGER UNSIGNED PRIMARY KEY, -- value 0 not working with AUTO_INCREMENT
	pkey	VARCHAR(250) NOT NULL, -- FIXME not unique
	pkey2	VARCHAR(250) NOT NULL, -- FIXME removed after migration
	occ	TINYINT UNSIGNED NOT NULL,
	death	enum('NotDead','Dead','DeadYoung','DeadDontKnowWhen','DontKnowIfDead','OfCourseDead') NOT NULL,
	n_id	INTEGER UNSIGNED,
	s_id	INTEGER UNSIGNED,
	consang	DECIMAL(10,8) DEFAULT -1, -- FIXME spécifique
	sex	enum('','M','F') NOT NULL DEFAULT '',
	access	enum(
		'IfTitles',
		'Public',
		'Private',
		'Friend', -- Roglo specific
		'Friend_m' -- Roglo specific
	) NOT NULL DEFAULT 'IfTitles',
	FOREIGN KEY (n_id) REFERENCES notes(n_id),
	FOREIGN KEY (s_id) REFERENCES sources(s_id),
	INDEX(pkey)
);

create index idx_persons_pkey2 on persons (pkey2);

CREATE TABLE names_crush (
	nac_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	crush	VARCHAR(120) NOT NULL UNIQUE
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE names_givn (
	nag_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	givn	VARCHAR(120) NOT NULL UNIQUE,
	givn_l	VARCHAR(120) NOT NULL,
	nac_id	INTEGER UNSIGNED NOT NULL,
	FOREIGN KEY (nac_id) REFERENCES names_crush(nac_id),
	INDEX (givn_l)
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE names_nick (
	nan_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	nick	VARCHAR(30) NOT NULL UNIQUE
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE names_surn (
	nas_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	surn	VARCHAR(120) NOT NULL UNIQUE,
	surn_l	VARCHAR(120) NOT NULL,
	surn_wp	VARCHAR(120) NOT NULL,
	part	VARCHAR(30) NOT NULL,
	surn_p	VARCHAR(120) AS (if(part='',surn,concat(surn_wp,part))) VIRTUAL,
	nac_id	INTEGER UNSIGNED NOT NULL,
	FOREIGN KEY (nac_id) REFERENCES names_crush(nac_id),
	INDEX(surn_l),
	INDEX(surn_wp),
	INDEX(surn_p)
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE names (
	na_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	nag_id	INTEGER UNSIGNED,
	nan_id	INTEGER UNSIGNED,
	nas_id	INTEGER UNSIGNED,
	nac_id	INTEGER UNSIGNED NOT NULL,
	unique(nag_id, nan_id, nas_id),
	FOREIGN KEY (nag_id) REFERENCES names_givn(nag_id),
	FOREIGN KEY (nan_id) REFERENCES names_nick(nan_id),
	FOREIGN KEY (nas_id) REFERENCES names_surn(nas_id),
	FOREIGN KEY (nac_id) REFERENCES names_crush(nac_id)
);

CREATE TABLE person_name (
	pn_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	p_id	INTEGER UNSIGNED NOT NULL,
	na_id	INTEGER UNSIGNED,
	givn	VARCHAR(120) NOT NULL,
	givn_l	VARCHAR(120) NOT NULL,
	givn_c	VARCHAR(120) NOT NULL,
	nick	VARCHAR(30) NOT NULL,
	surn	VARCHAR(120) NOT NULL,
	surn_l	VARCHAR(120) NOT NULL,
	surn_wp	VARCHAR(120) NOT NULL,
	part	VARCHAR(30) NOT NULL,
	surn_c	VARCHAR(120) NOT NULL,
	crush	VARCHAR(120) NOT NULL,
	n_type	enum(
		'Main',
		'FirstNamesAlias',
		'SurnamesAlias',
		'Alias',
		'Qualifier',
		'PublicName'
	) NOT NULL DEFAULT 'Main',
	FOREIGN KEY (p_id) REFERENCES persons(p_id),
	FOREIGN KEY (na_id) REFERENCES names(na_id),
	INDEX(n_type)
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE events (
	e_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	-- Type
	e_type	ENUM(
		-- From INDIVIDUAL_EVENT_STRUCTURE
		'BIRT',
		'CHR',
		'DEAT',
		'BURI',
		'CREM',
		'ADOP',
		'BAPM',
		'BARM',
		'BASM',
		'CHRA',
		'CONF',
		'FCOM',
		'NATU',
		'EMIG',
		'IMMI',
		'CENS',
		'PROB',
		'WILL',
		'GRAD',
		'RETI',
		'EVEN',
		-- From INDIVIDUAL_ATTRIBUTE_STRUCTURE
		'CAST',
		'DSCR',
		'EDUC',
		'IDNO',
		'NATI',
		'NCHI',
		'NMR',
		'OCCU',
		'PROP',
		'RELI',
		'RESI',
		'TITL',
		'FACT',
		-- From FAMILY_EVENT_STRUCTURE
		'ANNU',
		'DIV',
		'DIVF',
		'ENGA',
		'MARB',
		'MARC',
		'MARR',
		'MARL',
		'MARS'
	) NOT NULL,
	t_name	VARCHAR(90) NOT NULL DEFAULT '',
	-- Date
	d_prec	enum(
		'',
		'ABT',
		'Maybe', -- FIXME à supprimer
		'CAL',
		'EST',
		'BEF',
		'AFT',
		'BET-AND', -- FIXME privilégier 2 événements avec AFT et BEF ?
		'FROM',
		'TO',
		'FROM-TO',
		'INT',
                'OrYear', -- FIXME privilégier 2 événements ?
		'YearInt' -- FIXME privilégier 2 événements ?
	) NOT NULL DEFAULT '',
	d_cal1	enum(
		'Gregorian',
		'Julian',     -- @#DJULIAN@
		'French',     -- @#DFRENCH R@
		'Hebrew'      -- @#DHEBREW@
	) NOT NULL DEFAULT 'Gregorian',
	dmy1_d	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	dmy1_m	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	dmy1_y	SMALLINT NOT NULL DEFAULT 0,
	d_text	VARCHAR(35) NOT NULL DEFAULT '', -- FIXME deprecated
	-- Autres champs
	pl_id	INTEGER UNSIGNED,
	n_id	INTEGER UNSIGNED,
	s_id	INTEGER UNSIGNED,
	FOREIGN KEY (pl_id) REFERENCES places(pl_id),
	FOREIGN KEY (n_id) REFERENCES notes(n_id),
	FOREIGN KEY (s_id) REFERENCES sources(s_id)
);

CREATE TABLE death_details (
	e_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
        reason enum (
		'Killed',
		'Murdered',
		'Executed',
		'Disappeared'
	) NOT NULL,
	FOREIGN KEY (e_id) REFERENCES events(e_id)
);

CREATE TABLE event_dmy2 (
	e_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	d_cal2	enum(
		'Gregorian',
		'Julian',     -- @#DJULIAN@
		'French',     -- @#DFRENCH R@
		'Hebrew'      -- @#DHEBREW@
	) NOT NULL DEFAULT 'Gregorian',
	dmy2_d	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	dmy2_m	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	dmy2_y	SMALLINT NOT NULL DEFAULT 0,
	FOREIGN KEY (e_id) REFERENCES events(e_id)
);

CREATE TABLE event_values (
	e_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	attr	VARCHAR(2000) NOT NULL,
	FOREIGN KEY (e_id) REFERENCES events(e_id),
	CHECK( JSON_VALID(attr) )
);

CREATE TABLE title_details (
	e_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	ident	VARCHAR(120) NOT NULL,
	place	VARCHAR(120) NOT NULL DEFAULT '',
	nth	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	main	enum( 'True', 'False') NOT NULL DEFAULT 'False',
	name	VARCHAR(120) NOT NULL DEFAULT '',
	-- FROM
	d1_prec	enum(
		'',
		'ABT',
		'Maybe',
		'BEF',
		'AFT',
		'OrYear',
		'YearInt'
	) NOT NULL DEFAULT '',
	d1_cal	enum(
		'Gregorian',
		'Julian',
		'French',
		'Hebrew'
	) NOT NULL DEFAULT 'Gregorian',
	d1_dmy1_d TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d1_dmy1_m TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d1_dmy1_y SMALLINT NOT NULL DEFAULT 0,
	d1_dmy2_d TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d1_dmy2_m TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d1_dmy2_y SMALLINT NOT NULL DEFAULT 0,
	d1_text	VARCHAR(35) NOT NULL DEFAULT '',
	-- TO
	d2_prec	enum(
		'',
		'ABT',
		'Maybe',
		'BEF',
		'AFT',
		'OrYear',
		'YearInt'
	) NOT NULL DEFAULT '',
	d2_cal	enum(
		'Gregorian',
		'Julian',
		'French',
		'Hebrew'
	) NOT NULL DEFAULT 'Gregorian',
	d2_dmy1_d TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d2_dmy1_m TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d2_dmy1_y SMALLINT NOT NULL DEFAULT 0,
	d2_dmy2_d TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d2_dmy2_m TINYINT UNSIGNED NOT NULL DEFAULT 0,
	d2_dmy2_y SMALLINT NOT NULL DEFAULT 0,
	d2_text	VARCHAR(35) NOT NULL DEFAULT '',
	FOREIGN KEY (e_id) REFERENCES events(e_id)
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE occupations (
	o_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	name	VARCHAR(200) NOT NULL UNIQUE -- /!\ limited to 90 with GEDCOM 5.5.5
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE occupation_details (
	e_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	name	VARCHAR(200) NOT NULL, -- removed after migration
	o_id	INTEGER UNSIGNED, 
	FOREIGN KEY (e_id) REFERENCES events(e_id),
	FOREIGN KEY (o_id) REFERENCES occupations(o_id)
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE person_event (
	pe_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	e_id	INTEGER UNSIGNED NOT NULL,
	p_id	INTEGER UNSIGNED NOT NULL,
	role	ENUM (
		'Main',
		'Witness',
		'AdoptionParent', -- FIXME -> person_group ?
		'RecognitionParent', -- FIXME -> person_group ?
		'CandidateParent', -- FIXME -> person_group ?
		'GodParent',
		'FosterParent',
		'Official'
	) NOT NULL,
	FOREIGN KEY (p_id) REFERENCES persons(p_id),
	FOREIGN KEY (e_id) REFERENCES events(e_id)
);

CREATE TABLE origins (
	o_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	origin	VARCHAR(100) NOT NULL UNIQUE
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE groups (
	g_id	INTEGER UNSIGNED PRIMARY KEY, -- value 0 not working with AUTO_INCREMENT
	n_id	INTEGER UNSIGNED,
	s_id	INTEGER UNSIGNED,
	o_id	INTEGER UNSIGNED,
	FOREIGN KEY (n_id) REFERENCES notes(n_id),
	FOREIGN KEY (s_id) REFERENCES sources(s_id),
	FOREIGN KEY (o_id) REFERENCES origins(o_id)
);

CREATE TABLE group_event (
	ge_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	e_id	INTEGER UNSIGNED NOT NULL,
	g_id	INTEGER UNSIGNED NOT NULL,
	FOREIGN KEY (g_id) REFERENCES groups(g_id),
	FOREIGN KEY (e_id) REFERENCES events(e_id)
);

CREATE TABLE person_group (
	pg_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	g_id	INTEGER UNSIGNED NOT NULL,
	p_id	INTEGER UNSIGNED NOT NULL,
	role	ENUM (
		'Parent1',
		'Parent2',
		'Child'
	) NOT NULL,
	seq	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	FOREIGN KEY (g_id) REFERENCES groups(g_id),
	FOREIGN KEY (p_id) REFERENCES persons(p_id)
);

CREATE TABLE medias (
	m_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	fname	VARCHAR(200) NOT NULL
);

CREATE TABLE person_media (
	pm_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	p_id	INTEGER UNSIGNED NOT NULL,
	m_id	INTEGER UNSIGNED NOT NULL,
	FOREIGN KEY (p_id) REFERENCES persons(p_id),
	FOREIGN KEY (m_id) REFERENCES medias(m_id)
);

CREATE TABLE linked_notes (
	ln_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	ln_type enum (
		'PgInd',
		'PgFam',
		'PgNotes',
		'PgMisc',
		'PgWizard',
		'PgPhp' -- Specific
	) NOT NULL,
	nkey	VARCHAR(100) NOT NULL UNIQUE,
	p_id	INTEGER UNSIGNED,
	g_id	INTEGER UNSIGNED,
	FOREIGN KEY (p_id) REFERENCES persons(p_id),
	FOREIGN KEY (g_id) REFERENCES groups(g_id)
);

CREATE TABLE linked_notes_nt (
	lnn_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	ln_id	INTEGER UNSIGNED NOT NULL,
	nkey	VARCHAR(100) NOT NULL,
	FOREIGN KEY (ln_id) REFERENCES linked_notes(ln_id)
);

CREATE TABLE linked_notes_ind (
	lni_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	ln_id	INTEGER UNSIGNED NOT NULL,
	pkey	VARCHAR(250) NOT NULL,
	p_id	INTEGER UNSIGNED,
	text	VARCHAR(200),
	pos	INTEGER NOT NULL,
	role	enum( '', 'Main' ) NOT NULL DEFAULT '', -- Specific
	FOREIGN KEY (ln_id) REFERENCES linked_notes(ln_id),
	FOREIGN KEY (p_id) REFERENCES persons(p_id)
);

CREATE TABLE particles (
	pa_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	particle	VARCHAR(10) NOT NULL UNIQUE
)
CHARACTER SET 'utf8' COLLATE 'utf8_bin'
;

CREATE TABLE caches (
	name	enum(
		'ascends',
		'couples'
		) PRIMARY KEY,
	object	LONGBLOB NOT NULL
);
