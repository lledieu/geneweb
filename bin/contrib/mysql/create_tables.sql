DROP TABLE IF EXISTS php_notes_ind;
DROP TABLE IF EXISTS php_notes;

DROP TABLE IF EXISTS linked_notes_ind;
DROP TABLE IF EXISTS linked_notes_nt;
DROP TABLE IF EXISTS linked_notes;
DROP TABLE IF EXISTS person_media;
DROP TABLE IF EXISTS medias;
DROP TABLE IF EXISTS person_group;
DROP TABLE IF EXISTS groups;
DROP TABLE IF EXISTS person_event;
DROP TABLE IF EXISTS title_details;
DROP TABLE IF EXISTS occupation_details;
DROP TABLE IF EXISTS events;
DROP TABLE IF EXISTS person_name;
DROP TABLE IF EXISTS names;
DROP TABLE IF EXISTS persons;
DROP TABLE IF EXISTS sources;
DROP TABLE IF EXISTS notes;
DROP TABLE IF EXISTS places;
DROP TABLE IF EXISTS occupations;

CREATE TABLE places (
	pl_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	place	VARCHAR(120) NOT NULL,
	unique (place)
);

CREATE TABLE notes (
	n_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	note	TEXT
);

CREATE TABLE sources (
	s_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	source	TEXT
);

CREATE TABLE persons (
	p_id	INTEGER UNSIGNED PRIMARY KEY, -- value 0 not working with AUTO_INCREMENT
	pkey	VARCHAR(250) NOT NULL, -- FIXME not unique
	pkey2	VARCHAR(250) NOT NULL, -- removed after migration
	occ	TINYINT UNSIGNED NOT NULL,
	death	enum('NotDead','Dead','DeadYoung','DeadDontKnowWhen','DontKnowIfDead','OfCourseDead') NOT NULL,
	n_id	INTEGER UNSIGNED,
	s_id	INTEGER UNSIGNED,
	consang	DECIMAL(10,8) DEFAULT -1, -- FIXME spécifique
	sex	enum('','M','F') NOT NULL DEFAULT '',
	access	enum(
		'IfTitles',
		'Public',
		'Private'
	) NOT NULL DEFAULT 'IfTitles',
	FOREIGN KEY (n_id) REFERENCES notes(n_id),
	FOREIGN KEY (s_id) REFERENCES sources(s_id)
);

create index idx_persons_pkey2 on persons (pkey2);

CREATE TABLE names (
	n_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	npfx	VARCHAR(30) NOT NULL,
	givn	VARCHAR(120) NOT NULL,
	nick	VARCHAR(30) NOT NULL,
	spfx	VARCHAR(30) NOT NULL,
	surn	VARCHAR(120) NOT NULL,
	nsfx	VARCHAR(30) NOT NULL,
	unique(npfx, givn, nick, spfx, surn, nsfx)
);

CREATE TABLE person_name (
	pn_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	p_id	INTEGER UNSIGNED NOT NULL,
	n_id	INTEGER UNSIGNED,
	npfx	VARCHAR(30) NOT NULL,
	givn	VARCHAR(120) NOT NULL,
	nick	VARCHAR(30) NOT NULL,
	spfx	VARCHAR(30) NOT NULL,
	surn	VARCHAR(120) NOT NULL,
	nsfx	VARCHAR(30) NOT NULL,
	n_type	enum(
		'Main',
		'FirstNamesAlias',
		'SurnamesAlias',
		'Alias',
		'Qualifier',
		'PublicName'
	) NOT NULL DEFAULT 'Main',
	FOREIGN KEY (p_id) REFERENCES persons(p_id),
	FOREIGN KEY (n_id) REFERENCES names(n_id)
);

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
	t_name	VARCHAR(90),
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
		'INT'
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
	d_cal2	enum(
		'Gregorian',
		'Julian',     -- @#DJULIAN@
		'French',     -- @#DFRENCH R@
		'Hebrew'      -- @#DHEBREW@
	) NOT NULL DEFAULT 'Gregorian',
	dmy2_d	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	dmy2_m	TINYINT UNSIGNED NOT NULL DEFAULT 0,
	dmy2_y	SMALLINT NOT NULL DEFAULT 0,
	d_text	VARCHAR(35) NOT NULL DEFAULT '', -- FIXME deprecated
	-- Autres champs
        death_reason enum (
		'Killed',
		'Murdered',
		'Executed',
		'Disappeared',
		''
	) NOT NULL DEFAULT '',
	place	VARCHAR(120) NOT NULL DEFAULT '', -- Removed after migration
	pl_id	INTEGER UNSIGNED,
	n_id	INTEGER UNSIGNED,
	s_id	INTEGER UNSIGNED,
	FOREIGN KEY (pl_id) REFERENCES places(pl_id),
	FOREIGN KEY (n_id) REFERENCES notes(n_id),
	FOREIGN KEY (s_id) REFERENCES sources(s_id)
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
);

CREATE TABLE occupations (
	o_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	name	VARCHAR(200) NOT NULL, -- /!\ limité à 90 caractères selon GEDCOM 5.5.5
	unique (name)
);

CREATE TABLE occupation_details (
	e_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	name	VARCHAR(200) NOT NULL, -- removed after migration
	o_id	INTEGER UNSIGNED, 
	FOREIGN KEY (e_id) REFERENCES events(e_id),
	FOREIGN KEY (o_id) REFERENCES occupations(o_id)
);

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

CREATE TABLE groups (
	g_id	INTEGER UNSIGNED PRIMARY KEY, -- value 0 not working with AUTO_INCREMENT
	n_id	INTEGER UNSIGNED,
	s_id	INTEGER UNSIGNED,
	origin  VARCHAR(100) NOT NULL, -- FIXME spécifique
	FOREIGN KEY (n_id) REFERENCES notes(n_id),
	FOREIGN KEY (s_id) REFERENCES sources(s_id)
);

CREATE TABLE person_group (
	pg_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	g_id	INTEGER UNSIGNED NOT NULL,
	p_id	INTEGER UNSIGNED NOT NULL,
	role	ENUM (
		'Parent',
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
		'PgWizard'
	) NOT NULL,
	nkey	VARCHAR(100) NOT NULL,
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
	FOREIGN KEY (ln_id) REFERENCES linked_notes(ln_id),
	FOREIGN KEY (p_id) REFERENCES persons(p_id)
);
