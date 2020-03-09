DROP TABLE IF EXISTS history_details;
DROP TABLE IF EXISTS history;
DROP TABLE IF EXISTS old_history;
DROP TABLE IF EXISTS transactions;
DROP TABLE IF EXISTS users;

CREATE TABLE users (
	u_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	user	VARCHAR(20) NOT NULL
);

CREATE TABLE transactions (
	t_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	h_date	DATETIME NOT NULL,
	u_id	INTEGER UNSIGNED NOT NULL,
	FOREIGN KEY (u_id) REFERENCES users(u_id),
	unique (h_date, u_id)
);

CREATE TABLE history (
	h_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	t_id	INTEGER UNSIGNED,
	h_date	DATETIME NOT NULL,
	wizard	VARCHAR(20) NOT NULL, -- removed after migration
	u_id	INTEGER UNSIGNED,
	pkey	VARCHAR(250) NOT NULL
);

CREATE TABLE history_details (
	hd_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	h_id	INTEGER UNSIGNED,
	data	VARCHAR(200) NOT NULL,
	old	TEXT NOT NULL,
	new	TEXT NOT NULL,
	FOREIGN KEY (h_id) REFERENCES history(h_id)
);

CREATE TABLE old_history (
	oh_id	INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
	t_id	INTEGER UNSIGNED,
	h_date	DATETIME NOT NULL,
	wizard	VARCHAR(20) NOT NULL, -- removed after migration
	u_id	INTEGER UNSIGNED,
	a	CHAR(2) NOT NULL, -- removed after migration
	action	enum(
		'addPerson',
		'modifyPerson',
		'deletePerson',
		'mergePersons',
		'sendImage',
		'deleteImage',
		'addFamily',
		'modifyFamily',
		'deleteFamily',
		'invertFamilies',
		'mergeFamilies',
		'changeChildrenName',
		'addParents',
		'modifyNote',
		'modifyPlace',
		'modifySource',
		'modifyOccupation',
		'modifyFirstName',
		'modifySurname'
	) NOT NULL,
	pkey 	VARCHAR(250) NOT NULL,
	pkey2	VARCHAR(250) NOT NULL -- removed after migration
);
