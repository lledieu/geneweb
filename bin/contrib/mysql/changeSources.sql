DROP TABLE IF EXISTS tmp1_sources;
CREATE TABLE tmp1_sources (
	s_id	INTEGER UNSIGNED PRIMARY KEY,
	source	TEXT,
	unique (source(500))
);

insert into tmp1_sources
select min(s_id), source
from sources
group by 2;

DROP TABLE IF EXISTS tmp2_sources;
CREATE TABLE tmp2_sources (
	s_id INTEGER UNSIGNED PRIMARY KEY,
	s_id_to   INTEGER UNSIGNED
);

insert into tmp2_sources
select t1.s_id, t2.s_id
from sources t1
inner join tmp1_sources t2 using (source)
where t1.s_id <> t2.s_id;

DROP TABLE tmp1_sources;

update persons
inner join tmp2_sources using (s_id)
set persons.s_id = tmp2_sources.s_id_to ;

update events
inner join tmp2_sources using (s_id)
set events.s_id = tmp2_sources.s_id_to ;

update groups
inner join tmp2_sources using (s_id)
set groups.s_id = tmp2_sources.s_id_to ;

delete from sources
where s_id in (select s_id from tmp2_sources);

DROP TABLE tmp2_sources;
