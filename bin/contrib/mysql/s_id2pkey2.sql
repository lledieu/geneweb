-- Usage : MariaDB> set @id = <s_id> ; \. s_id2pkey2.sql
select e_type, persons.pkey2
from sources
inner join events using(s_id)
inner join person_event using(e_id)
inner join persons using(p_id)
where sources.s_id = @id and role='Main'
union
select "INDI", persons.pkey2
from sources
inner join persons using(s_id)
where sources.s_id = @id
union
select "FAM", persons.pkey2
from sources
inner join groups using(s_id)
inner join person_group using(g_id)
inner join persons using(p_id)
where sources.s_id = @id and role='Parent'
;
