-- Usage : MariaDB> set @id = <e_id> ; \. e_id2pkey2.sql
select e_type, persons.pkey2
from events
inner join person_event using(e_id)
inner join persons using(p_id)
where events.e_id = @id and role='Main'
union
select e_type, persons.pkey2
from events
inner join group_event using(e_id)
inner join person_group using(g_id)
inner join persons using(p_id)
where events.e_id = @id and role in ('Parent1','Parent2')
;
