-- Usage : MariaDB> set @id = <n_id> ; \. n_id2pkey2.sql
select e_type, persons.pkey2
from notes
inner join events using(n_id)
inner join person_event using(e_id)
inner join persons using(p_id)
where notes.n_id = @id and role='Main'
union
select e_type, persons.pkey2
from notes
inner join events using(n_id)
inner join group_event using(e_id)
inner join person_group using(g_id)
inner join persons using(p_id)
where notes.n_id = @id and role in ('Parent1','Parent2')
union
select "INDI", persons.pkey2
from notes
inner join persons using(n_id)
where notes.n_id = @id
union
select "FAM", persons.pkey2
from notes
inner join groups using(n_id)
inner join person_group using(g_id)
inner join persons using(p_id)
where notes.n_id = @id and role in ('Parent1','Parent2')
;
