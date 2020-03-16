select "pkey à revoir", count(*)
from persons where pkey = '.0.';

select "Personnes orphelines", count(*)
from persons p
where not exists (select 1 from names where p_id = p.p_id)
  and not exists (select 1 from person_event where p_id = p.p_id)
  and not exists (select 1 from person_group where p_id = p.p_id);

select ln_id, nkey, count(*)
from linked_notes
inner join linked_notes_ind using(ln_id)
where ln_type = 'PgPhp'
  and (nkey like 'CM-%' or nkey like 'M-%' or nkey like 'DOC-%-%')
  and role = 'Main'
group by 1, 2
having count(*) <> 2;

select ln_id, nkey, count(*)
from linked_notes
inner join linked_notes_ind using(ln_id)
where ln_type = 'PgPhp'
  and (nkey like 'N-%' or nkey like 'B-%' or nkey like 'D-%' or nkey like 'S-%' or nkey like 'DOC-%' or nkey like 'DOC_-%')
  and nkey not like 'DOC-%-%'
  and role = 'Main'
group by 1, 2
having count(*) <> 1;

