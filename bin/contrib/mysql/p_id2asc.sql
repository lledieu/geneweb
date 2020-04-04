select @id := ifnull(@id, 460);

select @max_gen := ifnull(@max_gen, 100);

with recursive ancestors as (
 select 1 as "gen", cast("1" as decimal(20)) as "sosa", g_id, p_id
 from person_group
 where p_id = @id and role = 'Child'
union
 select a.gen+1, a.sosa*2+if(p.role = 'Parent2', 1, 0), c.g_id, p.p_id
 from person_group p
 inner join ancestors a on p.g_id = a.g_id and p.role in ('Parent1', 'Parent2')
 left join person_group c on p.p_id = c.p_id and p.role in ('Parent1', 'Parent2') and c.role = 'Child'
 where a.gen < @max_gen
)

-- Version complète
-- select anc.gen, anc.sosa, n.givn, n.surn
-- from ancestors anc
-- inner join person_name pn on anc.p_id = pn.p_id and n_type = 'Main'
-- inner join names n using(n_id)
-- ;

select anc2.gen, anc2.sosa, n.givn, n.surn
from
 (
  select min(anc.gen) as "gen", min(anc.sosa) as "sosa", p_id
  from ancestors anc
  group by p_id
  order by 1, 2
 ) as anc2
inner join person_name pn on anc2.p_id = pn.p_id and n_type = 'Main'
inner join names n using(n_id)
;
