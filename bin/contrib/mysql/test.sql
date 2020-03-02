set @id = 17424;

select concat(givn, ' ', surn) from names where p_id = @id;

select concat(type, ' ', dmy1_d , '/', dmy1_m, '/', dmy1_y, ' ', place), role, n_id, s_id
from person_event
inner join events using(e_id)
inner join places using(pl_id)
where person_event.p_id = @id;

select
 pg1.role, pg1.seq,
 pg2.role, pg2.seq,
 n.givn, n.surn
from person_group pg1
inner join groups g1 on pg1.g_id = g1.g_id
inner join person_group pg2 on pg1.g_id = pg2.g_id
inner join names n on n.p_id = pg2.p_id
where pg1.p_id = @id
  and pg2.p_id <> @id
  and n.main = 'True'
order by 1, 2, 4;
