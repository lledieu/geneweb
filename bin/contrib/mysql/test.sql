set @id = 17424;
-- set @id = 17417;
-- set @id = 17418;

select n_type, givn, nick, surn
from names
inner join person_name using(n_id)
where p_id = @id;

select
 e_type, t_name,
 concat(' ', dmy1_d , '/', dmy1_m, '/', dmy1_y, ' ') as "date",
 ifnull(place,'') as "place",
 role,
 ifnull(n_id,'') as "note",
 ifnull(s_id,'') as "source"
from person_event
inner join events using(e_id)
left join places using(pl_id)
where person_event.p_id = @id
order by dmy1_y;

select
 name,
 group_concat(case d_prec
  when 'FROM-TO' then concat(dmy1_y, '-',  dmy2_y)
  else dmy1_y
 end order by dmy1_y separator ', ') as period
from events
inner join occupation_details using(e_id)
inner join occupations using(o_id)
where e_type = 'OCCU'
  and e_id in (select concat_ws(',',e_id) from person_event where p_id = @id and role = 'Main')
group by 1 order by period;

select
 pg1.role, pg1.seq,
 pg2.role, pg2.seq,
 n.givn, n.surn
from person_group pg1
inner join groups g1 on pg1.g_id = g1.g_id
inner join person_group pg2 on pg1.g_id = pg2.g_id
inner join person_name pn on pn.p_id = pg2.p_id
inner join names n on n.n_id = pn.n_id
where pg1.p_id = @id
  and pg2.p_id <> @id
  and pn.n_type = 'Main'
order by 1 desc, 2 asc, 3 asc, 4 asc;

select distinct case ln_type
  when 'PgInd' then concat( 'p_id: ', linked_notes.p_id)
  when 'PgFam' then concat( 'g_id: ', g_id)
  when 'PgNotes' then 'PgNotes'
  when 'PgMisc' then nkey
  when 'PgWizard' then concat( 'w:', nkey)
 end as 'Linked from'
from linked_notes_ind
inner join linked_notes using(ln_id)
where pkey = (select pkey from persons where p_id = @id)
union
select concat('PHP - ', nkey) as 'Linked from'
from php_notes
inner join php_notes_ind using (pn_id)
where pkey = (select pkey from persons where p_id = @id)
;

select
 h_date,
 action,
 old_history.pkey as "pkey(a)",
 history.pkey as "pkey",
 data, old, new
from transactions
left join history using(t_id)
left join old_history using(t_id)
left join history_details using(h_id)
where t_id in (
 select t_id
 from history
 where pkey = (select pkey from persons where p_id = @id)
);

select "pkey à revoir", count(*)
from persons where pkey = '.0.';

select "Personnes orphelines", count(*)
from persons p
where not exists (select 1 from names where p_id = p.p_id)
  and not exists (select 1 from person_event where p_id = p.p_id)
  and not exists (select 1 from person_group where p_id = p.p_id);
