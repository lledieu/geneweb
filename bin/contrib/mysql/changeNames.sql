insert into names_crush
select distinct null, givn_c from person_name
union
select distinct null, surn_c from person_name
union
select distinct null, crush from person_name
;

insert into names_givn
select distinct null, givn, givn_l, nac_id from person_name pn
inner join names_crush nc on nc.crush = pn.givn_c
;

insert into names_nick
select distinct null, nick from person_name
;

insert into names_surn (surn, surn_l, surn_wp, part, nac_id)
select distinct surn, surn_l, surn_wp, part, nac_id from person_name pn
inner join names_crush nc on nc.crush = pn.surn_c
;

insert into names
select distinct null, nag_id, nan_id, nas_id, names_crush.nac_id
from person_name
inner join names_givn using(givn)
inner join names_nick using(nick)
inner join names_surn using(surn)
inner join names_crush using(crush)
;

update person_name
inner join names_givn ng using(givn)
inner join names_nick nn using(nick)
inner join names_surn ns using(surn)
inner join names_crush nc using(crush)
inner join names n on n.nag_id = ng.nag_id and n.nan_id = nn.nan_id and n.nas_id = ns.nas_id and n.nac_id = nc.nac_id
set person_name.na_id = n.na_id
;

alter table person_name
 drop column givn,
 drop column givn_l,
 drop column givn_c,
 drop column nick,
 drop column surn,
 drop column surn_l,
 drop column surn_wp,
 drop column part,
 drop column surn_c,
 drop column crush
;

update names
inner join names_givn using (nag_id)
set names.nag_id = null where givn = ''
;

delete from names_givn where givn = '';

update names
inner join names_nick using (nan_id)
set names.nan_id = null where nick = ''
;

delete from names_nick where nick = '';

update names
inner join names_surn using (nas_id)
set names.nas_id = null where surn = ''
;

delete from names_surn where surn = '';
