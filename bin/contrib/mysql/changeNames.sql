insert into names
select distinct null, npfx, givn, nick, spfx, surn, nsfx from person_name;

update person_name
inner join names using (npfx, givn, nick, spfx, surn, nsfx)
set person_name.n_id = names.n_id;

alter table person_name
 drop column npfx,
 drop column givn,
 drop column nick,
 drop column spfx,
 drop column surn,
 drop column nsfx;
