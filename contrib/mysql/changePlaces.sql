insert into places
select distinct null, place from events;

update events
inner join places using (place)
set events.pl_id = places.pl_id
where events.place <> '';

alter table events drop column place;
