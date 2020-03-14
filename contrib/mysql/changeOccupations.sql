insert into occupations
select distinct null, name from occupation_details;

update occupation_details
inner join occupations using (name)
set occupation_details.o_id = occupations.o_id;

alter table occupation_details drop column name;
