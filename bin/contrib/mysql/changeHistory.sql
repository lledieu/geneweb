insert into users
select distinct null, wizard
from old_history;

update old_history
inner join users on user = wizard
set old_history.u_id = users.u_id, action = case a
 when 'ap' then 'addPerson'
 when 'mp' then 'modifyPerson'
 when 'dp' then 'deletePerson'
 when 'fp' then 'mergePersons'
 when 'si' then 'sendImage'
 when 'di' then 'deleteImage'
 when 'af' then 'addFamily'
 when 'mf' then 'modifyFamily'
 when 'df' then 'deleteFamily'
 when 'if' then 'invertFamilies'
 when 'ff' then 'mergeFamilies'
 when 'cn' then 'changeChildrenName'
 when 'aa' then 'addParents'
 when 'mn' then 'modifyNote'
 when 'cp' then 'modifyPlace'
 when 'cs' then 'modifySource'
 when 'co' then 'modifyOccupation'
 when 'fn' then 'modifyFirstName'
 when 'sn' then 'modifySurname'
end
;

update old_history
set pkey = ifnull((select pkey from persons where persons.pkey2=old_history.pkey2), 'Missing')
where pkey2 <> '?.0 ?' and action <> 'modifyNote';

alter table old_history modify column u_id integer unsigned not null;

alter table old_history drop column wizard;

alter table old_history drop column a;

update history
inner join users on user = wizard
set history.u_id = users.u_id;

alter table history modify column u_id integer unsigned not null;

alter table history drop column wizard;

insert into transactions
select distinct null, h_date, u_id from old_history;

insert ignore into transactions
select distinct null, h_date, u_id from history;

update old_history
inner join transactions using(h_date, u_id)
set old_history.t_id = transactions.t_id;

update history
inner join transactions using(h_date, u_id)
set history.t_id = transactions.t_id;

alter table history add foreign key (t_id) references transactions(t_id);

alter table old_history add foreign key (t_id) references transactions(t_id);

alter table history
 drop column h_date,
 drop column u_id;

alter table old_history
 drop column h_date,
 drop column u_id;
