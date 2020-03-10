#!/bin/bash

MYSQL=./mysql.sh

liste=$($MYSQL -N << EOF
select group_concat(n_id) from notes where note like '%\%sm=IM;s=S-\%i.jpg%';
EOF
)

$MYSQL -t << EOF
insert into medias
select null, concat('src/Ledieu7/images/S-', pkey, '.jpg') from persons where n_id in ($liste);

insert into person_media
select
 null, p_id,
 (select m_id from medias where fname like concat('%S-', pkey, '%'))
from persons where n_id in ($liste);
EOF
