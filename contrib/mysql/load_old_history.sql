load data
 local infile 'txt/old_history.tmp'
 into table old_history
 character set utf8mb4
 fields terminated by '||'
 (h_date, wizard, a, pkey2)
;
