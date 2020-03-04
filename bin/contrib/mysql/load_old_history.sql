load data
 local infile 'old_history.tmp'
 into table old_history
 character set utf8
 fields terminated by '||'
 (h_date, wizard, a, pkey2)
;
