-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

create table 'srtm' as select * from 'first_srtm_file.csv'; 
insert into 'srtm' as select * from 'strtm_2.csv';