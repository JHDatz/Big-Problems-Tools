use retrosheet;

show tables;

# 2020 Retrosheet Gamelogs

select * from gamelogs
where Date like '2020%'
order by Date, HomeTeam, DoubleHeader;

# 2020 Retrosheet PlayByPlay

create temporary table temp
select * from playByPlay
where game_id like '%2020%'
order by game_id, inn_ct, bat_lineup_id;

select * from temp;

# 2020 Retrosheet Players

create temporary table temp2 (IDs char(255));

insert into temp2
select distinct bat_id from temp;
insert into temp2
select distinct pit_id from temp;
insert into temp2
select distinct base1_run_id from temp;
insert into temp2
select distinct base2_run_id from temp;
insert into temp2
select distinct base3_run_id from temp;
insert into temp2
select distinct pos2_fld_id from temp;
insert into temp2
select distinct pos3_fld_id from temp;
insert into temp2
select distinct pos4_fld_id from temp;
insert into temp2
select distinct pos5_fld_id from temp;
insert into temp2
select distinct pos6_fld_id from temp;
insert into temp2
select distinct pos7_fld_id from temp;
insert into temp2
select distinct pos8_fld_id from temp;
insert into temp2
select distinct pos9_fld_id from temp;

select * from players
where playerid in (select ids from temp2);