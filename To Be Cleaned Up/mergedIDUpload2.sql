show databases;

create database merged;

create database staging;

use lahman;

show tables;

create temporary table ids
select playerID, uuid() as mergedID, now() as CreateDate
from people;

create table merged.PlayerCrossRef(mergedID varchar(255), identifier varchar(255), source varchar(255), CreateDate datetime);

insert into merged.PlayerCrossRef
select mergedID, playerID, 'lahman', CreateDate from ids;

insert into merged.PlayerCrossRef
select mergedID, retroID, 'retrosheet', now()
from ids
inner join people p on p.playerID = ids.playerID;

insert into merged.PlayerCrossRef
select mergedID, bbrefID, 'baseballReference', now()
from ids
inner join people p on p.playerID = ids.playerID