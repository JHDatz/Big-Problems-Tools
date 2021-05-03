-- A query whose purpose was to merge MLB identification information into the table merged.PlayerCrossRef.
-- In the future this will become a stored procedure to be used in "SQL Database Generator" for future
-- projects.

use statcast;

show tables;

select * from pitching;

show databases;

use staging;

insert into merged.PlayerCrossRef
select distinct mergedID, key_fangraphs, 'fangraphs', now()
from mlbIDScrape mlbs
inner join merged.PlayerCrossRef pxr on pxr.identifier = mlbs.key_retro
where pxr.source = 'retrosheet';

select * from merged.PlayerCrossRef;

delete from mlbIDScrape
where key_mlbam in (select identifier from merged.PlayerCrossRef where source = 'mlbam')
