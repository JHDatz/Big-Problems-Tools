# Created by: Joseph Datz
# February 23rd, 2021
#
# A quick query for putting users on the MySQL Server.

show grants for admin;

CREATE USER 'blah'@'%' IDENTIFIED BY 'blah blah blah';

grant SELECT, INSERT, UPDATE, DELETE, CREATE, DROP, RELOAD, PROCESS, REFERENCES, INDEX, ALTER, 
SHOW DATABASES, CREATE TEMPORARY TABLES, LOCK TABLES, EXECUTE, REPLICATION SLAVE, REPLICATION CLIENT, 
CREATE VIEW, SHOW VIEW, CREATE ROUTINE, ALTER ROUTINE, EVENT, TRIGGER ON *.* 
TO `blah`@`%` WITH GRANT OPTION;

grant all privileges on lahman.* to 'blah'@'%';
grant all privileges on retrosheet.* to 'blah'@'%';
grant all privileges on gameday.* to 'blah'@'%';
grant all privileges on statcast.* to 'blah'@'%';
grant all privileges on staging.* to 'blah'@'%';
grant all privileges on merged.* to 'blah'@'%';
-- grant all privileges on figmentLeague.* to 'blah'@'%';
-- grant all privileges on imaginaryLeague.* to 'blah'@'%';

flush privileges;
