DROP VIEW IF EXISTS lahman.vPrimPos;

CREATE VIEW lahman.vPrimPos AS
	SELECT 
		playerID,
		yearID,
		teamID,
		lgID,
		POS,
		sum(G) as Gtot
	FROM 
		fielding
	GROUP BY
		playerID, 
		yearID, 
		teamID,
		lgID,
		POS
	HAVING
		IF(POS = 'OF' and yearID > 1995, 1, 0) = 0;