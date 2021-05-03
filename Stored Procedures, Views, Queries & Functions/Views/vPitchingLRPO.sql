DROP VIEW IF EXISTS lahman.vPitchingLRPO;

CREATE VIEW lahman.vPitchingLRPO AS
	SELECT 
		p.yearID,
		p.playerID,
		p.lgID,
		p.R,
		IF(p.IPOUTS is NULL, 0, p.IPOUTS) as IPOUTS,
		pr.POS,
		pr.Gtot
	FROM 
		pitching p
	INNER JOIN
		lahman.vPrimPos pr on pr.yearID = p.yearID and pr.playerID = p.playerID and pr.lgID = p.lgID
	WHERE
		pr.POS = 'P';