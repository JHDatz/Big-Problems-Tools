DROP VIEW IF EXISTS lahman.vYearBatting;

CREATE VIEW lahman.vYearBatting AS
	SELECT
		b.yearID,
		b.lgID,
		sum(AB) as ABtot,
		sum(R) as Rtot,
		SUM(H) as Htot,
		sum(X2B) as X2Btot,
		sum(X3B) as X3Btot,
		sum(HR) as HRtot,
		sum(SB) as SBtot,
		sum(CS) as CStot,
		sum(BB) as BBtot,
		sum(SO) as SOtot,
		sum(IBB) as IBBtot,
		sum(HBP) as HBPtot,
		sum(SF) as SFtot
	FROM
		batting b
	INNER JOIN
		vPrimPos pr on pr.playerID = b.playerID and pr.yearID = b.yearID and pr.lgID = b.lgID
	WHERE
		b.lgID in ('NL', 'AL')
	GROUP BY
		b.yearID, b.lgID
	HAVING
		sum(AB) IS NOT NULL
		and sum(R) IS NOT NULL
		and SUM(H) IS NOT NULL
		and sum(X2B) IS NOT NULL
		and sum(X3B) IS NOT NULL
		and sum(HR) IS NOT NULL
		and sum(SB) IS NOT NULL
		and sum(CS) IS NOT NULL
		and sum(BB) IS NOT NULL
		and sum(SO) IS NOT NULL
		and sum(IBB) IS NOT NULL
		and sum(HBP) IS NOT NULL
		and sum(SF) IS NOT NULL
	ORDER BY
		b.yearID, b.lgID;