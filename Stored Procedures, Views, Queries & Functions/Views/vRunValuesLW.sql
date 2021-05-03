DROP VIEW IF EXISTS lahman.vRunValuesLW;

CREATE VIEW lahman.vRunValuesLW AS
	SELECT
		yearID,
		lgID,
		sum(R) as totR,
		sum(IPOUTS) as totIPOUTS,
		sum(R)/sum(IPOUTS) as RperOut,
		sum(R)/sum(IPOUTS) + 0.14 as runBB,
		sum(R)/sum(IPOUTS) + 0.14 + 0.025 as runHBP,
		sum(R)/sum(IPOUTS) + 0.14 + 0.155 as run1B,
		sum(R)/sum(IPOUTS) + 0.14 + 0.155 + 0.3 as run2B,
		sum(R)/sum(IPOUTS) + 0.14 + 0.155 + 0.3 + 0.27 as run3B,
		1.4 as runHR,
		0.2 as runSB,
		2*sum(R)/sum(IPOUTS) + 0.075 as runCS
	FROM
		lahman.vpitchingLRPO
	WHERE
		lgID in ('NL', 'AL')
	GROUP BY
		yearID, lgID;