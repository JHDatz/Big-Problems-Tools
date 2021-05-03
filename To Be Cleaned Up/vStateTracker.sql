CREATE VIEW merged.vStateTracker AS
    SELECT 
        *
    FROM
        merged.vGenerateStates vgs
	INNER JOIN 
		(SELECT 
			half_inning,
			SUM(EVENT_OUTS_CT) AS outs_inning,
			SUM(runs_scored) AS runs_inning,
			MIN(runs) AS runs_start,
			(SUM(runs_scored) + MIN(runs)) AS max_runs
		FROM
            merged.vGenerateStates
        GROUP BY 
			half_inning) s
	ON vgs.half_inning = s.half_inning