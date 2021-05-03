# Created by: Joseph Datz
# February 23rd, 2021
#
# Material used to construct this procedure:
# 	The Book, Chapter 1
#	Mathletics, Chapter 6
#	Analyzing Baseball Data w/ R, Chapter 5
#
# This procedure is referenced in:
#	computeRunExpectancy
#
# This stored procedure is meant to be the second half
# in computing the 24 base-out states. I had tried
# converting this to a view but it became woefully
# expensive to compute.
#
# The procedure grabs the view vGenerateStates between
# two years, start_year and end_year, and places it
# into the temporary table rawData. This period then
# has the outs of each half-inning, the runs scored,
# the runs at the beginning of each half-inning, and
# the total runs accumulated by the end of the inning
# computed in the temporary table summaries.
#
# This information is then joined back onto the temporary
# table rawData so that we can make the column needed for
# the procedure computeRunExpectancy: The runs until the
# rest of the inning, runs_roi.

DELIMITER \\

DROP PROCEDURE IF EXISTS merged.computeStates;

CREATE PROCEDURE merged.computeStates(start_year int, end_year int)
BEGIN

	DROP TEMPORARY TABLE IF EXISTS rawData;
	DROP TEMPORARY TABLE IF EXISTS summaries;
	DROP TEMPORARY TABLE IF EXISTS stateTracker;

	CREATE TEMPORARY TABLE rawData
		SELECT 
			*
		FROM 
			merged.vGenerateStates
		WHERE 
			YEAR BETWEEN start_year AND end_year;
    
	CREATE TEMPORARY TABLE summaries
		SELECT 
			half_inning,
			SUM(event_outs_ct) AS outs_inning,
			SUM(runs_scored) AS runs_inning,
			MIN(runs) AS runs_start,
			SUM(runs_scored) + MIN(runs) AS max_runs
		FROM merged.rawData
		GROUP BY half_inning;
    
	CREATE TEMPORARY TABLE stateTracker
		SELECT 
			rd.*, 
            outs_inning, 
            runs_inning, 
            runs_start, 
            max_runs, 
			max_runs - runs AS runs_roi
		FROM 
			merged.rawData rd
		INNER JOIN 
			summaries s ON rd.half_inning = s.half_inning;
    
	DROP TEMPORARY TABLE rawData;
	DROP TEMPORARY TABLE summaries;

END\\
