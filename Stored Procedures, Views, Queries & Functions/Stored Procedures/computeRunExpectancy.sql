# Created by: Joseph Datz
# February 23rd, 2021
#
# Work in progress!
#
# Material used to construct this procedure:
# 	The Book, Chapter 1
#	Mathletics, Chapter 6
#	Analyzing Baseball Data w/ R, Chapter 5
#
# This procedure computes the RE24 matrix as a list
# when given a starting year and ending year. It calls
# computeStates2 to generate a retrosheet play-by-play
# table with 24 base-out states and the runs_roi column,
# and then computes the average runs_roi for each state.

DELIMITER //

DROP PROCEDURE IF EXISTS computeRunExpectancy;

CREATE PROCEDURE merged.computeRunExpectancy(start_year int, end_year int)
BEGIN

	CALL merged.computeStates(start_year, end_year);

	CREATE TEMPORARY TABLE re24_list
		SELECT 
			start_state, 
			ROUND(AVG(runs_roi), 2) AS avg_runs_roi,
			COUNT(*) AS No_Plate_Appearances
		FROM 
			stateTracker
		WHERE 
			start_state != end_state 
            OR runs_scored > 0 
            AND outs_inning = 3
		GROUP BY 
			start_state;
    
	DROP TEMPORARY TABLE stateTracker;

	SELECT 
		* 
	FROM 
		re24_list
	ORDER BY 
		start_state;
END//
