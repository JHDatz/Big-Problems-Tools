# Created by: Joseph Datz
# February 23rd, 2021
#
# Material used to construct this procedure:
# 	The Book, Chapter 1
#	Mathletics, Chapter 8
#	Analyzing Baseball Data w/ R, Chapter 5
#
# This view is referenced in:
#	computeRunExpectancy
#	computeWinExpectancy
#
# This view is the first half of generating states for
# Markov Chain analysis. It creates a column of
# starting states and ending states (listed as start_state
# and end_state in columns). Here is an example of how to
# read them:
#
# '011 2'
#
# Reads as two runners on second and third with two outs.

CREATE VIEW merged.vGenerateStates AS
    SELECT 
	*,
	SUBSTR(GAME_ID, 4, 4) AS year,
	AWAY_SCORE_CT + HOME_SCORE_CT AS runs,
	CONCAT(GAME_ID, INN_CT, BAT_HOME_ID) AS half_inning,
	IF(BAT_DEST_ID > 3, 1, 0) + IF(RUN1_DEST_ID > 3, 1, 0) + IF(RUN2_DEST_ID > 3, 1, 0) + 			IF(RUN3_DEST_ID > 3, 1, 0) AS runs_scored,
	CONCAT(IF((BASE1_RUN_ID IS NOT NULL), 1, 0), IF((BASE2_RUN_ID IS NOT NULL), 1, 0), 			IF((BASE3_RUN_ID IS NOT NULL), 1, 0), ' ', OUTS_CT) AS start_state,
	CONCAT(IF(((RUN1_DEST_ID = 1) OR (BAT_DEST_ID = 1)), 1, 0),
		IF(((RUN1_DEST_ID = 2) OR (BAT_DEST_ID = 2) OR (RUN2_DEST_ID = 2)), 1, 0), 
		IF(((RUN1_DEST_ID = 3) OR (BAT_DEST_ID = 3) OR (RUN2_DEST_ID = 3) OR 		(RUN3_DEST_ID = 3)), 1, 0), ' ', (OUTS_CT + EVENT_OUTS_CT)) AS end_state
    FROM
        retrosheet.playByPlay
