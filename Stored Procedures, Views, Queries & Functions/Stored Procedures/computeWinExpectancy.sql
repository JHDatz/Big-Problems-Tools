# Created by: Joseph Datz
# February 23rd, 2021
#
# Material used to construct this procedure:
# 	The Book, Chapter 1
#	Mathletics, Chapter 8
#	Analyzing Baseball Data w/ R, Chapter 5
#
# This stored procedure is meant to compute the chance
# that a team has to win a particular game given a range
# of years to draw data from, the current score
# difference (from the home team perspective), the 24-out
# state that the team currently finds itself in, and the
# current inning.
#
# This is computed with the fraction of home teams that
# went on to win the game when reaching a particular
# 24-out state and inning. found_gameIDs is used to find
# what those games are and gameWins is used to identify
# which home teams have won that game when achieving
# that particular state and inning.
#
# "The Book" has the same concept of Win Expectancy as
# is used here (taken from Mathletics), but uses Markov
# Chain analysis to come to similar conclusions. Recreating
# this format of Win Expectancy is a work in progress.

DELIMITER //

DROP PROCEDURE IF EXISTS merged.computeWinExpectancy;

CREATE PROCEDURE merged.computeWinExpectancy(start_year int, end_year int, score_diff int, state varchar(5), inning int)
BEGIN

	DROP TEMPORARY TABLE IF EXISTS found_gameIDs;
	DROP TEMPORARY TABLE IF EXISTS gameWins;

	CREATE TEMPORARY TABLE found_gameIDs
		SELECT 
			DISTINCT game_id AS game_id2
		FROM 
			merged.vGenerateStates
		WHERE 
			year BETWEEN start_year AND end_year
			AND home_score_ct - away_score_ct = score_diff
			AND start_state = state
			AND inn_ct = inning
			AND bat_home_id = 1;

	CREATE TEMPORARY TABLE gameWins
		SELECT 
			game_ID, 
            IF(max(home_score_ct) >= max(away_score_ct), 1, 0) AS win
		FROM 
			merged.vGenerateStates gs
		INNER JOIN 
			found_gameIDs fg on fg.game_id2 = gs.game_id
		GROUP BY 
			game_ID;

	SELECT sum(win)/count(*) FROM gameWins;
    
END//
