DELIMITER //

DROP PROCEDURE IF EXISTS merged.computeWinExpectancySampleTable;

CREATE PROCEDURE merged.computeWinExpectancySampleTable(start_year int, end_year int)
BEGIN

	DROP TEMPORARY TABLE IF EXISTS temp;
    DROP TEMPORARY TABLE IF EXISTS temp2;
    DROP TEMPORARY TABLE IF EXISTS temp3;

	CREATE TEMPORARY TABLE temp
		SELECT 
			game_id, 
			max(inn_ct), 
			max(abs(home_score_ct - away_score_ct))
		FROM 
			merged.vGenerateStates
		WHERE
			year BETWEEN start_year AND end_year
		GROUP BY
			game_id
		HAVING 
			max(inn_ct) < 10 AND max(abs(home_score_ct - away_score_ct)) < 11;

	CREATE TEMPORARY TABLE temp2
		SELECT
			vgs.game_id, 
			concat(bat_home_id, ' ', inn_ct, ' ', start_state) as win_exp_state, 
			home_score_ct - away_score_ct as differential
		FROM merged.vGenerateStates vgs
		INNER JOIN
			temp t on t.game_id = vgs.game_id;

	CREATE TEMPORARY TABLE temp3
		SELECT 
			vgs.game_id,
			if(bat_home_id = 1 and home_score_ct + runs_scored > away_score_ct, 1, 0) +  if(bat_home_id = 0 and home_score_ct > away_score_ct + runs_scored, 1, 0) as won_game
		FROM 
			merged.vGenerateStates vgs
		INNER JOIN
			temp t on t.game_id = vgs.game_id
		WHERE 
			game_end_fl = 1;
        
	DROP TABLE IF EXISTS merged.winExpectancySample;

	CREATE TABLE merged.winExpectancySample
		SELECT
			t2.win_exp_state, 
			t2.differential, 
			sum(won_game)/count(won_game) as prob
		FROM 
			temp2 t2
		INNER JOIN
			temp3 t3 on t3.game_id = t2.game_id
		GROUP BY
			win_exp_state, differential
		ORDER BY
			win_exp_state, differential;
            
	INSERT INTO merged.winExpectancySample (win_exp_state, differential, prob)
    VALUES
		('homeWins', 10, 1),
        ('homeWins', 9, 1),
        ('homeWins', 8, 1),
        ('homeWins', 7, 1),
        ('homeWins', 6, 1),
        ('homeWins', 5, 1),
        ('homeWins', 4, 1),
        ('homeWins', 3, 1),
        ('homeWins', 2, 1),
        ('homeWins', 1, 1),
        ('awayWins', -10, 0),
        ('awayWins', -9, 0),
        ('awayWins', -8, 0),
        ('awayWins', -7, 0),
        ('awayWins', -6, 0),
        ('awayWins', -5, 0),
        ('awayWins', -4, 0),
        ('awayWins', -3, 0),
        ('awayWins', -2, 0),
        ('awayWins', -1, 0);
	
    DROP TEMPORARY TABLE temp;
    DROP TEMPORARY TABLE temp2;
    DROP TEMPORARY TABLE temp3;
END//