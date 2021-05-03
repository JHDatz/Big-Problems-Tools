DELIMITER //

DROP FUNCTION IF EXISTS merged.fixEndState;

CREATE FUNCTION merged.fixEndState(bat_home_id int, home_score_ct int, runs_scored int, away_score_ct int, inn_ct int, game_end_fl int, end_state varchar(9))
RETURNS varchar(9)
DETERMINISTIC
BEGIN
	RETURN IF(game_end_fl = 1,
				IF(merged.wonGame(bat_home_id, home_score_ct, runs_scored, away_score_ct) = 1, "homeWins", "awayWins"),
			regexp_replace(concat(bat_home_id, ' ', inn_ct, ' ', end_state),
							'[0-1] [0-9] [0-1]{3} 3$', 
							concat(((bat_home_id + 1) mod 2), ' ', if(bat_home_id = 1, inn_ct + 1, inn_ct), ' ', '000 0'))
			);
END //