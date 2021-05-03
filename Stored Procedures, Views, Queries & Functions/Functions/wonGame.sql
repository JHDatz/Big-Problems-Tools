DELIMITER //

DROP FUNCTION IF EXISTS merged.wonGame;

CREATE FUNCTION merged.wonGame(bat_home_id int, home_score_ct int, runs_scored int, away_score_ct int)
RETURNS INT
DETERMINISTIC
BEGIN
	RETURN IF(bat_home_id = 1 AND home_score_ct + runs_scored > away_score_ct, 1, 0) +  IF(bat_home_id = 0 AND home_score_ct > away_score_ct + runs_scored, 1, 0);
END //