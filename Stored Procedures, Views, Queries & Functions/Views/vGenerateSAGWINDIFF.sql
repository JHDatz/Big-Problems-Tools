DROP VIEW IF EXISTS merged.vGenerateSAGWINDIFF;

CREATE VIEW merged.vGenerateSAGWINDIFF AS
	SELECT 
		vgs.*, 
		concat(bat_home_id, ' ', inn_ct, ' ', start_state) as start_win_state, 
		merged.fixEndState(bat_home_id, home_score_ct, runs_scored, away_score_ct, inn_ct, game_end_fl, end_state) as end_win_state,
		wes1.prob as start_prob,
		wes2.prob as end_prob,
		1000*(wes1.prob - (1 - wes1.prob)) as start_SAGWINDIFF,
		1000*(wes2.prob - (1 - wes2.prob)) as end_SAGWINDIFF,
		1000*((wes2.prob - (1 - wes2.prob)) - (wes1.prob - (1 - wes1.prob))) as earnedSAGWINDIFF
	FROM 
		merged.vGenerateStates vgs
	LEFT JOIN
		merged.winExpectancySample wes1 ON
		concat(bat_home_id, ' ', inn_ct, ' ', start_state) = wes1.win_exp_state 
		AND (home_score_ct - away_score_ct) = wes1.differential
	LEFT JOIN
		merged.winExpectancySample wes2 ON
		merged.fixEndState(bat_home_id, home_score_ct, runs_scored, away_score_ct, inn_ct, game_end_fl, end_state) = wes2.win_exp_state 
		AND IF(bat_home_id = 1, home_score_ct - away_score_ct + runs_scored, home_score_ct - away_score_ct - runs_scored) = wes2.differential