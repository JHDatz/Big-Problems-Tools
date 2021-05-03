/* 	
	Mathletics Code-Along
	Written by Joe Datz

    Worked on during the majority of the 2021 semester. The goal of this Code-Along is to be:

		1. A reference for students so that they can familiarize themselves with MySQL coding.
		2. A means to build a map in students' minds of how the MySQL server is layed out.


	To be added in later:

		1. Chapter 11: Streakiness in Sports, when I have a better understanding of stats.
		2. More of Chapter 7: Fielding, when I have a copy of the "Fielding Bible."

	To start, we'll need to tell MySQL what database we intend to use. All of our future
    statements, unless otherwise specified, will assume we're talking about tables in this
    database.
*/

USE lahman;

/* 
	Chapter 1: Baseball's Pythagorean Theorem

	There is a minor difference in answers from book due to sigfigs being used.

	We start off by recreating Baseball's Pythagorean theorem the "predicted_winLoss" column.
	After that, we create the absolute_error column to see how far off this usually is.
*/

CREATE TEMPORARY TABLE temp
	SELECT 
		yearID, 
		teamID, 
		W, 
		L, 
		R, 
		RA,
		R/RA AS scoring_ratio,
		W/(W+L) AS winLoss,
		POWER(R/RA, 2)/(POWER(R/RA, 2) + 1) AS predicted_winLoss,
		ABS(W/(W+L) - POWER(R/RA, 2)/(POWER(R/RA, 2) + 1)) AS absolute_error
	FROM 
		teams
	WHERE 
		yearID BETWEEN 1980 AND 2006
	ORDER BY 
		yearID DESC;

SELECT 
	AVG(absolute_error) 
FROM 
	temp;

DROP TEMPORARY TABLE temp;

/*
	As we can see from our computations, the average absolute error is pretty good at 2%.
    This means that when using this formula to predict the record of the team during the
    season, on average we'll be off by 162*0.02 = 3.2 games per season.
    
    This alternate_pythag_theorem is equivalent to the prior one. I'm presenting it here to
    show how it would look when coding it up in MySQL.
*/

SELECT 
	*,
	POWER(scoring_ratio, 2)/(POWER(scoring_ratio, 2) + 1) AS alternate_pythag_theorem
FROM 
	temp;
    
/* 
	The best choice for the exponent in the alternate pythagorean theorem is a bit more
    laborsome to code in MySQL, so I'm saving this instead for the complimentary R file.
    
    Chapter 2: Runs-Created Approach
    
    Like earlier, we'll select columns from the team table and create new ones out of existing
    ones. The "runs_created" column adds the Runs Created column from the book; the
    "absolute_error" column starts off the formula for finding the mean absolute error, and
    "squared_error" column is starting off the formula for the mean squared error, because I
    think this author is crazy for not including this.
*/


CREATE TEMPORARY TABLE temp
	SELECT 
		yearID, 
        R, 
        AB, 
        H,
		H - X2B - X3B - HR AS singles,
		X2B, 
        X3B, 
        HR,
		BB + HBP AS Walks,
		(H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP) AS runs_created,
		ABS(R - (H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP)) AS absolute_error,
		POWER(R - (H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP), 2) AS squared_error,
		teamID
	FROM 
		teams
	WHERE 
		yearID IN (2000, 2006);

SELECT AVG(absolute_error), SQRT(AVG(squared_error)) FROM temp;

DROP TEMPORARY TABLE temp;

/*
	As we can see above, the absolute errors that we're off on with respect to the mean
    absolute error or the square root of the MSE are pretty small. This makes it a decent
    model, at least at the team level. To try this formula on individual players, we'll have
    to move on from the teams table to the batting table.
    
	We'll start with creating the "runs_created" column for Ichiro and Barry Bonds. For reference:

		Ichiro's ID in Lahman: 'suzukic01'
        Bonds' ID in Lahman: 'bondsba01'
	
    We'll also create the "games_outs_used" formula using the information from the second formula.
    This will help us get a sense of how many outs these players have created through their
    performance. We'll then combine these two formulas to create the "runs_created_per_game" column.
*/

CREATE TEMPORARY TABLE temp
	SELECT 
		*, 
		(H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP) AS runs_created,
		(.982*AB - H + GIDP + SF + SH + CS)/26.72 as game_outs_used
	FROM 
		batting
	WHERE 
		playerid IN ('bondsba01', 'suzukic01')
		AND yearid = 2004;
        
SELECT
	*,
    runs_created/game_outs_used as runs_created_per_game
FROM
	temp;
    
DROP TEMPORARY TABLE temp;


/*
	I didn't add Anthony Nomar to the list, but his information can be reached with the same
    lines of reasoning in MySQL code.
    
    To see how well this formula goes for individual players, I'll now apply the same process
    we did for Chapter 1 and 2 for getting the mean absolute error and mean squared error for
    individual batters.
    
*/

CREATE TEMPORARY TABLE temp
	SELECT 
		*, 
		(H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP) AS runs_created,
		(.982*AB - H + GIDP + SF + SH + CS)/26.72 as game_outs_used,
        ((H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP))*26.72/(.982*AB - H + GIDP + SF + SH + CS) as runs_created_per_game,
        abs(R - ((H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP))*26.72/(.982*AB - H + GIDP + SF + SH + CS)) as absolute_error
	FROM 
		batting
	WHERE
		yearid = 2004;
        
SELECT
	AVG(absolute_error)
FROM
	temp;
    
DROP TEMPORARY TABLE temp;

/*
	As we can see above, this is a rather high absolute error for individual players.
    
    Chapter 3: Linear Weights

	The Linear Regression is not done here. I'll just be using the weights provided and save 
    that for the R file. On page 23, Winston has made a mistake in his formula. We also need 
    to subtract by H for computed_outs, which is added back into the SQL code below.
*/

SELECT 
	*, 
	(H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP) AS james_runs_created,
	((H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP))*26.72/(.982*AB - H + GIDP + SF + SH + CS) AS james_runs_created_per_game,
	.982*AB - H + SH + SF + CS + GIDP AS computed_outs,
	4329/(.982*AB - H + SH + SF + CS + GIDP) AS scale_factor,
	-560 + (4329/(.982*AB - H + SH + SF + CS + GIDP))*(.63*(H - X2B - X3B - HR) + 0.71*X2B + 1.26*X3B + 1.49*HR + 0.35*(BB + HBP)) AS linear_weights_runs,
	(-560 + (4329/(.982*AB - H + SH + SF + CS + GIDP))*(.63*(H - X2B - X3B - HR) + 0.71*X2B + 1.26*X3B + 1.49*HR + 0.35*(BB + HBP)))/162 AS linear_weights_runs_per_game
FROM 
	batting
WHERE 
	playerid IN ('bondsba01', 'suzukic01')
	AND yearid = 2004;

/* 
	Chapter 4: Monte Carlo Simulations

    The Monte Carlo simulator is reserved for the R file; it'd be a nightmare to code up in 
    SQL. We'll just show how the data might be gathered in this file. This code snippet is 
    used in the R functions as well for gathering data.
    
	For some reason sacrifice hits aren't counted in the total plate appearances on the teams 
    table, so we'll have to gather that from the batting table. We can then just exclude 
    Pujols with a "not in" statement.

*/

SELECT 
	SUM(AB + BB + SH + SF + HBP) AS PA,
	ROUND(SUM(0.018*AB), 0) AS Errors,
	ROUND(SUM(AB + SF + SH - H - 0.018*AB - SO), 0) AS OutsInPlay,
	SUM(SO), SUM(BB), SUM(HBP),
	SUM(H - X2B - X3B - HR) AS Singles,
	SUM(X2B), SUM(X3B), SUM(HR)
FROM 
	batting
WHERE 
	teamID = 'SLN' 
    AND yearid = 2006 
    AND playerID NOT IN ('pujolal01');

-- For pujols as an individual:

SELECT 
	AB + BB + SH + SF + HBP AS PA,
	ROUND(0.018*AB, 0) AS Errors,
	AB + SF + SH - H - ROUND(0.018*AB, 0) - SO AS OutsInPlay,
	SO, 
    BB, 
    HBP,
	H - X2B - X3B - HR AS Singles,
	X2B, 
    X3B, 
    HR
FROM 
	batting
WHERE 
	playerid = 'pujolal01' 
    AND yearid = 2006;

/* 
	Chapter 5: Evaluating Baseball Pitchers and Forecasting Future Performance

	The linear regression is again done in R instead of SQL. Let's predict how how future 
    ERA holds up using linear weights. 
    
	Warning: the code is nasty.

	It's not stated in the book how the author edge cases such as...

		1. How did he handle players that played in 2002 and 2004, but not 2003? 
		Did he just ignore them for prediction purposes?
		
        2. What what the threshold for IPouts before we factored them into the analysis? Or if 
        it was games instead of innings?

	So naturally my numbers disagree with his.
*/

CREATE TEMPORARY TABLE temp
	SELECT 
		playerID, 
		yearID, 
		SUM(ER) AS ER, 
		SUM(IPOuts) AS IPOuts, 
		(SUM(ER)/(SUM(IPOuts)/3))*9 AS ERA,
		2.8484 + .353*(SUM(ER)/(SUM(IPOuts)/3))*9 AS predicted_ERA
	FROM 
		pitching
	GROUP BY 
		playerID, yearID
	HAVING 
		yearID BETWEEN 2000 AND 2006;

CREATE TEMPORARY TABLE temp2
	SELECT 
		* 
	FROM 
		temp;

CREATE TEMPORARY TABLE temp3
	SELECT 
		t.*, 
		t2.predicted_ERA AS prior_predicted_ERA
	FROM 
		temp t
	LEFT JOIN 
		temp2 t2 ON t.yearID = t2.yearID + 1 
		AND t.playerID = t2.playerID;

SELECT *, 
	ABS(ERA - prior_predicted_ERA) AS absolute_error
FROM 
	temp3
ORDER BY 
	ABS(ERA - prior_predicted_ERA) DESC;

SELECT 
	AVG(ABS(ERA - prior_predicted_ERA)) 
FROM 
	temp3
WHERE 
	prior_predicted_ERA IS NOT NULL;
    
DROP TEMPORARY TABLE temp;
DROP TEMPORARY TABLE temp2;
DROP TEMPORARY TABLE temp3;

-- Finally, let's calculate DICE for pitchers.

SELECT 
	playerID, 
    yearID, 
	3 + (13*HR + 3*(BB + HBP) - 2*SO)/(IPOuts/3) AS DICE 
FROM 
	pitching
ORDER BY 
	DICE DESC;

/* 
	Chapter 6: Baseball Decision-Making

	First, we'll need the Run value computations for 2004. This isn't actually covered in the 
    book! Just the table is given without showing how it is calculated. This author's a total 
    bench for not explaining this.
    
	A couple of notes before we begin...

		1. This process is saved in the sproc merged.computeRunExpectancy, with the assisting
        sproc merged.vComputeStates and view merged.vGenerate states. Further comments can be 
        found in those sprocs about the process of making of building the Run Expectancy matrix.

		2. This is also calculated in Chapter 5 of Analyzing Baseball Data w/ R and most of the 
        following SQL code is an adaptation of the R script from that chapter. I recommend 
        reading through that chapter so that you can go at the pace you need.

		3. I use the notation of states from ABDwR instead of mathletics because I find it easier 
        to read. For example, you read "011 2" as 2 guys on 2nd and 3rd base with 2 outs.

 		4. The data pretty much agrees but isn't exact. I don't own a copy of "Baseball Hacks" 
        so I can't explore further why that is.
	
    First, we'll generate the Run Expectancy matrix.
*/

CALL merged.computeRunExpectancy(2004, 2004);

/* 
	I'll now show how to calculate the probabilities involved in table 6.3 and calculate
    expected value.
    
    First, I'll need to run merged.computeStates to get the state changes. Then I'll filter out
    to considering bunts only to calculate probability, and then join on the run expectancy
    matrix to compute expected value.
*/

CALL merged.computeStates(2004, 2004);

CREATE TEMPORARY TABLE stateTrackerBunt
	SELECT 
		*
	FROM 
		merged.stateTracker
	WHERE 
		(event_tx LIKE '%BG%' OR event_tx LIKE '%BP%')
        AND start_state = '100 0';
        
-- I see that 1010 records were created, so I use the 1010 to calculate prob.

CREATE TEMPORARY TABLE Probabilities
	SELECT 
		end_state, 
		round(count(*)/1010, 2) as prob
	FROM 
		stateTrackerBunt
	GROUP BY 
		end_state;

SELECT 
	sum(pr.prob*avg_runs_roi) AS expected_value
FROM 
	Probabilities pr
INNER JOIN
	merged.re24_list re on pr.end_state = re.start_state;

/*
	Probabilities and Expected Value generally agree; I get a run value that is .03 higher 
    than the author.

	Chapter 7: Evaluating Fielders

	Here is how to calculate the formula from the fielding table:
*/

SELECT 
	*,
	(PO + A)/(PO + A + E) AS fieldingPercentage 
FROM
	fielding;
    
/*
	Let's now move on to calculating average Range Factor.

	My numbers were wildly off this time. I get the feeling there is a criterion like "must 
    have 40 games played" in order to qualify for this stat that's not being explicitly stated 
    in the book. Once applying that criterion I get an answer that's within .06 of his. I'll 
    stick with his computation though.
*/

SELECT 
	AVG((PO + A)/(InnOuts/(8.9*3))) as RangeFactor
FROM 
	fielding
WHERE 
	yearID BETWEEN 2000 AND 2006
	AND (InnOuts/(8.9*3)) > 40;

-- Calculating Jeter and Furcal's Range Factor in particular for 2006

SELECT 
	playerID, ((PO + A)/(InnOuts/(8.9*3)))/4.483
FROM 
	fielding
WHERE 
	playerID IN ('jeterde01', 'furcara01')
	AND yearID = 2006;

/* 
	I'll put together the infrastructure necessary to convert Fielder's Scores into Runs and
    Wins, but will implement these sections later. Something I see as a roadblock to that 
    section is adequately assigning responsibility to each player. For example, in Game 5 of
    the 2020 World Series, how do we properly assign run value calculations to both Mookie 
    Betts' and Will Smith's massive blunders in the last play?
    
    Perhaps buying a copy of "The Fielding Bible" will clear that up.
    
    Chapter 8: Win Averages
    
    SAGWINDIFF required some bigly MySQL infrastructure to be made. The following are used to
    put this section together:
    
		merged.computeWinExpectancy (Stored Procedure)
		merged.computeWinExpectancySampleTable (Stored Procedure)
		merged.fixEndState (Function)
		merged.wonGame(Function)
		merged.winExpectancySample (Table)
		merged.vGenerateSAGWINDIFF (View)
    
	Do keep in mind that for games going longer than 10 innings or having a differential 
    larger than 10, calculations of SAGWINDIFF will return NULLS instead. Even at differentials
    of 8 or -8 there's not enough data to make a precise approximation of the probability.
    
    Since the example given is a playoff game, this unfortunately isn't stored in the server.
	However, here is a very similar game where Bryce Harper gives a walk-off Grand Slam in 2019:
*/

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
	AND (home_score_ct - away_score_ct + runs_scored) = wes2.differential
WHERE
	bat_id = 'harpb003'
    and year = 2019
    and game_end_fl = 1
    and away_team_id = 'CHN';

-- To get all of Bryce Harper's SAGWINDIFF points in the 2019 season:

SELECT
	SUM(IF(away_team_id = "PHI", -earnedSAGWINDIFF, earnedSAGWINDIFF))
FROM 
	merged.vGenerateSAGWINDIFF
WHERE
	bat_id = 'harpb003'
    and year = 2019;

-- To get all of a particular baseball team:

SELECT
	SUM(IF(away_team_id = "PHI", -earnedSAGWINDIFF, earnedSAGWINDIFF))
FROM
	merged.vGenerateSAGWINDIFF
WHERE
	YEAR = 2019
    AND concat(game_id, away_team_id) LIKE '%PHI%';
    
/* 
	Not exactly a great number for the Phillies...since 2000 SAGWINDIFF points equals 1 win,
	this would imply the Phillies would only win half their games. Which they did.
    
    Chapter 9: The Value of Replacement Players
    
    This section is unusually short due to the chapter being incredibly straightforward in
    comparison to the last chapter.
    
    To get the win-loss record of a team of all replacement players we would need to do 
    some monte carlo methods, so this is saved for the accompanying R file. We just show how
    to get the VORPP of Bryce Harper by recycling code from the last chapter:
*/

SELECT
	SUM(IF(away_team_id = "PHI", -earnedSAGWINDIFF, earnedSAGWINDIFF)) + 5.97*b.PA
FROM 
	merged.vGenerateSAGWINDIFF,
    (SELECT 
		(AB + BB + IBB + HBP + SH + SF) AS PA 
	FROM 
		batting
    WHERE 
		playerID = 'harpebr03'
		AND yearID = 2019) as b
WHERE
	bat_id = 'harpb003'
    and year = 2019;

-- (H + BB + HBP)*(H - X2B - X3B - HR + 2*X2B + 3*X3B + 4*HR)/(AB + BB + HBP) AS runs_created,