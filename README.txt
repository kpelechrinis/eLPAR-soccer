This repository includes all the code and data used for developing the expected league points above replacement for soccer as described in "Positional Value in Soccer: Expected League Points Added above Replacement".

The sub-directory named "Data" has all of the data needed: 

1. database-kaggle.sqlite.tar.gz: this is the original data from Kaggle (Obtained from https://www.kaggle.com/hugomathien/soccer)
2. player_positions.csv: this is a file that includes for every player in the original dataset his positions. Every row of this file is a player and there are 19 columns, one for every possible position. The main position of a player is marked with 1, while the secondary with 2 etc. Columns with 0 represent positions that the player does not play (obtained from https://sofifa.com/)
3. player_marketvalue.csv: this is a file that includes the current (2017-18 season) market values and wages of the players (obtained from https://sofifa.com/)
4. soccer-elpar.RDa: this is an R workspace file that includes all the processed structures needed for building the win probability model. This is provided in order to not have to run the pre-processing part of the code (see eLPAR_script.R)
5. FCBarcelona.csv: includes the starting 11 for FC Barcelona for the 2017-18 season, along with their rating, market value and wage (obtained from https://sofifa.com/)
6. ManUnited.csv: same as above but for Manchester United
7. PremierLeague-201718.csv: this file includes the final standings along with budget information for the 2017-18 Premier League (obtained from https://fbref.com)

The code includes the following files: 

1. eLPAR_script.R: This is the main script that you need to replicate the analysis, results and produce the figures in the paper. The following are helper functions to make the code more readable. The code is also documented ot 
2. pos_grouping.R: This script simply defines a hash-table that maps the micro-positions to the 4 lines used in the analysis (attack, middlefield, defense and goal-keeping)
3. get_lines.R: This script processes the game data and produces the independent variables (i.e., the average rating differences between the two teams lines) for the Skellam regression model
4. predict_skellam.R: This is a script that gives predictions for the Skellam model built
5. replacement_levels.R: This script calculates the replacement level for each line
6. elpar_formation.R: This script calculates the eLPAR for a player with a given rating and position, based on the input formation.

