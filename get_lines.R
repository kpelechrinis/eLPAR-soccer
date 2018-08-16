get_lines <- function(match,player_pos,season){
    
    match.dat <- match[which(match$season == season),]
    
    # we will add a column with the player's rating in player_pos
    # We are using the latest rating for that season that we have for a player -- We examined other approaches like average rating etc. and had no visible effect (ratings change slowly over a season).
    
    rtg = c()
    
    min.date = min(match.dat$date)
    max.date = max(match.dat$date)
    
    for (i in 1:dim(player_pos)[1]){
        xk = pattr[which(pattr$player_fifa_api_id ==player_pos[i,1]),]
        ind = which(xk$date > min.date & xk$date < max.date)
        if (length(ind) > 0){
            rtg[i] = xk[ind,]$overall_rating[length(ind)]
        }else{
            rtg[i] = xk[1,]$overall_rating
        }
    }
    
    player_pos$rtg = rtg
    
    # We load the grouping of positions to the four lines: attack/forwards, middlefielders, defenders and goal keeper
    source("pos_grouping.R")
    
    # This matrix data.mat will include for every match:
    # Columns 1-4: Average rating for home team lines (defense, middlefield, attack, goalkeeper)
    # Columns 5-8: Average rating for away team lines (defense, middlefield, attack, goalkeeper)
    # Column 9: Goal differential (home-away)
    # Column 10: Goals scored by home
    # Column 11: Goals scored by away
    
    data.mat <- matrix(0,nrow = dim(match.dat)[1],ncol = 11)
    
    for (i in 1:dim(match.dat)[1]){
        # Home team
        # we keep track of the number of players at each of the team's lines and their average rating
        # in some cases, the lineup/player position data lead to 0 players in one line and hence, when we calculate the average rating of a line we will get 0/0. To avoid this we initialize the number of players in a line with a small epsilon. We will later remove these games from our data (happens to a very small fraction of the games)
        homeplayers.n = rep(0.000001,4)
        homeplayers.rtg = rep(0,4)
        # The home team player IDs at the original match data are in the columns 56-66 of the match.dat table
        # We identify the fifa_id of the player and then obtain the rating of this player
        # Finally we find the main position of the player from the player_pos matrix (i.e., we find the column that equals to 1)
        for (ph in 56:66){
            fifa_id = playertable[which(playertable$player_api_id == match.dat[i,ph]),]$player_fifa_api_id
            p = which(player_pos$playerID == fifa_id)
            p.dat <- player_pos[p,]
            rtg = player_pos[which(player_pos$playerID == fifa_id),]$rtg
            for (j in 2:19){
                if (p.dat[j] ==1){
                        ind = new.pos[[colnames(player_pos)[j]]]
                        homeplayers.n[ind] = homeplayers.n[ind] + 1
                        homeplayers.rtg[ind] = homeplayers.rtg[ind] + rtg
                        break
                }
            }
        }
        homeplayers.rtg = homeplayers.rtg/homeplayers.n
        
        # Away team (similar as above)
        awayplayers.n = rep(0.000001,4)
        awayplayers.rtg = rep(0,4)
        for (pa in 67:77){
            fifa_id = playertable[which(playertable$player_api_id == match.dat[i,pa]),]$player_fifa_api_id
            p = which(player_pos$playerID == fifa_id)
            p.dat <- player_pos[p,]
            rtg = player_pos[which(player_pos$playerID == fifa_id),]$rtg
            for (j in 2:19){
                if (p.dat[j] == 1){
                    ind = new.pos[[colnames(player_pos)[j]]]
                    awayplayers.n[ind] = awayplayers.n[ind] + 1
                    awayplayers.rtg[ind] = awayplayers.rtg[ind] + rtg
                    break
                }
            }
        }
        awayplayers.rtg = awayplayers.rtg/awayplayers.n
        #score home-away
        score = match.dat[i,]$home_team_goal -  match.dat[i,]$away_team_goal
        home = match.dat[i,]$home_team_goal
        away = match.dat[i,]$away_team_goal
        tmp1 = append(homeplayers.rtg,awayplayers.rtg)
        tmp = append(tmp1,score)
        tmp = append(tmp,home)
        tmp = append(tmp,away)
        data.mat[i,]=tmp
    }
    
    # Create a data frame with the independent variables of the Skellam regression model
    
    data.df = as.data.frame(data.mat)
    
    data.all=data.df[,c(1:4)]-data.df[,c(5:8)]
    data.all$score = data.df$V9
    data.all$hg = data.df$V10
    data.all$ag = data.df$V11
    

    # Remove data points that have the problem mentioned above with lines with 0 players found (and correspondingly 0 average rating for the line)

    data.all = data.all[-(which(data.df$V1==0 | data.df$V2==0 | data.df$V5==0 | data.df$V6==0| data.df$V7==0 | data.df$V3 ==0 | data.df$V4 == 0 | data.df$V8==0)),]

    data.all

    
}
