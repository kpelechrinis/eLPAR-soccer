replacement_levels <- function(player_pos,pattr,min.date="2015-07-17 00:00:00",max.date="2016-05-25 00:00:00"){
    
    ## player_pos is a data frame where each player is a row and each column corresponds to the players and every column to all the positions. The primary position of a player is marked with 1, the secondary with 2 etc. Columns with 0s are positions that this players does not cover.
    ## pattr is the player_attributes table of the original data
    ## min.date and max.date are used to limit the search of the replacement level
    ## The replacement level is defined as 80% of the average rating for each position
    
    
    rtg <- c()
    
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
    
    source("pos_grouping.R")
    
    off <- c()
    def <- c()
    center <- c()
    gk <- c()
    
    for (i in 1:dim(player_pos)[1]){
        for (j in 2:19){
            if (player_pos[i,j] == 1){
                ind = new.pos[[colnames(player_pos)[j]]]
                if (length(ind)>0){
                    if (ind == 1){def = append(def,player_pos[i,]$rtg)}
                    if (ind == 2){center = append(center,player_pos[i,]$rtg)}
                    if (ind == 3){off = append(off,player_pos[i,]$rtg)}
                    if (ind == 4){gk = append(gk,player_pos[i,]$rtg)}
                }
            }
        }
    }
    replacement_levels = data.frame(offense=0.8*mean(off,na.rm=T),defense=0.8*mean(def,na.rm=T),middlefield=0.8*mean(center,na.rm=T),gk=0.8*mean(gk,na.rm=T))
    replacement_levels
    
}