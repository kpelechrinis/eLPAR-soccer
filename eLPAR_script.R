### Libraries and functions needed

library("RSQLite")
library("nnet")
library("bbmle")
library("extraDistr")
library("verification")
library("RColorBrewer")
library("corrplot")
source("get_lines.R")
source("predict_skellam.R")



### We will load the data from the Kaggle sqlite database

con <- dbConnect(drv=RSQLite::SQLite(), dbname="Data/database-kaggle.sqlite")
tables <- dbListTables(con)
tables <- tables[tables != "sqlite_sequence"]
lDataFrames <- vector("list", length=length(tables))
for (i in seq(along=tables)) {
    lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}


# we do not have all the data for the starting lineups so we need to filter only the games for which we know the starting 11 for both teams

qr = "SELECT id FROM Match WHERE home_player_1 IS NOT NULL AND home_player_2 IS NOT NULL  AND home_player_3 IS NOT NULL  AND home_player_4 IS NOT NULL  AND home_player_5 IS NOT NULL  AND home_player_6 IS NOT NULL  AND home_player_7 IS NOT NULL  AND home_player_8 IS NOT NULL  AND home_player_9 IS NOT NULL AND home_player_10 IS NOT NULL  AND home_player_11 IS NOT NULL  AND away_player_1 IS NOT NULL AND away_player_2 IS NOT NULL  AND away_player_3 IS NOT NULL  AND away_player_4 IS NOT NULL  AND away_player_5 IS NOT NULL  AND away_player_6 IS NOT NULL  AND away_player_7 IS NOT NULL  AND away_player_8 IS NOT NULL  AND away_player_9 IS NOT NULL AND away_player_10 IS NOT NULL  AND away_player_11 IS NOT NULL"
indices <- dbGetQuery(conn=con, statement=qr)

# subsetting the part of the data/db tables that we are interested in

match <- lDataFrames[[3]]
match <- match[indices$id,]
pattr = lDataFrames[[5]]
playertable = lDataFrames[[4]]

# player positions
# this is a data frame where each player is a row and each column corresponds to the players and every column to all the positions. The primary position of a player is marked with 1, the secondary with 2 etc. Columns with 0s are positions that this players does not cover.

player_pos <- read.csv("Data/player_positions.csv",header=T)


# the following function call will return a data frame for all the games within the season in the argument

data.1516.full = get_lines(match, player_pos, season = "2015/2016")
data.1415.full = get_lines(match, player_pos, season = "2014/2015")
data.1314.full = get_lines(match, player_pos, season = "2013/2014")
data.1213.full = get_lines(match, player_pos, season = "2012/2013")
data.1112.full = get_lines(match, player_pos, season = "2011/2012")
data.1011.full = get_lines(match, player_pos, season = "2010/2011")
data.0910.full = get_lines(match, player_pos, season = "2009/2010")
data.0809.full = get_lines(match, player_pos, season = "2008/2009")

data.all.full = rbind(data.0809.full,data.0910.full,data.1011.full,data.1112.full,data.1213.full,data.1314.full,data.1415.full,data.1516.full)

## final data clean-up

data.all.full = data.all.full[-which(is.na(data.all.full$V1) | is.na(data.all.full$V2) | is.na(data.all.full$V3) | is.na(data.all.full$V4)),]


############### Skellam Regression Model ###############

### Train and evaluate the win probability model using Maximum Likelihood Estimation ####

train = sample(dim(data.all.full)[1],size=floor(0.8*dim(data.all.full)[1]))

mod_skellam_mle <- mle2(score~dskellam(mu1 = exp(l1), mu2  = exp(l2)),data = data.all.full[train,],parameters=list(l1~V1+V2+V3+V4,l2~V1+V2+V3+V4),start=list(l1=0,l2=0))


test = data.all.full[-train,]

## diff will keep track the difference between the actual score differential and the predicted one in the test set

diff <- c()

## results.calibration will keep track of the home win-tie-loss probability as well as the final result - 1:win, 0:tie, -1:loss

results.calibration = data.frame(V1=c(),V2=c(),V3=c(),V4=c())

for (i in 1:dim(test)[1]){
    
    m = predict_skellam(mod_skellam_mle,test[i,])
    d = m$mu1 - m$mu2
    diff[i] = test[i,]$score-d
    if (test[i,]$score==0){r = 0}
    if (test[i,]$score>0){r=1}
    if (test[i,]$score<0) {r = -1}
    results.calibration = rbind(results.calibration,data.frame(V1=sum(dskellam(c(1:10),m$mu1,m$mu2)),V2=sum(dskellam(0,m$mu1,m$mu2)),V3=sum(dskellam(c(-10:-1),m$mu1,m$mu2)),V4=r))
    
}

## chi-square test for checking normality of the difference between predicted score differential and actual score differential

h <- hist(diff) # this is also the histogram plot
theor <- rnorm(length(diff),0,sd(diff))
ht=hist(theor,breaks=h$breaks)
chisq.test(h$counts/length(diff),ht$counts/length(diff))

diff.df = data.frame(mids=ht$mids,density=ht$density)

ggplot(data=diff.df, aes(mids,density))+geom_col()+stat_function(fun = dnorm, n = 101, color="red",args = list(mean = 0, sd = sd(diff)))+labs(x="Score Prediction Error",y="Probability Density")+theme_bw(base_size=17)

## calibration plots

calibration.df = data.frame(obs = c(),pred=c(),result=c())

## calculate the calibration curve for wins (i.e., win = 1, loss/tie = 0)

homewins.calibration = results.calibration

homewins.calibration[which(homewins.calibration$V4==-1),]$V4 = 0

homewins <- verify(obs=homewins.calibration$V4,pred=homewins.calibration$V1,frcst.type = "prob", obs.type = "binary")
reliability.plot(homewins)

calibration.df <- rbind(calibration.df,data.frame(obs=homewins$obar.i,pred=homewins$y.i,result=rep("Win",10)))

# calculate the calibration curve for tie predictions (i.e., tie = 1, loss/win = 0)


tie.calibration = results.calibration

tie.calibration[which(tie.calibration$V4==1),]$V4=-1
tie.calibration$V4 = tie.calibration$V4 + 1

ties <- verify(obs=tie.calibration$V4,pred=tie.calibration$V2,frcst.type = "prob", obs.type = "binary")
reliability.plot(ties)

calibration.df <- rbind(calibration.df,data.frame(obs=homewins$obar,pred=ties$y.i,result=rep("Win",10)))


# calculate the calibration curve for loss predictions (i.e., loss = 1, draw/win = 0)

loss.calibration = results.calibration
loss.calibration[which(loss.calibration$V4==1),]$V4=0
loss.calibration[which(loss.calibration$V4==-1),]$V4=1

losses <- verify(obs=loss.calibration$V4,pred=loss.calibration$V3,frcst.type = "prob", obs.type = "binary")
reliability.plot(losses)

calibration.df <- rbind(calibration.df,data.frame(obs=losses$obar.i,pred=losses$y.i,result=rep("Loss",10)))

ggplot(calibration.df,aes(x=pred,y=obs,color=Result))+geom_line(size=2)+labs(x="Predicted Probability",y="Observed Probability")+theme_bw(base_size=17)+geom_abline(intercept=0,slope=1,color = "black",size=1.5)

## find the replacement levels for each line

rep.levels = replacement_levels(player_pos,pattr)

## for a given formation find the eLPAR for a given combination of player rating-player position

theoretical.451 = elpar_formation(4,5,1,rep.levels,mod_skellam_mle,50,99)
p451 <- ggplot(data = theoretical.451,aes(x=rating,y=elpar,group=position))+geom_point(aes(shape=position),size=5)+scale_colour_manual(values=c("blue", "red","black","green"))+ labs(y='Expected LPA per game (4-5-1)', x="FIFA Rating")+theme_bw(base_size=20)

theoretical.352 = elpar_formation(3,5,2,rep.levels,mod_skellam_mle,50,99)
p352 <- ggplot(data = theoretical.352,aes(x=rating,y=elpar,group=position))+geom_point(aes(shape=position),size=5)+scale_colour_manual(values=c("blue", "red","black","green"))+ labs(y='Expected LPA per game (3-5-2)', x="FIFA Rating")+theme_bw(base_size=20)

theoretical.433 = elpar_formation(4,3,3,rep.levels,mod_skellam_mle,50,99)
p433 <- ggplot(data = theoretical.433,aes(x=rating,y=elpar,group=position))+geom_point(aes(shape=position),size=5)+scale_colour_manual(values=c("blue", "red","black","green"))+ labs(y='Expected LPA per game (4-3-3)', x="FIFA Rating")+theme_bw(base_size=20)

theoretical.442 = elpar_formation(4,4,2,rep.levels,mod_skellam_mle,50,99)
p442 <- ggplot(data = theoretical.442,aes(x=rating,y=elpar,group=position))+geom_point(aes(shape=position),size=5)+scale_colour_manual(values=c("blue", "red","black","green"))+ labs(y='Expected LPA per game (4-5-1)', x="FIFA Rating")+theme_bw(base_size=20)


average.all = theoretical.352
average.all$elpar = (theoretical.352$elpar+theoretical.433$elpar+theoretical.442$elpar+theoretical.451$elpar)/4

pall <- ggplot(data = average.all,aes(x=rating,y=elpar,group=position))+geom_point(aes(shape=position),size=5)+scale_colour_manual(values=c("blue", "red","black","green"))+ labs(y='Expected LPA per game', x="FIFA Rating")+theme_bw(base_size=20)

############### Market Value Analysis ###############

## The following data frame contains the current age, market value (estimated amount of transfer fee to be paid for acquiring the player) and wage. Potential is the maximum projected rating for the player in the future

soccer_mv <- read.csv("Data/player_marketvalue.csv")

## add the position/line and the rating of the player

pos <- c()
rating = c()
for (i in 1:dim(soccer_mv)[1]){
    rating[i] = player_pos[i,]$rating
    for (j in 2:19){
        if (player_pos[i,j] == 1){
            ind = new.pos[[colnames(player_pos)[j]]]
            if (length(ind)>0){
                if (ind == 1){pos[i] = "D"}
                if (ind == 2){pos[i] = "M"}
                if (ind == 3){pos[i] = "O"}
                if (ind == 4){pos[i] = "GK"}
            }
        }
    }
}

soccer_mv$Position = pos

## calculate average differences between market values of players in different positions/lines

mat = matrix(0, nrow = 4, ncol = 4)
pval.mat = mat

soccer_mv.split = split(f=soccer_mv$pos,soccer_mv)

for (i in 1:4){
    for (j in 1:4){
        mat[i,j]= mean(soccer_mv.split[[i]]$MarketValue)-mean(soccer_mv.split[[j]]$MarketValue)
        pval.mat[i,j] = t.test(soccer_mv.split[[i]]$MarketValue,soccer_mv.split[[j]]$MarketValue)$p.val
    }
}

colnames(mat) = c(soccer_mv.split[[1]][1,]$pos,soccer_mv.split[[2]][1,]$pos,soccer_mv.split[[3]][1,]$pos,soccer_mv.split[[4]][1,]$pos)
rownames(mat) = colnames(mat)
corrplot(mat,is.corr = F,p.mat = pval.mat,method = "square",tl.srt=45,col=brewer.pal(n=8, name="PuOr"))


## Calculate cost per 1 expected league points as a function of total eLPAR of a player

## We first find for every player his elpar (using the average of the formations above)

elpar <- c()

for (i in 1:dim(soccer_mv)[1]){
    if (length(which(average.all$rating == soccer_mv[i,]$rating & average.all$position == soccer_mv[i,]$Position)) > 0){
        elpar[i] = average.all[which(average.all$rating == soccer_mv[i,]$rating & average.all$position == soccer_mv[i,]$Position),]$elpar
    }
}

soccer_mv$elpar = elpar

soccer_mv$cpp = soccer_mv$MarketValue/soccer_mv$elpar

# symbol for Euro
Euro <- "\u20AC"


p.cpp.elpar <- ggplot(data = soccer_mv[which(elpar>0),],aes(x=elpar,y=cpp,group=Position,linetype=Position))+geom_smooth(aes(color=Position),method='loess',size=2)+theme_bw(base_size=20)+labs(x="eLPAR",y=paste0(Euro,"(M)/League Point"))


## Calculate cost per 1 expected league points as a function of a player's rating

p.cpp.rtg <- ggplot(data = soccer_mv[which(elpar>0),],aes(x=rating,y=cpp,group=Position,linetype=Position))+geom_smooth(aes(color=Position),method='loess',size=2)+theme_bw(base_size=20)+labs(x="FIFA Rating",y=paste0(Euro,"(M)/League Point"))


#### Reallocate budget based on player's elpar
## We will calculate the elpar for every starting player based on the average of the above formation as well as the default formation of the team
## We then use this elpar to redistribute the wages of the players based on the fraction of elpar (out of the total elpar of the team) that each one produces

## Barcelona

fbc <- read.csv("Data/FCBarcelona.csv",sep="\t",header=T)

# Barcelona's default formation is 4-4-2
elpar442 <- c()
elpar <- c()

for (i in 1:11){
    elpar442[i] = theoretical.442[which(theoretical.442$rating==fbc[i,]$Rating & theoretical.442$position==fbc[i,]$Position),]$elpar
    elpar[i] = average.all[which(average.all$rating==fbc[i,]$Rating & average.all$position==fbc[i,]$Position),]$elpar
}

elpar442.fr = elpar442/sum(elpar442)
salary.proj.442 = elpar442.fr*sum(fbc$Wage)
elpar.fr = elpar/sum(elpar)
salary.proj = elpar.fr*sum(fbc$Wage)

fbc.df <- as.data.frame(fbc)
fbc.df$Type = rep("Average",11)
fbc.df$eLPARWage = salary.proj
fbc1.df <- fbc.df
fbc1.df$Type = rep("4-4-2",11)
fbc1.df$eLPARWage = salary.proj.442
fbc.df <- rbind(fbc.df,fbc1.df)
fbc.df$Wage = fbc.df$Wage/1000
fbc.df$eLPARWage = fbc.df$eLPARWage/1000
pbarca = ggplot(fbc.df,aes(x=Wage,y=eLPARWage,group=Type))+geom_point(aes(shape=Position,color=Type,size=Rating))+labs(x=paste0("Wage ",Euro," (K)"),y=paste0("eLPAR Wage ",Euro," (K) (projected)"))+geom_abline(intercept=0,slope=1,color="blue",size=1.5)+theme_bw(base_size=20)+xlim(c(0,600))+ylim(c(0,600))

## Man United

manu <- read.csv("Data/ManUnited.csv",sep="\t",header=T)

# Manchester United's default formation is 4-3-3
elpar433 <- c()
elpar <- c()

for (i in 1:11){
    elpar433[i] = theoretical.433[which(theoretical.433$rating==manu[i,]$Rating & theoretical.433$position==manu[i,]$Position),]$elpar
    elpar[i] = average.all[which(average.all$rating==manu[i,]$Rating & average.all$position==manu[i,]$Position),]$elpar
}

elpar433.fr = elpar433/sum(elpar433)
salary.proj.433 = elpar433.fr*sum(manu$Wage)
elpar.fr = elpar/sum(elpar)
salary.proj = elpar.fr*sum(manu$Wage)

manu.df <- as.data.frame(manu)

manu.df$Type = rep("Average",11)
manu.df$eLPARWage = salary.proj
manu1.df <- manu.df
manu1.df$Type = rep("4-4-2",11)
manu1.df$eLPARWage = salary.proj.442

manu.df <- rbind(manu.df,manu1.df)
manu.df$Wage = manu.df$Wage/1000
manu.df$eLPARWage = manu.df$eLPARWage/1000

pmanu = ggplot(manu.df,aes(x=Wage,y=eLPARWage,group=Type))+geom_point(aes(shape=Position,color=Type,size=Rating))+labs(x=paste0("Wage ",Euro," (K)"),y=paste0("eLPAR Wage",Euro," (K) (projected)"))+geom_abline(intercept=0,slope=1,color="blue",size=1.5)+theme_bw(base_size=20)+xlim(c(0,400))+ylim(c(0,400))

### League point value per 1 Euro in Premier League 2017-18

premier.league <- read.csv("Data/PremierLeague-201718.csv")
mod.budget <- lm(Points~Budget,data=premier.league)

epl <- ggplot(premier.league,aes(x=Budget,y=Points))+geom_smooth(method="lm")+geom_point()+labs(x=paste0("Total Transfer Budget",Euro," (M)"),y="Total League Points")+theme_bw(base_size=20)
