# Datasets from 5 past seasons have been used
# Last update for season 19/20 dataset: 10/26/2019
set.seed(123)
library("ggplot2")
library("rlist")
source("cleaning.R")

setwd("/Users/Sara/RStudioProjects/project_data_prep/DataPrep")

# Download datasets
df.15.16 <- data.frame(read.csv("premier-15-16.csv"))
df.16.17 <- data.frame(read.csv("premier-16-17.csv"))
df.17.18 <- data.frame(read.csv("premier-17-18.csv"))
df.18.19 <- data.frame(read.csv("premier-18-19.csv"))
df.19.20 <- data.frame(read.csv("premier-19-20.csv"))

# Clean data
df.15.16=clean(df.15.16)
df.16.17=clean(df.16.17)
df.17.18=clean(df.17.18)
df.18.19=clean(df.18.19)
df.19.20=clean(df.19.20)

# joint all datasets
df.past <- rbind(df.15.16, df.16.17, df.17.18, df.18.19)
str(df.past)

# Select only information of Arsenal
df.18.19_Arsenal = team(df.18.19,"Arsenal")
# Home matches
df.18.19_Arsenalhome = homeTeam(df.18.19_Arsenal,"Arsenal")
# Away matches
df.18.19_Arsenalaway = awayTeam(df.18.19_Arsenal,"Arsenal")


# EDA 1: boxplots of home and away of all the teams of 18-19 league:
i=6
teams = c("Man United","Bournemouth","Fulham","Huddersfield","Newcastle","Watford ","Wolves","Arsenal","Liverpool","Southampton","Cardiff","Chelsea","Everton","Leicester","Tottenham","West Ham","Brighton","Burnley","Man City","Crystal Palace")
df.18.19_subteam = team(df.18.19, teams[i])
p <- ggplot(df.18.19_subteam, aes(df.18.19_subteam$Location, df.18.19_subteam$FTHG)) + geom_boxplot(aes(fill = df.18.19_subteam$Location)) + labs(title=teams[i],x="Location", y = "Full Time Goals") + theme(legend.position = "none")
ggsave(p,filename=paste("home_away_18.19",teams[i],".png"))
p

#for (val in teams) {
#  print(val)
#  df.18.19_subteam = team(df.18.19, val)
#  p <- ggplot(df.18.19_subteam, aes(df.18.19_subteam$Location, df.18.19_subteam$FTHG))
#  p + geom_boxplot(aes(fill = df.18.19_subteam$Location)) + labs(title=val,x="Location", y = "Full Time Goals")
#  ggsave(p,filename=paste("myplot",teams[val],".png",sep=""))
#}

# EDA 2: percentage of home wins, away wins, and draws by season: 

homeWins<-c(sum(s0910$FTR=='H')/380,sum(s1011$FTR=='H')/380,sum(s1112$FTR=='H')/380,sum(s1213$FTR=='H')/380,sum(s1314$FTR=='H')/380,
            sum(s1415$FTR=='H')/380,sum(s1516$FTR=='H')/380,sum(s1617$FTR=='H')/380,sum(s1718$FTR=='H')/380,sum(s1819$FTR=='H')/380)
homeWins

awayWins<-c(sum(s0910$FTR=='A')/380,sum(s1011$FTR=='A')/380,sum(s1112$FTR=='A')/380,sum(s1213$FTR=='A')/380,sum(s1314$FTR=='A')/380,
            sum(s1415$FTR=='A')/380,sum(s1516$FTR=='A')/380,sum(s1617$FTR=='A')/380,sum(s1718$FTR=='A')/380,sum(s1819$FTR=='A')/380)
awayWins


draws<-c(sum(s0910$FTR=='D')/380,sum(s1011$FTR=='D')/380,sum(s1112$FTR=='D')/380,sum(s1213$FTR=='D')/380,sum(s1314$FTR=='D')/380,
         sum(s1415$FTR=='D')/380,sum(s1516$FTR=='D')/380,sum(s1617$FTR=='D')/380,sum(s1718$FTR=='D')/380,sum(s1819$FTR=='D')/380)
draws

#check that they sum to 1
homeWins+awayWins+draws


season<-c('2009-10','2010-11','2011-12','2012-13','2013-14','2014-15','2015-16','2016-17','2017-18','2018-19')

hadPercentages<-data.frame(season,homeWins,awayWins,draws)
wldPercentages

save(hadPercentages,file = '~/school/CSP 571/Project/EngPrimLg/had.RData')

ggplot(hadPercentages, aes(season, color = variable)) + 
  geom_line(aes(y = homeWins, color = "home wins", group = "home wins")) +
  geom_line(aes(y = awayWins, color = "away wins", group = "away wins")) +
  geom_line(aes(y = draws, color = "draws", group = "draws")) +
  geom_point(aes(y = homeWins, col = "home wins")) + 
  geom_point(aes(y = awayWins, col = "away wins")) +
  geom_point(aes(y = draws, col = "draws"))

# EDA 3: Correlation plot between shots, goals, yellow cards and red cards

"df.18.19_subset <- df.18.19
df.18.19_subset$Div <- NULL
df.18.19_subset$dd <- NULL
df.18.19_subset$mm <- NULL
df.18.19_subset$yy <- NULL
df.18.19_subset$HomeTeam <- NULL
df.18.19_subset$AwayTeam <- NULL
df.18.19_subset$HTR <- NULL
df.18.19_subset$FTR <- NULL
df.18.19_subset$Referee <- NULL"
HomeYellow=df.18.19$HY
AwayYellow=df.18.19$AY
HomeRed=df.18.19$HR
AwayRed=df.18.19$AR
FullTimeHomeGoals=df.18.19$FTHG
FullTimeAwayGoals=df.18.19$FTAG
HalfTimeHomeGoals=df.18.19$HTHG
HalfTimeAwayGoals=df.18.19$HTAG

df.18.19_subset <- data.frame(HomeYellow,AwayYellow,HomeRed,AwayRed,FullTimeHomeGoals,FullTimeAwayGoals,HalfTimeHomeGoals, HalfTimeAwayGoals)


library("PerformanceAnalytics")
chart.Correlation(df.18.19_subset, histogram=TRUE)

# EDA 4: Evolution of correlation between shots and goals over the years for a good team and a bad teams. (Arsenal, Southampton)

df.15.16_Arsenal = team(df.15.16,"Arsenal")
df.16.17_Arsenal = team(df.16.17,"Arsenal")
df.17.18_Arsenal = team(df.17.18,"Arsenal")
df.18.19_Arsenal = team(df.18.19,"Arsenal")
df.19.20_Arsenal = team(df.19.20,"Arsenal")

df.15.16_Southampton = team(df.15.16,"Southampton")
df.16.17_Southampton = team(df.16.17,"Southampton")
df.17.18_Southampton = team(df.17.18,"Southampton")
df.18.19_Southampton = team(df.18.19,"Southampton")
df.19.20_Southampton = team(df.19.20,"Southampton")

df.15.16_Arsenal <- df.15.16_Arsenal[, c(28,29)]
df.16.17_Arsenal <- df.16.17_Arsenal[, c(28,29)]
df.17.18_Arsenal <- df.17.18_Arsenal[, c(28,29)]
df.18.19_Arsenal <- df.18.19_Arsenal[, c(28,29)]
df.19.20_Arsenal <- df.19.20_Arsenal[, c(28,29)]

df.15.16_Southampton = df.15.16_Southampton[, c(28,29)]
df.16.17_Southampton = df.16.17_Southampton[, c(28,29)]
df.17.18_Southampton = df.17.18_Southampton[, c(28,29)]
df.18.19_Southampton = df.18.19_Southampton[, c(28,29)]
df.19.20_Southampton = df.19.20_Southampton[, c(28,29)]

source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")

par(mfrow=c(2,5)) 
df.15.16_Arsenal_cor=rquery.cormat(df.15.16_Arsenal)
df.16.17_Arsenal_cor=rquery.cormat(df.16.17_Arsenal)
df.17.18_Arsenal_cor=rquery.cormat(df.17.18_Arsenal)
df.18.19_Arsenal_cor=rquery.cormat(df.18.19_Arsenal)
df.19.20_Arsenal_cor=rquery.cormat(df.19.20_Arsenal)
df.15.16_Southampton_cor=rquery.cormat(df.15.16_Southampton)
df.16.17_Southampton_cor=rquery.cormat(df.16.17_Southampton)
df.17.18_Southampton_cor=rquery.cormat(df.17.18_Southampton)
df.18.19_Southampton_cor=rquery.cormat(df.18.19_Southampton)
df.19.20_Southampton_cor=rquery.cormat(df.19.20_Southampton)


library(gridExtra)
library(grid)
library(lattice)
p <- xyplot(df.15.16_Arsenal$Team.goals~df.15.16_Arsenal$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2015-2016")
p2 <- xyplot(df.16.17_Arsenal$Team.goals~df.16.17_Arsenal$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2016-2017")
p3 <- xyplot(df.17.18_Arsenal$Team.goals~df.17.18_Arsenal$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2017-2018")
p4 <- xyplot(df.18.19_Arsenal$Team.goals~df.18.19_Arsenal$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2018-2019")
p5 <- xyplot(df.19.20_Arsenal$Team.goals~df.19.20_Arsenal$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2019-2020")
a <- xyplot(df.15.16_Southampton$Team.goals~df.15.16_Southampton$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2015-2016")
a2 <- xyplot(df.16.17_Southampton$Team.goals~df.16.17_Southampton$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2016-2017")
a3 <- xyplot(df.17.18_Southampton$Team.goals~df.17.18_Southampton$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2017-2018")
a4 <- xyplot(df.18.19_Southampton$Team.goals~df.18.19_Southampton$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2018-2019")
a5 <- xyplot(df.19.20_Southampton$Team.goals~df.19.20_Southampton$Team.shots.on.target, xlab="Shots",ylab="Goals", main=" 2019-2020")

grid.arrange(p, p2,p3,p4,p5,a,a2,a3,a4,a5, ncol=5)
 


