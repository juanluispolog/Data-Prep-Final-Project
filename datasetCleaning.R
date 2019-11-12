# This file cointains the coded used for cleaning and homogeneizing the datasets

rm(list=ls())

setwd("/Users/eduardo/Desktop/Data Prep/Project/Data_Cleaning")

df.15.16 <- read.csv("premier-15-16.csv")
df.16.17 <- read.csv("premier-16-17.csv")
df.17.18 <- read.csv("premier-17-18.csv")
df.18.19 <- read.csv("premier-18-19.csv")
df.19.20 <- read.csv("premier-19-20.csv")

str(df.15.16)

# Removing betting attributes from datasets
df.15.16 <- df.15.16[, - c(which(colnames(df.15.16) == "B365H"):ncol(df.15.16))]
df.16.17 <- df.16.17[, - c(which(colnames(df.16.17) == "B365H"):ncol(df.16.17))]
df.17.18 <- df.17.18[, - c(which(colnames(df.17.18) == "B365H"):ncol(df.17.18))]
df.18.19 <- df.18.19[, - c(which(colnames(df.18.19) == "B365H"):ncol(df.18.19))]
df.19.20 <- df.19.20[, - c(which(colnames(df.19.20) == "B365H"):ncol(df.19.20))]

str(df.15.16)
str(df.16.17)
str(df.17.18)
str(df.18.19)
str(df.18.19)

# Removing Time attribute from df.19.20 season, since other df do not store this information
df.19.20 <- df.19.20[, - which(colnames(df.19.20) == "Time")]

# Homogeneizing Date attribute: yyyy/mm/dd
df.list <- list(df.15.16, df.16.17, df.17.18, df.18.19, df.19.20)

for (i in 1:length(df.list)) {
  date <- as.Date(df.list[[i]]$Date, format = "%d/%m/%Y")
  date.df <- data.frame(dd = as.numeric(format(date, format = "%d")),
                        mm= as.numeric(format(date, format = "%m")), 
                        yyyy = as.numeric(format(date, format = "%y"))
                        )
  
  df.list[[i]] <- data.frame(df.list[[i]]$Div, date.df, df.list[[i]][, c(3:23)])
}

df.15.16 <- df.list[[1]]
df.16.17 <- df.list[[2]]
df.17.18 <- df.list[[3]]
df.18.19 <- df.list[[4]]
df.19.20 <- df.list[[5]]


# Merging datasets

df <- rbind(df.15.16, df.16.17, df.17.18, df.18.19, df.19.20)
str(df)

################################################################################################
################################################################################################
################################### Exploring data ############################################
################################################################################################
################################################################################################


################################################################################################
########################################### Values ###########################################
################################################################################################

######################################### Nan and null #########################################
dim(df)
class(df)
str(df)

which(is.na(df))
which(is.null(df))
#No Nans and nor null values

################################### Exploring dates #######################################
### Days ###
unique(df$dd) 
#Most of the days of a month are covered

### Months ###
unique(df$mm) 
#June and July have no games

### Years ###
unique(df$yyyy)
#Years go from 2015 to 2019 (included)

################################### Exploring teams #######################################
### Home Teams ###
home.teams <- unique(df$HomeTeam)
away.teams <- unique(df$AwayTeam)
teams <- data.frame("Home"=home.teams,"Away"=away.teams)

which((teams[order(teams$Away),2] == teams[order(teams$Home),1])== FALSE)
#Both columns matches identically and hence, same teams for Home and Away
#(same teams playing during the entire years)

############################# Exploring results of games #######################################
### Full time ###
home.goals <- df$FTHG
away.goals <- df$FTAG
goals <- data.frame("HomeGoals"=home.goals,"AwayGoals"=away.goals)
head(goals)

result <- rep("a",dim(df)[1])
cbind(goals,result)

goals[goals$HomeGoals>goals$AwayGoals, "result"] = "H"
goals[goals$HomeGoals<goals$AwayGoals, "result"] = "A"
goals[goals$HomeGoals==goals$AwayGoals, "result"] = "D"

unique(goals$result)
#Now the column result (created manually must match with feature FTR)
real.result <- df$FTR
all(real.result==goals$result)  #Another way of checking if two columns are equal

### Half time ###
home.goals.half <- df$HTHG
away.goals.half <- df$HTAG
goals.half <- data.frame("HomeGoals"=home.goals.half,"AwayGoals"=away.goals.half)
head(goals.half)

result.half <- rep("a",dim(df)[1])
cbind(goals.half,result.half)

goals.half[goals.half$HomeGoals>goals.half$AwayGoals, "result.half"] = "H"
goals.half[goals.half$HomeGoals<goals.half$AwayGoals, "result.half"] = "A"
goals.half[goals.half$HomeGoals==goals.half$AwayGoals, "result.half"] = "D"

unique(goals.half$result.half)
#Now the column result (created manually must match with feature FTR)
real.result.half <- df$HTR
all(real.result.half==goals.half$result.half)

###################################### Exploring shots #######################################
### Home shots ###                                                      FIRST WRONG VALUE
home.shots <- df$HS
home.shots.target <- df$HST
all(home.shots.target <= home.shots)
#It looks that there is a wrong value

home.shots[(home.shots.target <= home.shots) == FALSE] 
home.shots.target[(home.shots.target <= home.shots) == FALSE]
#Therefore we found that this value is wrong and it will be studied how to deal with it
#It is not possible having a number of shots and a higher number of shots in a specific position

df[(home.shots.target <= home.shots) == FALSE,] 
#Despite this error, the row makes sense in the rest of features, it is possible to change 
#one of the values that are wrong.
df[(home.shots.target <= home.shots) == FALSE,]$HST = 2 #The value has been changed

### Away shots ###
away.shots <- df$AS
away.shots.target <- df$AST
all(away.shots.target <= away.shots) 
#The shots of away teams makes sense

############################# Exploring rate of shots in games ############################

total.shots <- home.shots + away.shots
normalized.shots <- data.frame("Home shots ratio"=home.shots/total.shots,
                      "Away shots ratio"=away.shots/total.shots)

sum(normalized.shots$Home.shots.ratio > normalized.shots$Away.shots.ratio)
sum(normalized.shots$Home.shots.ratio < normalized.shots$Away.shots.ratio)

data.frame("Home Wins"= sum(normalized.shots$Home.shots.ratio > normalized.shots$Away.shots.ratio),
      "Draw"= sum(normalized.shots$Home.shots.ratio == normalized.shots$Away.shots.ratio),
      "Away Wins"= sum(normalized.shots$Home.shots.ratio < normalized.shots$Away.shots.ratio))

#Idem that saying what is more common, the home team winning or away team winning
#It is concluded that it is more common to win at home (score more goals at home)

######################################## Referee #########################################

referee <- df$Referee
unique(referee)

### Yellow Cards ###
home.yellow <- df$HY
away.yellow <- df$AY
total.yellow <- home.yellow + away.yellow

yellow.cards <- data.frame(home.yellow, away.yellow, total.yellow)

#With referee
yellow.cards <- cbind(yellow.cards, referee)

#HOME YELLOW
plot(referee, yellow.cards$home.yellow)
#AWAY YELLOW
plot(referee, yellow.cards$away.yellow)
#TOTAL YELLOW
plot(referee, yellow.cards$total.yellow)

############ ??????????????????????????????????????? NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
#Value chosen as high number of yellow cards is 7
yellow.cards.dirty <- yellow.cards[which(yellow.cards$total.yellow>=7),]
max(summary(yellow.cards.dirty$referee))
#Therefore the referee with highest number of yellow cards in a match is M Dean


#Value chosen as low number of yellow cards is 0
yellow.cards.fair <- yellow.cards[which(yellow.cards$total.yellow==0),]
summary(yellow.cards.fair$referee)
max(summary(yellow.cards.fair$referee))
#Therefore the referee with highest number of yellow cards in a match is M Dean
############ ??????????????????????????????????????? NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


home.red <- df$HR
away.red <- df$AR

total.red <- home.red + away.red

red.cards <- data.frame(home.red, away.red, total.red)

#With referee
red.cards <- cbind(red.cards, referee)

#HOME RED
plot(referee, red.cards$home.red)
#AWAY RED
plot(referee, red.cards$away.red)
#TOTAL RED
plot(referee, red.cards$total.red)


################################################################################################
################################### Visualizing data ##########################################
################################################################################################


################################################################################################
################################### Correlations ###############################################
################################################################################################

#VAMOS A ANALIZAR MODELOS POR TEMPORADA Y ASIGNAR PESOS SEGUN EL TIEMPO

################################################################################################
################################################################################################
############################# Exploring data by TEAMS ######################################
################################################################################################
################################################################################################

unique(df$HomeTeam)

unique(df.15.16$HomeTeam)
unique(df.16.17$HomeTeam)
unique(df.17.18$HomeTeam)
unique(df.18.19$HomeTeam)


################################################################################################
#########################################     HOME     #######################################
################################################################################################

df.wat.home <- df[which(df$HomeTeam=='Watford'),]
df.wat.home[, c('AwayTeam','FTR','Referee')]

#HOW MANY TIMES THE TEAM WIN/LOSE WITH A SPECIFIC REFEREE
plot(df.wat.home$Referee, df.wat.home$FTR)

#Games lost at home 
df.wat.home.win  <- df.wat.home[which(df.wat.home[, c('AwayTeam','FTR','Referee')]$FTR=='H'),]
df.wat.home.lost <- df.wat.home[which(df.wat.home[, c('AwayTeam','FTR','Referee')]$FTR=='A'),]
df.wat.home.draw <- df.wat.home[which(df.wat.home[, c('AwayTeam','FTR','Referee')]$FTR=='D'),]

#STUDY NUMBER OF YELLOW/RED CARDS PLAYING HOME
plot(df.wat.home$AwayTeam, df.wat.home$HY) #Depending on the away team (STUDY)
plot(df.wat.home$Referee, df.wat.home$HY)  #Depending on the referee (INTERESTING SOME NULL YELLOW)
plot(df.wat.home$HTR, df.wat.home$HY)  #Depending on the half time result (INTERST SAME MEDIAN)

# CREATE A FUNCITION WHERE YOU CALL EACH TEAM AND GOT THE RESULTS####### TRABAJAR EN ESTO!!!!!

cat("In order to get the functions, the posible functions are:",
    "\n", "'yellow': get the box plots of each team with the yellow cards received",
    "\n", "'red': get the box plots of each team with the red cards received")

cat("\n","Choose an option to plot depending on:",
"\n","the other team type second parameter as 'away'",
"\n","the referee type second parameter 'referee'",
"\n","the half time result type second parameter as 'HTR'")

yellow <- function(x,y){
  
  df.yellow <- df[which(df$HomeTeam==x),]
  if(y=='away'){ p <- plot(df.yellow$AwayTeam, df.yellow$HY) }
  return(p)
}

edu <- yellow('Watford','away')

################################################################################################
#########################################     AWAY     #######################################
################################################################################################

df.wat.away <- df[which(df$AwayTeam=='Watford'),]


################################################################################################
################################################################################################
################################ Poisson Regression ######################################
################################################################################################
################################################################################################

################################################################################################
################################     TIME SERIES DATA     ######################################
###############################################################################################

df.15.16$mm
df.15.16$dd

tail(df.15.16)

watford.edu <- df.1[which(df.15.16$HomeTeam=='Watford'),]
head(chelsea.edu)

plot(watford.edu$FTHG)
unique(df$HomeTeam)

library("tidyr")
library("dplyr")
edu_test <- unite(df.16.17,col="Date",c("dd","mm","yyyy"),sep="/",remove=TRUE)
head(edu_test)

library("ggplot2")
ggplot(edu_test, aes(x=Date)) + 
  geom_line(aes(y=FTHG))

##########################################################################################
# Dado un arbitro hay mas amarillas a partir d eun valor si es true, y se cuentan los trues.
#, el equipo mete mas (suma total 
#de goles home y away es mayor que un valor) 
#predict lm de aebitro con home goals etc
#El equipo home suele ganar o perder


#CREAR UN MODELO DE REGRESION LINEAL PARA PREDECIR ALGO A PARTIR DE UN ARBITRO??

#visualizacion: equipo vs tiros de cada equipo dentro y fuera de casa, hacer counts de todo
#tipo que equipo es el mas guarro, limpio etc

#evaluar resultados por temporadas -> SARA/MARTA
#evaluar resultados por equipos (los que escojamos)
