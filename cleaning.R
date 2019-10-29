
## This script stores some functions used for cleaning and homogeneizing the datasets


# clean function removes the betting attributes, removes Time attribute (not useful) and
# homogeneizes the date format in order to have the same date format in all datasets.
# It receives the dataframe.
# It returns the cleaned dataframe.

clean <- function(df) {
  # Removing betting attributes from dataset
  df <- df[, - c(which(colnames(df) == "B365H"):ncol(df))]
  
  # Removing Time attribute in case it exists
  if (is.null(df$Time) == FALSE){
     df <- df[, - which(colnames(df) == "Time")]
    print("hello")
  }

  # Homogeneizing Date attribute: dd/mm/yy

  date <- as.Date(df$Date, format = "%d/%m/%Y")
  date.df <- data.frame("dd" = as.numeric(format(date, format = "%d")),
                        "mm" = as.numeric(format(date, format = "%m")),
                        "yy" = as.numeric(format(date, format = "%y")))

  df <- data.frame(date.df, df[, c(3:23)])

  return(df)
}



# wld function classify the futball game in win, loss or draw for a given team.
# It adds a column called WLD with class values: win, loss, draw (factor).
# It recieves two values: a season dataframe and the name of a team.
# It returns a dataframe containing football games for the given team and the new attibute WLD.

wld <- function(df, team) {
  
  df.team <- df[which(df$HomeTeam == team | df$AwayTeam == team), ]
  team.home <- which(df.team$HomeTeam == team)
  team.away <- which(df.team$AwayTeam == team)
  team.wld <- c()
  
  for (i in team.home) {
    if(df.team[i,]$FTHG > df.team[i,]$FTAG){
      team.wld[i] <- "win"
    }
    if(df.team[i,]$FTHG == df.team[i,]$FTAG){
      team.wld[i] <- "draw"
    }
    if(df.team[i,]$FTHG < df.team[i,]$FTAG){
      team.wld[i] <- "loss"
    }
  }
  
  for (i in team.away) {
    if(df.team[i,]$FTHG > df.team[i,]$FTAG){
      team.wld[i] <- "loss"
    }
    if(df.team[i,]$FTHG == df.team[i,]$FTAG){
      team.wld[i] <- "draw"
    }
    if(df.team[i,]$FTHG < df.team[i,]$FTAG){
      team.wld[i] <- "win"
    }
  }
  
  df.wld <- data.frame("WLD" = team.wld)
  df.team <- data.frame(df.team, df.wld)
  
  return(df.team)
}


# homeTeam function selects rows in a dataframe for a given home team
# It receives a dataframe and the team name as a string
# It returns a new dataframe that contains rows with the given home team

homeTeam <- function(df, home.team){
  df <- df[c(which(df$HomeTeam == home.team)), ]
  return(df)
}

# awayTeam function selects rows in a dataframe for a given away team
# It receives a dataframe and the team name as a string
# It returns a new dataframe that contains rows with the given away team

awayTeam <- function(df, away.team){
  df <- df[c(which(df$AwayTeam == away.team)), ]
  return(df)
}

