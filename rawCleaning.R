# This script cointains the coded used for cleaning and homogeneizing the datasets
# Datasets from 5 past seasons have been used
# Last update for season 19/20 dataset: 10/26/2019

rm(list=ls())

setwd("/Users/juanluispolog/OneDrive - Universidad Politécnica de Madrid/JUAN LUIS/06 MCS/CSP571_DataPrepAnalysis/project/DataPrep")

df.15.16 <- read.csv("premier-15-16.csv")
df.16.17 <- read.csv("premier-16-17.csv")
df.17.18 <- read.csv("premier-17-18.csv")
df.18.19 <- read.csv("premier-18-19.csv")
df.19.20 <- read.csv("premier-19-20.csv")

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
                        yy = as.numeric(format(date, format = "%y"))
                        )
  df.list[[i]] <- data.frame(df.list[[i]]$Div, date.df, df.list[[i]][, c(3:23)])
}

df.15.16 <- df.list[[1]]
df.16.17 <- df.list[[2]]
df.17.18 <- df.list[[3]]
df.18.19 <- df.list[[4]]
df.19.20 <- df.list[[5]]


# Merging datasets

df.past <- rbind(df.15.16, df.16.17, df.17.18, df.18.19)
str(df.past)

df.current <- df.19.20
str(df.past)


