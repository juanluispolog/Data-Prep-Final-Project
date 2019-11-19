
# This script stores the code used to implement classification models

rm(list=ls())

setwd("/Users/juanluispolog/OneDrive - Universidad Politécnica de Madrid/JUAN LUIS/06 MCS/CSP571_DataPrepAnalysis/project/DataPrep")
source("datasetPrep.R")

df.15.16 <- prepDataset(read.csv("premier-15-16.csv"))
df.16.17 <- prepDataset(read.csv("premier-16-17.csv"))
df.17.18 <- prepDataset(read.csv("premier-17-18.csv"))
df.18.19 <- prepDataset(read.csv("premier-18-19.csv"))
df.19.20 <- prepDataset(read.csv("premier-19-20.csv"))

# Merging datasets:
df <- rbind(df.15.16, df.16.17, df.17.18, df.18.19, df.19.20)

# Creating data frames for given team:
df.chel <- wld(df, "Chelsea")
df.liv <- wld(df, "Liverpool")
df.tot <- wld(df, "Tottenham")
df.wat <- wld(df, "Watford")
df.west <- wld(df, "West Ham")
df.ars <- wld(df, "Arsenal")
df.manu <- wld(df, "Man United")

# Create data partition: test/train:
library("caret")
set.seed(1122)
df.train.rows <- createDataPartition(df$WLD, p = 0.8, list = F)
df.train <- df[df.train.rows,]
df.test <- df[-df.train.rows,]
rm(df.train.rows)


### DECISION TREE:

# Using rpart library for create the decision tree model:
library(rpart)
library(rpart.plot)


# DT with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.train <- selectPred(df.train, "WLD")
df.test <- selectPred(df.test, "WLD")

dt.model <- rpart(WLD ~ ., method='class', data=df.train)
rpart.plot(dt.model, extra = 104, type = 4, fallen.leaves = T, 
           main = "Decision Tree on WLD")
dt.pred<- predict(dt.model, df.test, type = "class")
confusionMatrix(dt.pred, df.test$WLD)









### RANDOM FOREST: CHELSEA

# Using randomForest library in order to create the random forest models:
library(randomForest)

# Data Partition: train/test
# Test: current season

# df.chel.train <- df.chel[-which(df.chel$Season == "19/20"),]
# df.chel.test <- df.chel[which(df.chel$Season == "19/20"),]

set.seed(1122)
df.train.rows <- createDataPartition(df.chel$WLD, p = 0.8, list = F)
df.chel.train <- df.chel[df.train.rows,]
df.chel.test <- df.chel[-df.train.rows,]
rm(df.train.rows)

# RF with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.chel.train <- selectPred(df.chel.train, "WLD")
df.chel.test <- selectPred(df.chel.test, "WLD")

rf.model <- randomForest(WLD ~ ., data = df.chel.train, mtry = 5, ntree = 100)
rf.pred <- predict(rf.model, df.chel.test, type="class")

# Plotting variabvle importance: decreasing gini
varImpPlot(rf.model, main = "Variable importance")

# Evaluating model accuracy
confusionMatrix(rf.pred, df.chel.test$WLD)

# Plotting tottenham games' result per season:
plot(df.chel[which(df.chel$Season=="15/16"), 27], main = "Season 15/16")
plot(df.chel[which(df.chel$Season=="16/17"), 27], main = "Season 16/17")
plot(df.chel[which(df.chel$Season=="17/18"), 27], main = "Season 17/18")
plot(df.chel[which(df.chel$Season=="18/19"), 27], main = "Season 18/19")
plot(df.chel[which(df.chel$Season=="19/20"), 27], main = "Current Season 19/20")
plot(df.chel.test$WLD, main = "Test for Season 19/20")
plot(rf.pred, main = "Predicted for Season 19/20")






### RANDOM FOREST: LIVERPOOL

# Using randomForest library in order to create the random forest models:
library(randomForest)

# Data Partition: train/test
# Test: current season
# 
# df.liv.train <- df.liv[-which(df.liv$Season == "19/20"),]
# df.liv.test <- df.liv[which(df.liv$Season == "19/20"),]

set.seed(1122)
df.train.rows <- createDataPartition(df.liv$WLD, p = 0.8, list = F)
df.liv.train <- df.liv[df.train.rows,]
df.liv.test <- df.liv[-df.train.rows,]
rm(df.train.rows)

# RF with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.liv.train <- selectPred(df.liv.train, "WLD")
df.liv.test <- selectPred(df.liv.test, "WLD")

rf.model <- randomForest(WLD ~ ., data = df.liv.train, mtry = 5, ntree = 100)
rf.pred <- predict(rf.model, df.liv.test, type="class")

# Plotting variabvle importance: decreasing gini
varImpPlot(rf.model, main = "Variable importance")

# Evaluating model accuracy
confusionMatrix(rf.pred, df.liv.test$WLD)

# Plotting LIVERPOOL games' result per season:
plot(df.liv[which(df.liv$Season=="15/16"), 27], main = "Season 15/16")
plot(df.liv[which(df.liv$Season=="16/17"), 27], main = "Season 16/17")
plot(df.liv[which(df.liv$Season=="17/18"), 27], main = "Season 17/18")
plot(df.liv[which(df.liv$Season=="18/19"), 27], main = "Season 18/19")
plot(df.liv[which(df.liv$Season=="19/20"), 27], main = "Current Season 19/20")
plot(df.liv.test$WLD, main = "Test for Season 19/20")
plot(rf.pred, main = "Predicted for Season 19/20")





### RANDOM FOREST: Tottenham

# Using randomForest library in order to create the random forest models:
library(randomForest)

# Data Partition: train/test
# Test: current season
# 
# df.tot.train <- df.tot[-which(df.tot$Season == "19/20"),]
# df.tot.test <- df.tot[which(df.tot$Season == "19/20"),]

set.seed(1122)
df.train.rows <- createDataPartition(df.tot$WLD, p = 0.8, list = F)
df.tot.train <- df.tot[df.train.rows,]
df.tot.test <- df.tot[-df.train.rows,]
rm(df.train.rows)

# RF with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.tot.train <- selectPred(df.tot.train, "WLD")
df.tot.test <- selectPred(df.tot.test, "WLD")

rf.model <- randomForest(WLD ~ ., data = df.tot.train, mtry = 5, ntree = 100)
rf.pred <- predict(rf.model, df.tot.test, type="class")

# Plotting variabvle importance: decreasing gini
varImpPlot(rf.model, main = "Variable importance")

# Evaluating model accuracy
confusionMatrix(rf.pred, df.tot.test$WLD)


# Plotting tottenham games' result per season:
plot(df.tot[which(df.tot$Season=="15/16"), 27], main = "Season 15/16")
plot(df.tot[which(df.tot$Season=="16/17"), 27], main = "Season 16/17")
plot(df.tot[which(df.tot$Season=="17/18"), 27], main = "Season 17/18")
plot(df.tot[which(df.tot$Season=="18/19"), 27], main = "Season 18/19")
plot(df.tot[which(df.tot$Season=="19/20"), 27], main = "Current Season 19/20")
plot(df.tot.test$WLD, main = "Test for Season 19/20")
plot(rf.pred, main = "Predicted for Season 19/20")

# Regarding the confussion matrix and the plots, since the sensitivity for one of the classes
# is high (in this case WIN), we can bet for it.





### RANDOM FOREST: WATFORD

# Using randomForest library in order to create the random forest models:
library(randomForest)

# Data Partition: train/test
# Test: current season

# df.wat.train <- df.wat[-which(df.wat$Season == "19/20"),]
# df.wat.test <- df.wat[which(df.wat$Season == "19/20"),]

set.seed(1122)
df.train.rows <- createDataPartition(df.wat$WLD, p = 0.8, list = F)
df.wat.train <- df.wat[df.train.rows,]
df.wat.test <- df.wat[-df.train.rows,]
rm(df.train.rows)

# RF with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.wat.train <- selectPred(df.wat.train, "WLD")
df.wat.test <- selectPred(df.wat.test, "WLD")

rf.model <- randomForest(WLD ~ ., data = df.wat.train, mtry = 5, ntree = 100)
rf.pred <- predict(rf.model, df.wat.test, type="class")

# Plotting variabvle importance: decreasing gini
varImpPlot(rf.model, main = "Variable importance")

# Evaluating model accuracy
confusionMatrix(rf.pred, df.wat.test$WLD)

# Plotting tottenham games' result per season:
plot(df.wat[which(df.wat$Season=="15/16"), 27], main = "Season 15/16")
plot(df.wat[which(df.wat$Season=="16/17"), 27], main = "Season 16/17")
plot(df.wat[which(df.wat$Season=="17/18"), 27], main = "Season 17/18")
plot(df.wat[which(df.wat$Season=="18/19"), 27], main = "Season 18/19")
plot(df.wat[which(df.wat$Season=="19/20"), 27], main = "Current Season 19/20")
plot(df.wat.test$WLD, main = "Test for Season 19/20")
plot(rf.pred, main = "Predicted for Season 19/20")






### RANDOM FOREST: WEST HAM

# Using randomForest library in order to create the random forest models:
library(randomForest)

# Data Partition: train/test
# Test: current season

set.seed(1122)
df.train.rows <- createDataPartition(df.west$WLD, p = 0.8, list = F)
df.west.train <- df.west[df.train.rows,]
df.west.test <- df.west[-df.train.rows,]
rm(df.train.rows)

# RF with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.west.train <- selectPred(df.west.train, "WLD")
df.west.test <- selectPred(df.west.test, "WLD")

rf.model <- randomForest(WLD ~ ., data = df.west.train, mtry = 5, ntree = 100)
rf.pred <- predict(rf.model, df.west.test, type="class")

# Plotting variabvle importance: decreasing gini
varImpPlot(rf.model, main = "Variable importance")

# Evaluating model accuracy
confusionMatrix(rf.pred, df.west.test$WLD)

# Plotting tottenham games' result per season:
plot(df.west[which(df.west$Season=="15/16"), 27], main = "Season 15/16")
plot(df.west[which(df.west$Season=="16/17"), 27], main = "Season 16/17")
plot(df.west[which(df.west$Season=="17/18"), 27], main = "Season 17/18")
plot(df.west[which(df.west$Season=="18/19"), 27], main = "Season 18/19")
plot(df.west[which(df.west$Season=="19/20"), 27], main = "Current Season 19/20")
plot(df.west.test$WLD, main = "Test for Season 19/20")
plot(rf.pred, main = "Predicted for Season 19/20")






### RANDOM FOREST: ARSENAL

# Using randomForest library in order to create the random forest models:
library(randomForest)

# Data Partition: train/test
# Test: current season

set.seed(1122)
df.train.rows <- createDataPartition(df.ars$WLD, p = 0.8, list = F)
df.ars.train <- df.ars[df.train.rows,]
df.ars.test <- df.ars[-df.train.rows,]
rm(df.train.rows)

# RF with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.ars.train <- selectPred(df.ars.train, "WLD")
df.ars.test <- selectPred(df.ars.test, "WLD")

rf.model <- randomForest(WLD ~ ., data = df.ars.train, mtry = 5, ntree = 100)
rf.pred <- predict(rf.model, df.ars.test, type="class")

# Plotting variabvle importance: decreasing gini
varImpPlot(rf.model, main = "Variable importance")

# Evaluating model accuracy
confusionMatrix(rf.pred, df.ars.test$WLD)

# Plotting tottenham games' result per season:
plot(df.ars[which(df.ars$Season=="15/16"), 27], main = "Season 15/16")
plot(df.ars[which(df.ars$Season=="16/17"), 27], main = "Season 16/17")
plot(df.ars[which(df.ars$Season=="17/18"), 27], main = "Season 17/18")
plot(df.ars[which(df.ars$Season=="18/19"), 27], main = "Season 18/19")
plot(df.ars[which(df.ars$Season=="19/20"), 27], main = "Current Season 19/20")
plot(df.ars.test$WLD, main = "Test for Season 19/20")
plot(rf.pred, main = "Predicted for Season 19/20")



### RANDOM FOREST: MAN UNITED

# Using randomForest library in order to create the random forest models:
library(randomForest)

# Data Partition: train/test
# Test: current season

set.seed(1122)
df.train.rows <- createDataPartition(df.manu$WLD, p = 0.8, list = F)
df.manu.train <- df.ars[df.train.rows,]
df.manu.test <- df.ars[-df.train.rows,]
rm(df.train.rows)

# RF with attributes Season, SF, HomeTeam, AwayTeam, Referee:
df.manu.train <- selectPred(df.manu.train, "WLD")
df.manu.test <- selectPred(df.manu.test, "WLD")

rf.model <- randomForest(WLD ~ ., data = df.manu.train, mtry = 5, ntree = 100)
rf.pred <- predict(rf.model, df.manu.test, type="class")

# Plotting variabvle importance: decreasing gini
varImpPlot(rf.model, main = "Variable importance")

# Evaluating model accuracy
confusionMatrix(rf.pred, df.manu.test$WLD)

# Plotting tottenham games' result per season:
plot(df.ars[which(df.ars$Season=="15/16"), 27], main = "Season 15/16")
plot(df.ars[which(df.ars$Season=="16/17"), 27], main = "Season 16/17")
plot(df.ars[which(df.ars$Season=="17/18"), 27], main = "Season 17/18")
plot(df.ars[which(df.ars$Season=="18/19"), 27], main = "Season 18/19")
plot(df.ars[which(df.ars$Season=="19/20"), 27], main = "Current Season 19/20")
plot(df.manu.test$WLD, main = "Test")
plot(rf.pred, main = "Predicted")





#############################

## CONCLUSIONS

# The better the team, the better the model works




