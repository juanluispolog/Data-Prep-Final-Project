
# This script stores the code used for implement a random forest

rm(list=ls())

setwd("/Users/juanluispolog/OneDrive - Universidad Politécnica de Madrid/JUAN LUIS/06 MCS/CSP571_DataPrepAnalysis/project/DataPrep")
source("datasetPrep.R")

df.15.16 <- prepDataset(read.csv("premier-15-16.csv"))
df.16.17 <- prepDataset(read.csv("premier-16-17.csv"))
df.17.18 <- prepDataset(read.csv("premier-17-18.csv"))
df.18.19 <- prepDataset(read.csv("premier-18-19.csv"))

# Merging datasets:
df <- rbind(df.15.16, df.16.17, df.17.18, df.18.19)

# Creating data frame for given team:
df <- wld(df, "Chelsea")

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


# DT with all attributes:
dt.model <- rpart(WLD ~ Season + SF + HomeTeam + AwayTeam + Referee, method='class', data=df.train)
rpart.plot(dt.model, extra = 104, type = 4, fallen.leaves = T, 
           main = "Decision Tree on WLD")
dt.pred<- predict(dt.model, df.test, type = "class")
confusionMatrix(dt.pred, df.test$WLD)


### RANDOM FOREST:

# Using randomForest library for create the random forest models:
library(randomForest)

# RF with all attributes:
rf.model <- randomForest(WLD ~ Season + SF + HomeTeam + AwayTeam + Referee, data = df.train)
rf.pred <- predict(rf.model, df.test, type="class")
confusionMatrix(rf.pred, df.test$WLD)
