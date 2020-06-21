library(data.table)
library(caret)
library(xgboost)
library(pROC)
library(rpart)
library(rpart.plot)
library(tidyverse)

data <- readRDS("data/labeled_stats.RDS")

# let's actually drop NAs (defensive scoring stats since 2009)

naniar::vis_miss(data)
data <- data.frame(data)
data <- data[, which(colMeans(!is.na(data)) > 0.5)]
data <- data.table(data)
# sb player prediction

data[, win_SB := NULL]
data[, season := NULL]

data$play_SB <- ifelse(data$play_SB == 1, 'played_sb', 'did_not_play_sb')
data$play_SB <- factor(data$play_SB)

# train test

set.seed(20202020)
train_index <- createDataPartition(data[["play_SB"]], p = 0.75, list = F)
train <- data[train_index,]
test <- data[-train_index]


# control

trC <- trainControl(method = "CV", number = 3)


# simple cart

set.seed(20202020)

CART <- train(play_SB ~ . -Team,
              method = "rpart",
              data = data,
              tuneGrid = data.frame(cp = c(0.02)), # 0.04
              trControl = trC)

rpart.plot(CART$finalModel,
           type = 1, 
           extra = 101,
           leaf.round = 0,
           fallen.leaves = F, 
           branch = 1, 
           under = F,
           tweak = 4/3, 
           cex = 2/3)

# what decides playing in SB
# 1. offensive scored points > 426 AND defensive allowed rush TDs < 8
# 1. offensive scored points > 426 AND defensive allowed rush TDs >= 8 AND defense allowed pass rating < 72

# gbm

set.seed(20202020)

GBM <- train(play_SB ~ . -Team,
             method = "gbm",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(n.trees = 500, 
                                    interaction.depth = c(7, 8, 9), 
                                    shrinkage = c(0.01, 0.005, 0.001),
                                    n.minobsinnode = c(14, 15, 16)),
             verbose = T)

GBM$results

GBM$results[GBM$results$interaction.depth == GBM$bestTune$interaction.depth & 
              GBM$results$n.minobsinnode == GBM$bestTune$n.minobsinnode &
              GBM$results$shrinkage == GBM$bestTune$shrinkage,]


pred <- predict(GBM, newdata = test, type = 'prob')$played_sb
act <- ifelse(test$play_SB == "played_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj) # 0.9 AUC

plot(roc_obj, 
     print.thres = "best", auc.polygon = T, print.auc = T,
     print.auc.x = 0.25, print.auc.y = 0.25,
     xlab = "1 - False Positive Rate", ylab = "True Positive Rate",
     grid=c(0.1, 0.2), max.auc.polygon = T, auc.polygon.col = "lightblue")


probs <- predict(GBM, test, type = 'prob')
threshold <- 0.2
pred      <- factor(ifelse(probs[, "played_sb"] > threshold, "played_sb", "did_not_play_sb") )
confusionMatrix(pred, test$play_SB)


# xgboost

# TODO


# enhance dataset with predictions
data_w_predictions <- data.table()
data_w_predictions[, team := data$Team]
data_w_predictions[, season := data$season]
data_w_predictions[, season := factor(season)]
data_w_predictions[, played_sb := data$play_SB]
data_w_predictions[, prediction := predict(GBM, data, type = 'prob')$played_sb]
str(data_w_predictions)

preds_making_it_to_sb_gbm <- data_w_predictions %>% 
  arrange(desc(prediction))

write_csv(preds_making_it_to_sb_gbm, "data/predictions_to_make_it_to_sb.csv")
  

