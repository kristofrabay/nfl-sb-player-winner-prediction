library(data.table)
library(caret)
library(xgboost)
library(pROC)

data <- readRDS("data/labeled_stats.RDS")

# sb player prediction

data[, win_SB := NULL]
data[, season := NULL]

data$play_SB <- ifelse(data$play_SB == 1, 'won_sb', 'no_sb')
data$play_SB <- factor(data$play_SB)

# train test

set.seed(20202020)
train_index <- createDataPartition(data[["play_SB"]], p = 0.75, list = F)
train <- data[train_index,]
test <- data[-train_index]


# control

trC <- trainControl(method = "CV", number = 5)


# gbm

set.seed(20202020)

GBM <- train(play_SB ~ . -Team,
             method = "gbm",
             data = train,
             trControl = trC,
             na.action  = na.pass,
             tuneGrid = expand.grid(n.trees = 500, 
                                    interaction.depth = c(25, 30), 
                                    shrinkage = c(0.01),
                                    n.minobsinnode = c(10)),
             verbose = T)


GBM$results[GBM$results$interaction.depth == GBM$bestTune$interaction.depth & 
              GBM$results$n.minobsinnode == GBM$bestTune$n.minobsinnode &
              GBM$results$shrinkage == GBM$bestTune$shrinkage,]


pred <- predict(GBM, newdata = test, type = 'prob', na.action = na.pass)$won_sb
act <- ifelse(test$play_SB == "no_sb", 0, 1)
roc_obj <- roc(act, pred)
auc(roc_obj) # 0.9 AUC

plot(roc_obj, 
     print.thres = "best", auc.polygon = T, print.auc = T,
     print.auc.x = 0.25, print.auc.y = 0.25,
     xlab = "1 - False Positive Rate", ylab = "True Positive Rate",
     grid=c(0.1, 0.2), max.auc.polygon = T, auc.polygon.col = "lightblue")


probs <- predict(GBM, test, type = "prob", na.action = na.pass)
threshold <- 0.5
pred      <- factor(ifelse(probs[, "won_sb"] > threshold, "won_sb", "no_sb") )
confusionMatrix(pred, test$play_SB)


# TODO
# xgboost
# drop NA
# feature importances
# fine tune gbm, rf and xgb later
