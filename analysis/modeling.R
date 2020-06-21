library(data.table)
library(caret)
library(xgboost)
library(pROC)
library(rpart)
library(rpart.plot)
library(gbm)
library(tidyverse)

data <- readRDS("data/labeled_stats.RDS")
data_as_is <- readRDS("data/labeled_stats.RDS")

# let's actually drop NAs (defensive scoring stats since 2009)

naniar::vis_miss(data)
data <- data.frame(data)
data <- data[, which(colMeans(!is.na(data)) > 0.5)]
data <- data.table(data)

#############################################
########### sb player prediction ############
#############################################

data[, win_SB := NULL]
data[, season := NULL]

data$play_SB <- ifelse(data$play_SB == 1, 'played_sb', 'did_not_play_sb')
data$play_SB <- factor(data$play_SB)

# train test

set.seed(20202020)
train_index <- createDataPartition(data[["play_SB"]], p = 0.75, list = F)
train <- data[train_index,]
test <- data[-train_index]

# scale features

#numerics <- names(select_if(train, is.numeric))

#scales <- build_scales(dataSet = train, cols = numerics, verbose = F)
#fastScale(dataSet = train, scales = scales, verbose = F)
#fastScale(dataSet = test, scales = scales, verbose = F) 


# control

trC <- trainControl(method = "CV", number = 3)


### simple cart

set.seed(20202020)

CART <- train(play_SB ~ . -Team,
              method = "rpart",
              data = data,
              tuneGrid = data.frame(cp = c(0.02)), # 0.04
              trControl = trC)

rpart.plot(CART$finalModel,
           type = 1, 
           extra = 4,
           leaf.round = 0,
           fallen.leaves = F, 
           branch = 1, 
           under = F,
           tweak = 4/3, 
           cex = 2/3)

# what decides playing in SB
# 1. offensive scored points > 426 AND defensive allowed rush TDs < 8
# 1. offensive scored points > 426 AND defensive allowed rush TDs >= 8 AND defense allowed pass rating < 72


### random forest

set.seed(20202020)

rf <- train(play_SB ~ . -Team,
            method = "ranger",
            data = train,
            trControl = trainControl(method = "CV", number = 3, classProbs=TRUE),
            tuneGrid = expand.grid(.mtry = c(3, 5, 7),
                                   .splitrule = c("gini", "extratrees", "hellinger" ),
                                   .min.node.size = seq(10, 50, 10)),
            importance = "impurity")

rf$results

rf$results[rf$results$mtry == rf$bestTune$mtry & 
             rf$results$min.node.size == rf$bestTune$min.node.size &
             rf$results$splitrule == rf$bestTune$splitrule,,]


pred <- predict(rf, newdata = test, type = 'prob')$played_sb
act <- ifelse(test$play_SB == "played_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj) # 0.89 AUC

plot(roc_obj, 
     print.thres = "best", auc.polygon = T, print.auc = T,
     print.auc.x = 0.25, print.auc.y = 0.25,
     xlab = "1 - False Positive Rate", ylab = "True Positive Rate",
     grid=c(0.1, 0.2), max.auc.polygon = T, auc.polygon.col = "lightblue")


probs <- predict(rf, test, type = 'prob')
threshold <- 0.25
pred      <- factor(ifelse(probs[, "played_sb"] > threshold, "played_sb", "did_not_play_sb") )
confusionMatrix(pred, test$play_SB)



### gbm

set.seed(20202020)

GBM <- train(play_SB ~ . -Team,
             method = "gbm",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(n.trees = 500, 
                                    interaction.depth = c(7), # 7, 8, 9 
                                    shrinkage = c(0.001), # 0.01, 0.005, 0.001
                                    n.minobsinnode = c(14)), #14, 15 ,16
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
threshold <- 0.1
pred      <- factor(ifelse(probs[, "played_sb"] > threshold, "played_sb", "did_not_play_sb") )
confusionMatrix(pred, test$play_SB)



### xgboost

set.seed(20202020)

XGB <- train(play_SB ~ . -Team,
             method = "xgbTree",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(nrounds = 500,
                                    max_depth = c(1), # 1, 2
                                    eta = c(0.005),
                                    gamma = 0.1,
                                    colsample_bytree = c(1/3), # 1/3, 0.4
                                    min_child_weight = c(7), # 7, 10, 13
                                    subsample = c(0.5))) # 0.5, 2/3

XGB$results %>% arrange(desc(Accuracy))

XGB$results[XGB$results$max_depth == XGB$bestTune$max_depth & 
              XGB$results$eta == XGB$bestTune$eta &
              XGB$results$colsample_bytree == XGB$bestTune$colsample_bytree &
              XGB$results$min_child_weight == XGB$bestTune$min_child_weight &
              XGB$results$subsample == XGB$bestTune$subsample,]


pred <- predict(XGB, newdata = test, type = 'prob')$played_sb
act <- ifelse(test$play_SB == "played_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj)

plot(roc_obj, 
     print.thres = "best", auc.polygon = T, print.auc = T,
     print.auc.x = 0.25, print.auc.y = 0.25,
     xlab = "1 - False Positive Rate", ylab = "True Positive Rate",
     grid=c(0.1, 0.2), max.auc.polygon = T, auc.polygon.col = "lightblue")


probs <- predict(XGB, test, type = 'prob')
threshold <- 0.2
pred      <- factor(ifelse(probs[, "played_sb"] > threshold, "played_sb", "did_not_play_sb") )
confusionMatrix(pred, test$play_SB)



### variable imporatances

varimpRF <- varImp(rf)$importance
varimpRF$feature <- rownames(varimpRF)

varimpGBM <- varImp(GBM)$importance
varimpGBM$feature <- rownames(varimpGBM)

varimpXGB <- varImp(XGB)$importance
varimpXGB$feature <- rownames(varimpXGB)

varImps <- merge(merge(x = varimpRF, y = varimpGBM, by = "feature"), varimpXGB, "feature")
colnames(varImps) <- c("feature", "RF", "GBM", "XGB")

varImps %>% arrange(desc(GBM), desc(XGB))


melt(varImps) %>% 
  group_by(variable) %>% 
  top_n(15, value) %>% 
  ungroup() %>% 
  mutate(variable = as.factor(variable),
         feature = as.factor(feature),
         feature = tidytext::reorder_within(feature, value, variable)) %>%
  ggplot(aes(feature, value, color = variable, fill = variable)) +
  geom_col(position = 'dodge', show.legend = F) +
  coord_flip() +
  labs(subtitle = 'Most important stats deciding who makes it to the Super Bowl',
       title = 'Scored points by offense, QB pass rating, # of PAT attempts & allowed rush TDs are most deciding stats',
       caption = 'Non-scaled features',
       x = 'Statistic',
       y = 'Importance') +
  theme_bw() +
  tidytext::scale_x_reordered() +
  facet_wrap(~ variable, scales = 'free')

ggsave("plots/feature_importances_non_scaled_play_SB_with_RF.png", device = 'png', plot = last_plot())


### enhance dataset with predictions
data_w_predictions <- data.table()
data_w_predictions[, team := data_as_is$Team]
data_w_predictions[, season := data_as_is$season]
data_w_predictions[, season := factor(season)]
data_w_predictions[, played_sb := data_as_is$play_SB]
data_w_predictions[, prediction_GBM := predict(GBM, data, type = 'prob')$played_sb]
data_w_predictions[, prediction_RF := predict(rf, data, type = 'prob')$played_sb]
#data_w_predictions[, prediction_XGB := predict(XGB, data, type = 'prob')$played_sb]
data_w_predictions[, avg_prediction := (prediction_GBM + prediction_RF) / 2]
data_w_predictions <- data_w_predictions[order(-avg_prediction)]

View(data_w_predictions)

write_csv(data_w_predictions, "data/predictions_to_make_it_to_sb.csv")
  





#############################################
########### sb winner prediction ############
#############################################


# these are just default models
# TODO fine tune win-predictors


data <- readRDS("data/labeled_stats.RDS")
data_as_is <- readRDS("data/labeled_stats.RDS")

data <- data.frame(data)
data <- data[, which(colMeans(!is.na(data)) > 0.5)]
data <- data.table(data)

data[, play_SB := NULL]
data[, season := NULL]

data$win_SB <- ifelse(data$win_SB == 1, 'won_sb', 'lost_missed_sb')
data$win_SB <- factor(data$win_SB)

set.seed(20202020)
train_index <- createDataPartition(data[["win_SB"]], p = 0.75, list = F)
train <- data[train_index,]
test <- data[-train_index]



trC <- trainControl(method = "CV", number = 3)


set.seed(20202020)

rf <- train(win_SB ~ . -Team,
            method = "ranger",
            data = train,
            trControl = trainControl(method = "CV", number = 3, classProbs=TRUE),
            tuneGrid = expand.grid(.mtry = c(3, 5, 7),
                                   .splitrule = c("gini", "extratrees", "hellinger" ),
                                   .min.node.size = seq(10, 50, 10)),
            importance = "impurity")

rf$results

rf$results[rf$results$mtry == rf$bestTune$mtry & 
             rf$results$min.node.size == rf$bestTune$min.node.size &
             rf$results$splitrule == rf$bestTune$splitrule,,]

pred <- predict(rf, newdata = test, type = 'prob')$won_sb
act <- ifelse(test$win_SB == "won_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj)





set.seed(20202020)

GBM_w <- train(win_SB ~ . -Team,
             method = "gbm",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(n.trees = 500, 
                                    interaction.depth = c(7), # 7, 8, 9 
                                    shrinkage = c(0.001), # 0.01, 0.005, 0.001
                                    n.minobsinnode = c(14)), #14, 15 ,16
             verbose = T)

GBM_w$results

GBM_w$results[GBM_w$results$interaction.depth == GBM_w$bestTune$interaction.depth & 
                GBM_w$results$n.minobsinnode == GBM_w$bestTune$n.minobsinnode &
                GBM_w$results$shrinkage == GBM_w$bestTune$shrinkage,]


pred <- predict(GBM_w, newdata = test, type = 'prob')$won_sb
act <- ifelse(test$win_SB == "won_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj)


set.seed(20202020)

XGB_w <- train(win_SB ~ . -Team,
             method = "xgbTree",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(nrounds = 500,
                                    max_depth = c(1), # 1, 2
                                    eta = c(0.005),
                                    gamma = 0.1,
                                    colsample_bytree = c(1/3), # 1/3, 0.4
                                    min_child_weight = c(7), # 7, 10, 13
                                    subsample = c(0.5))) # 0.5, 2/3

XGB_w$results %>% arrange(desc(Accuracy))

XGB_w$results[XGB_w$results$max_depth == XGB_w$bestTune$max_depth & 
                XGB_w$results$eta == XGB_w$bestTune$eta &
                XGB_w$results$colsample_bytree == XGB_w$bestTune$colsample_bytree &
                XGB_w$results$min_child_weight == XGB_w$bestTune$min_child_weight &
                XGB_w$results$subsample == XGB_w$bestTune$subsample,]


pred <- predict(XGB_w, newdata = test, type = 'prob')$won_sb
act <- ifelse(test$win_SB == "won_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj)


varimpRF_w <- varImp(rf)$importance
varimpRF_w$feature <- rownames(varimpRF_w)

varimpGBM_w <- varImp(GBM_w)$importance
varimpGBM_w$feature <- rownames(varimpGBM_w)

varimpXGB_w <- varImp(XGB_w)$importance
varimpXGB_w$feature <- rownames(varimpXGB_w)

varImps_w <- merge(merge(x = varimpRF_w, y = varimpGBM_w, by = "feature"), varimpXGB_w, "feature")
colnames(varImps_w) <- c("feature", "RF", "GBM", "XGB")

varImps_w %>% arrange(desc(GBM), desc(XGB))


melt(varImps_w) %>% 
  group_by(variable) %>% 
  top_n(15, value) %>% 
  ungroup() %>% 
  mutate(variable = as.factor(variable),
         feature = as.factor(feature),
         feature = tidytext::reorder_within(feature, value, variable)) %>%
  ggplot(aes(feature, value, color = variable, fill = variable)) +
  geom_col(position = 'dodge', show.legend = F) +
  coord_flip() +
  labs(subtitle = 'Most important stats deciding who wins the Super Bowl',
       title = 'Allowed passed TDs by defense, passed yards / attemps by offense are most deciding stats',
       caption = 'Non-scaled features',
       x = 'Statistic',
       y = 'Importance') +
  theme_bw() +
  tidytext::scale_x_reordered() +
  facet_wrap(~ variable, scales = 'free')

ggsave("plots/feature_importances_non_scaled_win_SB_with_RF.png", device = 'png', plot = last_plot())
# Saving 11.7 x 5.33 in image


### enhance dataset with predictions
data_w_predictions_w <- data.table()
data_w_predictions_w[, team := data_as_is$Team]
data_w_predictions_w[, season := data_as_is$season]
data_w_predictions_w[, season := factor(season)]
data_w_predictions_w[, won_sb := data_as_is$win_SB]
data_w_predictions_w[, prediction_GBM := predict(GBM_w, data, type = 'prob')$won_sb]
data_w_predictions_w[, prediction_RF := predict(rf, data, type = 'prob')$won_sb]
#data_w_predictions[, prediction_XGB := predict(XGB, data, type = 'prob')$played_sb]
data_w_predictions_w[, avg_prediction := (prediction_GBM + prediction_RF) / 2]
data_w_predictions_w <- data_w_predictions_w[order(-avg_prediction)]

View(data_w_predictions_w)

write_csv(data_w_predictions_w, "data/predictions_to_win_the_sb.csv")

