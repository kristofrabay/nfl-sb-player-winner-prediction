library(data.table)
library(caret)
library(xgboost)
library(pROC)
library(rpart)
library(rpart.plot)
library(gbm)
library(tidyverse)
library(dataPreparation)

data <- readRDS("data/labeled_stats.RDS")
data_as_is <- readRDS("data/labeled_stats.RDS")

# let's actually drop NAs (defensive scoring stats since 2009)

naniar::vis_miss(data)
data <- data.frame(data)
data <- data[, which(colMeans(!is.na(data)) > 0.5)]
data <- data.table(data)

# correlation checks

data[, win_SB := NULL]
data[, season := NULL]

cors <- cor(data %>% select_if(is.numeric))
cors[upper.tri(cors, diag = TRUE)] <- NA
reshape2::melt(cors, na.rm=TRUE, value.name = "cor") %>% arrange((cor))

to_drop <- c('off_scoring_TD_pass', 'off_scoring_TD_rush', 'off_downs_first_downs_rush', 'off_scoring_TD_pr', 
             'def_downs_first_downs_rush', 'off_scoring_TD_kr', 'off_pass_NetYds', 'def_pass_NetYds', 
             'def_kick_ret_Yds', 'off_kick_ret_Yds', 'off_rush_Yds', 'def_rush_Yds', 'def_punt_ret_Yds', 
             'off_punt_ret_Yds', 'off_pass_Yds', 'def_pass_Yds', 'def_kick_ret_Num', 'off_kick_ret_Num', 
             'off_pass_Loss', 'def_pass_Loss')

data <- data[ , !(names(data) %in% to_drop), with = F]


# normalized statistics (per game, ratios)

# OFFENSE

data$off_pass_TD_per_attempts <- data$off_pass_TD / data$off_pass_Att
data$off_pass_INT_per_attempts <- data$off_pass_Int / data$off_pass_Att
data$off_pass_sack_per_attempts <- data$off_pass_Sack / data$off_pass_Att

data$off_rush_TD_per_attempts <- data$off_rush_TD / data$off_rush_Att
data$off_rush_FD_per_attempts <- data$off_rush_FD / data$off_rush_Att

data$off_pass_to_rush_attempts <- data$off_pass_Att / data$off_rush_Att

data$off_punt_ret_FC_to_Attempt <- data$off_punt_ret_FC / data$off_punt_ret_Num
data$off_punt_ret_TD_to_Attempt <- data$off_punt_ret_TD / data$off_punt_ret_Num
data$off_punt_inside20_to_Attempt <- data$off_punting_In20 / data$off_punting_Punts

data$off_scoring_pass_TD_rate <- data$off_pass_TD / data$off_scoring_TD_total
data$off_scoring_rush_TD_rate <- data$off_rush_TD / data$off_scoring_TD_total

data$off_PAT_rate <- data$off_scoring_kick_pat_made / data$off_scoring_kick_pat_att
data$off_FG_rate <- data$off_scoring_kick_fg_made / data$off_scoring_kick_fg_att

data$off_first_down_pass_rate <- data$off_downs_first_downs_pass / data$off_downs_first_downs_tot
data$off_first_down_pen_rate <- data$off_downs_first_downs_pen / data$off_downs_first_downs_tot

to_drop_non_normal <- c('off_pass_Att', 'off_pass_Cmp', 'off_pass_TD', 'off_pass_Int', 'off_pass_Sack', 
                        'off_rush_Att', 'off_rush_FD', 'off_rush_TD', 
                        'off_kick_ret_TD', 'off_punt_ret_Num', 'off_punt_ret_FC', 'off_punt_ret_TD', 'off_punting_In20', 
                        'off_punting_Punts', 'off_punting_Yds', 'off_punting_Blk', 'off_punting_TB',
                        'off_scoring_TD_total', 'off_scoring_TD_blocked_punt', 'off_scoring_TD_fr', 'off_scoring_TD_ir', '', 
                        'off_scoring_kick_pat_made', 'off_scoring_kick_pat_att', 'off_scoring_kick_fg_att', 'off_scoring_kick_fg_made', 
                        'off_scoring_conversions', 'off_scoring_safeties', 'off_downs_first_downs_pass', 'off_downs_first_downs_tot', 'off_downs_first_downs_pen',
                        'off_downs_third_downs_att', 'off_downs_third_downs_made', 
                        'off_downs_fourth_downs_att', 'off_downs_fourth_downs_made')

data <- data[ , !(names(data) %in% to_drop_non_normal), with = F]



# DEFENSE

data$def_pass_TD_per_attempts <- data$def_pass_TD / data$def_pass_Att
data$def_pass_INT_per_attempts <- data$def_pass_Int / data$def_pass_Att
data$def_pass_sack_per_attempts <- data$def_pass_Sack / data$def_pass_Att

data$def_rush_TD_per_attempts <- data$def_rush_TD / data$def_rush_Att
data$def_rush_FD_per_attempts <- data$def_rush_FD / data$def_rush_Att

data$def_punt_ret_FC_to_Attempt <- data$def_punt_ret_FC / data$def_punt_ret_Num
data$def_punt_ret_TD_to_Attempt <- data$def_punt_ret_TD / data$def_punt_ret_Num
data$def_punt_blocked_ratio <- data$def_punting_Blk / data$def_punting_Punts
data$def_punt_TB_ratio <- data$def_punting_TB / data$def_punting_Punts
data$def_punt_in20_ratio <- data$def_punting_In20 / data$def_punting_Punts

data$def_first_down_pass_rate <- data$def_downs_first_downs_pass / data$def_downs_first_downs_tot
data$def_first_down_pen_rate <- data$def_downs_first_downs_pen / data$def_downs_first_downs_tot

to_drop_non_normal <- c('def_pass_Att', 'def_pass_Cmp', 'def_pass_Sack', 'def_pass_TD', 'def_pass_Int', 
                        'def_rush_TD', 'def_rush_Att', 'def_rush_FD', 'def_kick_ret_TD', 
                        'def_punt_ret_Num', 'def_punt_ret_TD', 'def_punt_ret_FC', 'def_punting_Punts', 
                        'def_punting_Yds', 'def_punting_Blk', 'def_punting_TB', 'def_punting_In20',
                        'def_downs_third_downs_att', 'def_downs_third_downs_made', 
                        'def_downs_first_downs_pen', 'def_downs_fourth_downs_att', 'def_downs_fourth_downs_made',
                        'def_downs_first_downs_pass', 'def_downs_first_downs_tot')

data <- data[ , !(names(data) %in% to_drop_non_normal), with = F]

# points

data$off_scoring_points_per_game <- data$off_scoring_points / 16
data$off_scoring_points <- NULL


str(data)

#############################################
########### sb player prediction ############
#############################################

data$play_SB <- ifelse(data$play_SB == 1, 'played_sb', 'did_not_play_sb')
data$play_SB <- factor(data$play_SB)

saveRDS(names(data), 'analysis/namescheck.RDS')

# train test

set.seed(20202020)
train_index <- createDataPartition(data[["play_SB"]], p = 0.75, list = F)
train <- data[train_index,]
test <- data[-train_index]

# scale features NO NEED FOR TREE BASED METHODS

numerics <- names(select_if(train, is.numeric))

#scales <- build_scales(dataSet = train, cols = numerics, verbose = F)
#fastScale(dataSet = train, scales = scales, verbose = F)
#fastScale(dataSet = test, scales = scales, verbose = F) 


# control

trC <- trainControl(method = "CV", number = 3)


### simple cart

set.seed(20202020)

CART <- train(play_SB ~ . -Team,
              method = "rpart",
              data = train,
              #preProcess = c("center", "scale"),
              tuneGrid = data.frame(cp = c(0.01)), # 0.04
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
                                   .min.node.size = seq(7, 9, 11)),
            importance = "impurity")

rf$results

rf$results[rf$results$mtry == rf$bestTune$mtry & 
             rf$results$min.node.size == rf$bestTune$min.node.size &
             rf$results$splitrule == rf$bestTune$splitrule,,]


pred <- predict(rf, newdata = test, type = 'prob')$played_sb
act <- ifelse(test$play_SB == "played_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj) # 0.9046 AUC

plot(roc_obj, 
     print.thres = "best", auc.polygon = T, print.auc = T,
     print.auc.x = 0.25, print.auc.y = 0.25,
     xlab = "1 - False Positive Rate", ylab = "True Positive Rate",
     grid=c(0.1, 0.2), max.auc.polygon = T, auc.polygon.col = "lightblue")


probs <- predict(rf, test, type = 'prob')
threshold <- 0.25
pred      <- factor(ifelse(probs[, "played_sb"] > threshold, "played_sb", "did_not_play_sb") )
confusionMatrix(pred, test$play_SB)

saveRDS(rf, "analysis/RFMODEL.RDS")

### gbm

set.seed(20202020)

GBM <- train(play_SB ~ . -Team,
             method = "gbm",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(n.trees = 200, 
                                    interaction.depth = c(7, 8, 9), # 7, 8, 9 
                                    shrinkage = c(0.01, 0.005), # 0.01, 0.005, 0.001
                                    n.minobsinnode = c(7, 9, 11)), #14, 15 ,16
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

saveRDS(GBM, "analysis/GBMMODEL.RDS")

### xgboost

set.seed(20202020)

XGB <- train(play_SB ~ . -Team,
             method = "xgbTree",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(nrounds = 500,
                                    max_depth = c(3, 5), # 3
                                    eta = c(0.005, 0.01), # 0.005
                                    gamma = 0.1,
                                    colsample_bytree = c(1/3, 1/2), # 1/3
                                    min_child_weight = c(5, 7, 9), # 5
                                    subsample = c(0.5, 1))) # 1

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


saveRDS(XGB, "analysis/XGBMODEL.RDS")


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
  top_n(10, value) %>% 
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

# correlation checks

data[, play_SB := NULL]
data[, season := NULL]

cors <- cor(data %>% select_if(is.numeric))
cors[upper.tri(cors, diag = TRUE)] <- NA
reshape2::melt(cors, na.rm=TRUE, value.name = "cor") %>% arrange(desc(cor))

to_drop <- c('off_scoring_TD_pass', 'off_scoring_TD_rush', 'off_downs_first_downs_rush', 'off_scoring_TD_pr', 
             'def_downs_first_downs_rush', 'off_scoring_TD_kr', 'off_pass_NetYds', 'def_pass_NetYds', 
             'def_kick_ret_Yds', 'off_kick_ret_Yds', 'off_rush_Yds', 'def_rush_Yds', 'def_punt_ret_Yds', 
             'off_punt_ret_Yds', 'off_pass_Yds', 'def_pass_Yds', 'def_kick_ret_Num', 'off_kick_ret_Num', 
             'off_pass_Loss', 'def_pass_Loss')

data <- data[ , !(names(data) %in% to_drop), with = F]


# normalized statistics (per game, ratios)

# OFFENSE

data$off_pass_TD_per_attempts <- data$off_pass_TD / data$off_pass_Att
data$off_pass_INT_per_attempts <- data$off_pass_Int / data$off_pass_Att
data$off_pass_sack_per_attempts <- data$off_pass_Sack / data$off_pass_Att

data$off_rush_TD_per_attempts <- data$off_rush_TD / data$off_rush_Att
data$off_rush_FD_per_attempts <- data$off_rush_FD / data$off_rush_Att

data$off_pass_to_rush_attempts <- data$off_pass_Att / data$off_rush_Att

data$off_punt_ret_FC_to_Attempt <- data$off_punt_ret_FC / data$off_punt_ret_Num
data$off_punt_ret_TD_to_Attempt <- data$off_punt_ret_TD / data$off_punt_ret_Num
data$off_punt_inside20_to_Attempt <- data$off_punting_In20 / data$off_punting_Punts

data$off_scoring_pass_TD_rate <- data$off_pass_TD / data$off_scoring_TD_total
data$off_scoring_rush_TD_rate <- data$off_rush_TD / data$off_scoring_TD_total

data$off_PAT_rate <- data$off_scoring_kick_pat_made / data$off_scoring_kick_pat_att
data$off_FG_rate <- data$off_scoring_kick_fg_made / data$off_scoring_kick_fg_att

data$off_first_down_pass_rate <- data$off_downs_first_downs_pass / data$off_downs_first_downs_tot
data$off_first_down_pen_rate <- data$off_downs_first_downs_pen / data$off_downs_first_downs_tot

to_drop_non_normal <- c('off_pass_Att', 'off_pass_Cmp', 'off_pass_TD', 'off_pass_Int', 'off_pass_Sack', 
                        'off_rush_Att', 'off_rush_FD', 'off_rush_TD', 
                        'off_kick_ret_TD', 'off_punt_ret_Num', 'off_punt_ret_FC', 'off_punt_ret_TD', 'off_punting_In20', 
                        'off_punting_Punts', 'off_punting_Yds', 'off_punting_Blk', 'off_punting_TB',
                        'off_scoring_TD_total', 'off_scoring_TD_blocked_punt', 'off_scoring_TD_fr', 'off_scoring_TD_ir', '', 
                        'off_scoring_kick_pat_made', 'off_scoring_kick_pat_att', 'off_scoring_kick_fg_att', 'off_scoring_kick_fg_made', 
                        'off_scoring_conversions', 'off_scoring_safeties', 'off_downs_first_downs_pass', 'off_downs_first_downs_tot', 'off_downs_first_downs_pen',
                        'off_downs_third_downs_att', 'off_downs_third_downs_made', 
                        'off_downs_fourth_downs_att', 'off_downs_fourth_downs_made')

data <- data[ , !(names(data) %in% to_drop_non_normal), with = F]



# DEFENSE

data$def_pass_TD_per_attempts <- data$def_pass_TD / data$def_pass_Att
data$def_pass_INT_per_attempts <- data$def_pass_Int / data$def_pass_Att
data$def_pass_sack_per_attempts <- data$def_pass_Sack / data$def_pass_Att

data$def_rush_TD_per_attempts <- data$def_rush_TD / data$def_rush_Att
data$def_rush_FD_per_attempts <- data$def_rush_FD / data$def_rush_Att

data$def_punt_ret_FC_to_Attempt <- data$def_punt_ret_FC / data$def_punt_ret_Num
data$def_punt_ret_TD_to_Attempt <- data$def_punt_ret_TD / data$def_punt_ret_Num
data$def_punt_blocked_ratio <- data$def_punting_Blk / data$def_punting_Punts
data$def_punt_TB_ratio <- data$def_punting_TB / data$def_punting_Punts
data$def_punt_in20_ratio <- data$def_punting_In20 / data$def_punting_Punts

data$def_first_down_pass_rate <- data$def_downs_first_downs_pass / data$def_downs_first_downs_tot
data$def_first_down_pen_rate <- data$def_downs_first_downs_pen / data$def_downs_first_downs_tot

to_drop_non_normal <- c('def_pass_Att', 'def_pass_Cmp', 'def_pass_Sack', 'def_pass_TD', 'def_pass_Int', 
                        'def_rush_TD', 'def_rush_Att', 'def_rush_FD', 'def_kick_ret_TD', 
                        'def_punt_ret_Num', 'def_punt_ret_TD', 'def_punt_ret_FC', 'def_punting_Punts', 
                        'def_punting_Yds', 'def_punting_Blk', 'def_punting_TB', 'def_punting_In20',
                        'def_downs_third_downs_att', 'def_downs_third_downs_made', 
                        'def_downs_first_downs_pen', 'def_downs_fourth_downs_att', 'def_downs_fourth_downs_made',
                        'def_downs_first_downs_pass', 'def_downs_first_downs_tot')

data <- data[ , !(names(data) %in% to_drop_non_normal), with = F]

# points

data$off_scoring_points_per_game <- data$off_scoring_points / 16
data$off_scoring_points <- NULL



data$win_SB <- ifelse(data$win_SB == 1, 'won_sb', 'lost_missed_sb')
data$win_SB <- factor(data$win_SB)

set.seed(20202020)
train_index <- createDataPartition(data[["win_SB"]], p = 0.75, list = F)
train <- data[train_index,]
test <- data[-train_index]


# decision tree

set.seed(20202020)

CART <- train(win_SB ~ . -Team,
              method = "rpart",
              data = train,
              #preProcess = c("center", "scale"),
              tuneGrid = data.frame(cp = c(0.01, 0.02, 0.005)),
              trControl = trC)

CART

rpart.plot(CART$finalModel,
           type = 1, 
           extra = 4,
           leaf.round = 0,
           fallen.leaves = F, 
           branch = 1, 
           under = F,
           tweak = 4/3, 
           cex = 2/3)






trC <- trainControl(method = "CV", number = 3)


set.seed(20202020)

rf_w <- train(win_SB ~ . -Team,
            method = "ranger",
            data = train,
            trControl = trainControl(method = "CV", number = 3, classProbs=TRUE),
            tuneGrid = expand.grid(.mtry = c(3, 5, 7),
                                   .splitrule = c("gini", "extratrees", "hellinger" ),
                                   .min.node.size = seq(10, 50, 10)),
            importance = "impurity")

rf_w$results

rf_w$results[rf_w$results$mtry == rf_w$bestTune$mtry & 
               rf_w$results$min.node.size == rf_w$bestTune$min.node.size &
               rf_w$results$splitrule == rf_w$bestTune$splitrule,,]

pred <- predict(rf_w, newdata = test, type = 'prob')$won_sb
act <- ifelse(test$win_SB == "won_sb", 1, 0)
roc_obj <- roc(act, pred)
auc(roc_obj)





set.seed(20202020)

GBM_w <- train(win_SB ~ . -Team,
             method = "gbm",
             data = train,
             trControl = trC,
             tuneGrid = expand.grid(n.trees = 200, 
                                    interaction.depth = c(5, 7, 9), # 5
                                    shrinkage = c(0.001, 0.005, 0.01), # 0.01, 
                                    n.minobsinnode = c(11, 13, 15)), #11
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
             tuneGrid = expand.grid(nrounds = 200,
                                    max_depth = c(3, 5, 7), # 1, 2
                                    eta = c(0.005, 0.001),
                                    gamma = 0.1,
                                    colsample_bytree = c(1/3, 1/2), # 1/3, 0.4
                                    min_child_weight = c(5, 7, 9), # 7, 10, 13
                                    subsample = c(0.5, 1))) # 0.5, 2/3

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
  top_n(10, value) %>% 
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
data_w_predictions_w[, prediction_RF := predict(rf_w, data, type = 'prob')$won_sb]
data_w_predictions_w[, prediction_XGB := predict(XGB_w, data, type = 'prob')$won_sb]
data_w_predictions_w[, avg_prediction := (prediction_GBM + prediction_RF + prediction_XGB) / 3]
data_w_predictions_w <- data_w_predictions_w[order(-avg_prediction)]

View(data_w_predictions_w)

pred <- data_w_predictions_w$avg_prediction
act <- data_w_predictions_w$won_sb
roc_obj <- roc(act, pred)
auc(roc_obj)

saveRDS(rf_w, "analysis/RFMODELW.RDS")
saveRDS(GBM_w, "analysis/GBMMODELW.RDS")
saveRDS(XGB_w, "analysis/XGBMODELW.RDS")

write_csv(data_w_predictions_w, "data/predictions_to_win_the_sb.csv")




### PREDICT ON 2020 ###

new_data <- readRDS("data/scraped_stats_2020.RDS")

new_data <- data.frame(new_data)
new_data <- new_data[, which(colMeans(!is.na(new_data)) > 0.5)]
new_data <- data.table(new_data)

new_data[, season := NULL]

names(data)
names(new_data)

# game number

new_data$games <- round(new_data$off_rush_Yds / new_data$off_rush_Yds.G)


# calculate features

new_data$off_pass_TD_per_attempts <- new_data$off_pass_TD / new_data$off_pass_Att
new_data$off_pass_INT_per_attempts <- new_data$off_pass_Int / new_data$off_pass_Att
new_data$off_pass_sack_per_attempts <- new_data$off_pass_Sack / new_data$off_pass_Att

new_data$off_rush_TD_per_attempts <- new_data$off_rush_TD / new_data$off_rush_Att
new_data$off_rush_FD_per_attempts <- new_data$off_rush_FD / new_data$off_rush_Att

new_data$off_pass_to_rush_attempts <- new_data$off_pass_Att / new_data$off_rush_Att

new_data$off_punt_ret_FC_to_Attempt <- new_data$off_punt_ret_FC / new_data$off_punt_ret_Num
new_data$off_punt_ret_TD_to_Attempt <- new_data$off_punt_ret_TD / new_data$off_punt_ret_Num
new_data$off_punt_inside20_to_Attempt <- new_data$off_punting_In20 / new_data$off_punting_Punts

new_data$off_scoring_pass_TD_rate <- new_data$off_pass_TD / new_data$off_scoring_TD_total
new_data$off_scoring_rush_TD_rate <- new_data$off_rush_TD / new_data$off_scoring_TD_total

new_data$off_PAT_rate <- new_data$off_scoring_kick_pat_made / new_data$off_scoring_kick_pat_att
new_data$off_FG_rate <- new_data$off_scoring_kick_fg_made / new_data$off_scoring_kick_fg_att

new_data$off_first_down_pass_rate <- new_data$off_downs_first_downs_pass / new_data$off_downs_first_downs_tot
new_data$off_first_down_pen_rate <- new_data$off_downs_first_downs_pen / new_data$off_downs_first_downs_tot

to_drop_non_normal <- c('off_pass_Att', 'off_pass_Cmp', 'off_pass_TD', 'off_pass_Int', 'off_pass_Sack', 
                        'off_rush_Att', 'off_rush_FD', 'off_rush_TD', 
                        'off_kick_ret_TD', 'off_punt_ret_Num', 'off_punt_ret_FC', 'off_punt_ret_TD', 'off_punting_In20', 
                        'off_punting_Punts', 'off_punting_Yds', 'off_punting_Blk', 'off_punting_TB',
                        'off_scoring_TD_total', 'off_scoring_TD_blocked_punt', 'off_scoring_TD_fr', 'off_scoring_TD_ir', '', 
                        'off_scoring_kick_pat_made', 'off_scoring_kick_pat_att', 'off_scoring_kick_fg_att', 'off_scoring_kick_fg_made', 
                        'off_scoring_conversions', 'off_scoring_safeties', 'off_downs_first_downs_pass', 'off_downs_first_downs_tot', 'off_downs_first_downs_pen',
                        'off_downs_third_downs_att', 'off_downs_third_downs_made', 
                        'off_downs_fourth_downs_att', 'off_downs_fourth_downs_made')

new_data <- new_data[ , !(names(new_data) %in% to_drop_non_normal), with = F]

new_data$def_pass_TD_per_attempts <- new_data$def_pass_TD / new_data$def_pass_Att
new_data$def_pass_INT_per_attempts <- new_data$def_pass_Int / new_data$def_pass_Att
new_data$def_pass_sack_per_attempts <- new_data$def_pass_Sack / new_data$def_pass_Att

new_data$def_rush_TD_per_attempts <- new_data$def_rush_TD / new_data$def_rush_Att
new_data$def_rush_FD_per_attempts <- new_data$def_rush_FD / new_data$def_rush_Att

new_data$def_punt_ret_FC_to_Attempt <- new_data$def_punt_ret_FC / new_data$def_punt_ret_Num
new_data$def_punt_ret_TD_to_Attempt <- new_data$def_punt_ret_TD / new_data$def_punt_ret_Num
new_data$def_punt_blocked_ratio <- new_data$def_punting_Blk / new_data$def_punting_Punts
new_data$def_punt_TB_ratio <- new_data$def_punting_TB / new_data$def_punting_Punts
new_data$def_punt_in20_ratio <- new_data$def_punting_In20 / new_data$def_punting_Punts

new_data$def_first_down_pass_rate <- new_data$def_downs_first_downs_pass / new_data$def_downs_first_downs_tot
new_data$def_first_down_pen_rate <- new_data$def_downs_first_downs_pen / new_data$def_downs_first_downs_tot

to_drop_non_normal <- c('def_pass_Att', 'def_pass_Cmp', 'def_pass_Sack', 'def_pass_TD', 'def_pass_Int', 
                        'def_rush_TD', 'def_rush_Att', 'def_rush_FD', 'def_kick_ret_TD', 
                        'def_punt_ret_Num', 'def_punt_ret_TD', 'def_punt_ret_FC', 'def_punting_Punts', 
                        'def_punting_Yds', 'def_punting_Blk', 'def_punting_TB', 'def_punting_In20',
                        'def_downs_third_downs_att', 'def_downs_third_downs_made', 
                        'def_downs_first_downs_pen', 'def_downs_fourth_downs_att', 'def_downs_fourth_downs_made',
                        'def_downs_first_downs_pass', 'def_downs_first_downs_tot')

new_data <- new_data[ , !(names(new_data) %in% to_drop_non_normal), with = F]

new_data$off_scoring_points_per_game <- new_data$off_scoring_points / new_data$games
new_data$off_scoring_points <- NULL



keep_cols <- which(colnames(new_data) %in% names(data))
new_data <- new_data %>% select(keep_cols)


# rename teams

new_data[, .N, by = Team]

new_data[(Team == 'Los Angeles RamsLA Rams') | 
       (Team == 'St. Louis RamsSt. Louis') |
       (Team == 'Los Angeles RamsLos Angeles'), Team := 'Rams']

new_data[(Team == 'Los Angeles RaidersLA Raiders') | 
       (Team == 'Oakland RaidersOakland') , Team := 'Raiders']

new_data[(Team == 'San Diego ChargersSan Diego') | 
       (Team == 'Los Angeles ChargersLA Chargers') , Team := 'Chargers']

new_data[(Team == 'Houston OilersHouston') | 
       (Team == 'Tennessee TitansTennessee') |
       (Team == 'Tennessee OilersTennessee'), Team := 'Titans']

teams <- new_data[, .N, by = Team]
teams[, Name := c("Cardinals", "Falcons", "Ravens", "Bills", "Panthers", "Bears", "Bengals", "Browns", 
                  "Cowboys", "Broncos", "Lions", "Packers", "Texans", "Colts", "Jaguars", "Chiefs", "Raiders",
                  "Chargers", "Rams", "Dolphins", "Vikings", "Patriots", "Saints", "Giants", "Jets", 
                  "Eagles", "Steelers", "49ers", "Seahawks", "Buccanneers", "Titans", "Redskins")]

new_data[, Team := plyr::mapvalues(Team, teams$Team, teams$Name)]


data_w_predictions <- data.table()
data_w_predictions[, team := new_data$Team]
data_w_predictions[, prediction_GBM := predict(GBM, new_data, type = 'prob')$played_sb]
data_w_predictions[, prediction_RF := predict(rf, new_data, type = 'prob')$played_sb]
data_w_predictions[, prediction_XGB := predict(XGB, new_data, type = 'prob')$played_sb]
data_w_predictions[, avg_prediction := (prediction_GBM + prediction_RF + prediction_XGB) / 3]
data_w_predictions <- data_w_predictions[order(-avg_prediction)]

View(data_w_predictions)


data_w_predictions$prediction_GBM <- NULL
data_w_predictions$prediction_RF <- NULL
data_w_predictions$prediction_XGB <- NULL
names(data_w_predictions)[2] <- as.character(Sys.Date())
write_csv(data_w_predictions, paste0("data/predictions-", Sys.Date(), '.csv'))



# 
# 
# 
# 
# 
# data_w_predictions <- data.table()
# data_w_predictions[, team := new_data$Team]
# data_w_predictions[, prediction_GBM := predict(GBM_w, new_data, type = 'prob')$won_sb]
# data_w_predictions[, prediction_RF := predict(rf, new_data, type = 'prob')$won_sb]
# data_w_predictions[, prediction_XGB := predict(XGB_w, new_data, type = 'prob')$won_sb]
# data_w_predictions[, avg_prediction := (prediction_GBM + prediction_RF + prediction_XGB) / 3]
# data_w_predictions <- data_w_predictions[order(-avg_prediction)]
# 
# View(data_w_predictions)
