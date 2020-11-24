library(data.table)
library(caret)

data <- readRDS("analysis/namescheck.RDS")

RF <- readRDS("analysis/RFMODEL.RDS")
GBM <- readRDS("analysis/GBMMODEL.RDS")
XGB <- readRDS("analysis/XGBMODEL.RDS")

RFW <- readRDS("analysis/RFMODELW.RDS")
GBMW <- readRDS("analysis/GBMMODELW.RDS")
XGBW <- readRDS("analysis/XGBMODELW.RDS")


### PREDICT ON 2020 ###

new_data <- readRDS(paste0("data/scraped_stats_", Sys.Date(), ".RDS"))

new_data <- data.frame(new_data)
new_data <- new_data[, which(colMeans(!is.na(new_data)) > 0.5)]
new_data <- data.table(new_data)

new_data[, season := NULL]

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

keep_cols <- which(colnames(new_data) %in% data)
new_data <- new_data %>% select(keep_cols)


# rename teams


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

# make it
data_w_predictions <- data.table()
data_w_predictions[, team := new_data$Team]
data_w_predictions[, prediction_GBM := predict(GBM, new_data, type = 'prob')$played_sb]
data_w_predictions[, prediction_RF := predict(RF, new_data, type = 'prob')$played_sb]
data_w_predictions[, prediction_XGB := predict(XGB, new_data, type = 'prob')$played_sb]
data_w_predictions[, avg_prediction := (prediction_GBM + prediction_RF + prediction_XGB) / 3]
data_w_predictions <- data_w_predictions[order(-avg_prediction)]

View(data_w_predictions)

data_w_predictions$prediction_GBM <- NULL
data_w_predictions$prediction_RF <- NULL
data_w_predictions$prediction_XGB <- NULL
names(data_w_predictions)[2] <- as.character(Sys.Date())
readr::write_csv(data_w_predictions, paste0("data/predictions-play-sb-", Sys.Date(), '.csv'))

# win it
data_w_predictions <- data.table()
data_w_predictions[, team := new_data$Team]
data_w_predictions[, prediction_GBM := predict(GBMW, new_data, type = 'prob')$won_sb]
data_w_predictions[, prediction_RF := predict(RFW, new_data, type = 'prob')$won_sb]
data_w_predictions[, prediction_XGB := predict(XGBW, new_data, type = 'prob')$won_sb]
data_w_predictions[, avg_prediction := (prediction_GBM + prediction_RF + prediction_XGB) / 3]
data_w_predictions <- data_w_predictions[order(-avg_prediction)]

View(data_w_predictions)

data_w_predictions$prediction_GBM <- NULL
data_w_predictions$prediction_RF <- NULL
data_w_predictions$prediction_XGB <- NULL
names(data_w_predictions)[2] <- as.character(Sys.Date())
readr::write_csv(data_w_predictions, paste0("data/predictions-win-sb-", Sys.Date(), '.csv'))
