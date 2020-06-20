library(data.table)

# need to add binaries
# 1. won superbowl
# 2. made superbowl

## --> 2 predicion models (SB winner, SB team)

sb <- data.table(year = seq(1994, 2019, 1))

sb[, winner := c("49ers", "Cowboys", "Packers", "Broncos", "Broncos", "Rams", "Ravens", "Patriots", 
                 "Buccanneers", "Patriots", "Patriots", "Steelers", "Colts", "Giants", "Steelers", "Saints", 
                 "Packers", "Giants", "Ravens", "Seahawks", "Patriots", "Broncos", "Patriots", "Eagles", "Patriots", "Chiefs")]

sb[, loser := c("Chargers", "Steelers", "Patriots", "Packers", "Falcons", "Titans", "Giants", "Rams", 
                 "Raiders", "Panthers", "Eagles", "Seahawks", "Bears", "Patriots", "Cardinals", "Colts", 
                 "Steelers", "Patriots", "49ers", "Broncos", "Seahawks", "Panthers", "Falcons", "Patriots", "Rams", "49ers")]


data <- readRDS("data/cleaned_stats.RDS")

# label dataset

data <- merge(data, sb, by.x = c("season","Team"), by.y=c("year","winner"), all.x = T)
setnames(data,"loser", "winner")
data[, win_SB := ifelse(is.na(winner), 0, 1)]
data[, winner := NULL]

data <- merge(data, sb, by.x = c("season","Team"), by.y=c("year","loser"), all.x = T)
setnames(data,"winner", "loser")
data[, lose_SB := ifelse(is.na(loser), 0, 1)]

data[, play_SB := ifelse(lose_SB == 1 | win_SB == 1, 1, 0)]
data[, loser := NULL]
data[, lose_SB := NULL]

View(data)

saveRDS(data, "data/labeled_stats.RDS")

