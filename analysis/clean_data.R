library(data.table)

data <- readRDS("data/scraped_stats.RDS")

### naming conventions

data[, .N, by = Team]

data[(Team == 'Los Angeles RamsLA Rams') | 
       (Team == 'St. Louis RamsSt. Louis') |
       (Team == 'Los Angeles RamsLos Angeles'), Team := 'Rams']

data[(Team == 'Los Angeles RaidersLA Raiders') | 
       (Team == 'Oakland RaidersOakland') , Team := 'Raiders']

data[(Team == 'San Diego ChargersSan Diego') | 
       (Team == 'Los Angeles ChargersLA Chargers') , Team := 'Chargers']

data[(Team == 'Houston OilersHouston') | 
       (Team == 'Tennessee TitansTennessee') |
       (Team == 'Tennessee OilersTennessee'), Team := 'Titans']

teams <- data[, .N, by = Team]
teams[, Name := c("Cardinals", "Falcons", "Ravens", "Bills", "Panthers", "Bears", "Bengals", "Browns", 
                  "Cowboys", "Broncos", "Lions", "Packers", "Texans", "Colts", "Jaguars", "Chiefs", 
                  "Chargers", "Rams", "Dolphins", "Vikings", "Patriots", "Saints", "Giants", "Jets", 
                  "Raiders", "Eagles", "Steelers", "49ers", "Seahawks", "Buccanneers", "Titans", "Redskins")]

data[, Team := plyr::mapvalues(Team, teams$Team, teams$Name)]


### feature engineering

## NAs

naniar::vis_miss(data)
View(data) 
# no defensive scoring stats before 2009
# will keep features, tree methods can deal with NAs

## zero variance

numerics <- names(data[, sapply(data, is.numeric)])
caret::nearZeroVar(data[, ..numerics], names = T)

# variances almost at 0:
# offensive blocked fg returned for TD
# offensive missed fg returned for TD
# defensive blocked fg returned for TD
# defensive missed fg returned for TD

drops <- caret::nearZeroVar(data[, ..numerics], names = T)
data[, (drops) := NULL]


saveRDS(data, "data/cleaned_stats.RDS")
