library(tidyverse)
library(stringr)
library(data.table)
library(plyr)

setwd('data/')

playing <- grep('predictions-play', list.files(), value = TRUE)
winning <- grep('predictions-win', list.files(), value = TRUE)

playing <- lapply(playing, read_csv)
winning <- lapply(winning, read_csv)

playing <- Reduce(function(x, y) merge(x, y, all=TRUE), playing)
winning <- Reduce(function(x, y) merge(x, y, all=TRUE), winning)

# calc average (top teams throughout the weeks)

playing$mean <- playing %>% select_if(is.numeric) %>% rowMeans()
winning$mean <- winning %>% select_if(is.numeric) %>% rowMeans()

View(playing)
View(winning)

# order by mean, select top 5

#playing_top <- playing %>% arrange(desc(mean)) %>% top_n(5)
#winning_top <- winning %>% arrange(desc(mean)) %>% top_n(5)

playing <- playing %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Cardinals', 'Steelers', 
                               'Buccanneers', 'Rams', 'Titans', 'Colts', 'Ravens', 'Bills'))

winning <- winning %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Cardinals', 'Steelers', 
                               'Buccanneers', 'Rams', 'Titans', 'Colts', 'Ravens', 'Bills'))


playing$mean <- NULL
winning$mean <- NULL

playing <- playing %>% gather(date, predictions, '2020-11-03' : names(playing)[length(names(playing))])
winning <- winning %>% gather(date, predictions, '2020-11-03' : names(winning)[length(names(winning))])

playing$date <- as.Date(playing$date, "%Y-%m-%d")
winning$date <- as.Date(winning$date, "%Y-%m-%d")

# plot

playing %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Cardinals', 'Steelers')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Making it to the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_making_itA.png')

playing %>% filter(!team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Cardinals', 'Steelers')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Making it to the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_making_itB.png')


winning %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Cardinals', 'Steelers')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Winning the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_winning_itA.png')


winning %>% filter(!team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Cardinals', 'Steelers')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Winning the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_winning_itB.png')
