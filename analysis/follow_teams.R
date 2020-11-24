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

# order by mean, select top 5

playing_top <- playing %>% arrange(desc(mean)) %>% top_n(5)
winning_top <- winning %>% arrange(desc(mean)) %>% top_n(5)

playing_top$mean <- NULL
winning_top$mean <- NULL

playing_top <- playing_top %>% gather(date, predictions, '2020-11-03' : names(playing_top)[length(names(playing_top))])
winning_top <- winning_top %>% gather(date, predictions, '2020-11-03' : names(winning_top)[length(names(winning_top))])

playing_top$date <- as.Date(playing_top$date, "%Y-%m-%d")
winning_top$date <- as.Date(winning_top$date, "%Y-%m-%d")

# plot

playing_top %>% ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 0.75) + 
  geom_point(size = 2.5) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Making it to the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_making_it.png')


winning_top %>% ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Winning the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_winning_it.png')

