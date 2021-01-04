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

playing <- playing %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Steelers', 
                               'Buccanneers', 'Rams', 'Titans', 'Colts', 'Ravens', 'Bills'))

winning <- winning %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Steelers', 
                               'Buccanneers', 'Rams', 'Titans', 'Colts', 'Ravens', 'Bills'))


playing$mean <- NULL
winning$mean <- NULL

playing <- playing %>% gather(date, predictions, '2020-11-03' : names(playing)[length(names(playing))])
winning <- winning %>% gather(date, predictions, '2020-11-03' : names(winning)[length(names(winning))])

playing$date <- as.Date(playing$date, "%Y-%m-%d")
winning$date <- as.Date(winning$date, "%Y-%m-%d")

# plot

playing %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Bills', 'Titans')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Making it to the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_making_itA.png')

playing %>% filter(!team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Bills', 'Titans')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Making it to the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_making_itB.png')


winning %>% filter(team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Bills', 'Titans')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Winning the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_winning_itA.png')


winning %>% filter(!team %in% c('Seahawks', 'Chiefs', 'Saints', 'Packers', 'Bills', 'Titans')) %>% 
  ggplot(aes(date, predictions, fill = team, color = team, shape = team)) +
  geom_line(size = 2/3) + 
  geom_point(size = 2) + 
  labs(title = 'Teams with largest average SB probabilities since week 8',
       subtitle = 'Winning the big game', 
       x = 'Date', y = 'Predicted (unnormalized) probability') +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave('../plots/AAA_predictions_2020_winning_itB.png')


### SHOW ALL TEAMS ON BARCHART


playing %>% filter(date == '2021-01-04') %>% arrange(desc(predictions)) %>% 
  ggplot(aes(reorder(team, -predictions), predictions)) +
  geom_col(width = 3/4, color = 'black', fill = 'darkgreen', alpha = 2/3) +
  geom_text(aes(label = scales::percent(predictions, accuracy = 0.01)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.28)) +
  labs(title = 'Teams with highest probabilities of making it to the SB as of Week 17',
       x = NULL, y = NULL) +
  theme_bw() 


playing %>% group_by(team) %>% dplyr::summarize(mean_prob = mean(predictions)) %>% ungroup() %>% arrange(desc(mean_prob)) %>% 
  ggplot(aes(reorder(team, -mean_prob), mean_prob)) +
  geom_col(width = 3/4, color = 'black', fill = 'darkgreen', alpha = 2/3) +
  geom_text(aes(label = scales::percent(mean_prob, accuracy = 0.01)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.28)) +
  labs(title = 'Average probabilites of last 8 weeks',
       x = NULL, y = NULL) +
  theme_bw() 


playing %>% group_by(team) %>% dplyr::summarize(mean_prob = median(predictions)) %>% ungroup() %>% arrange(desc(mean_prob)) %>% 
  ggplot(aes(reorder(team, -mean_prob), mean_prob)) +
  geom_col(width = 3/4, color = 'black', fill = 'darkgreen', alpha = 2/3) +
  geom_text(aes(label = scales::percent(mean_prob, accuracy = 0.01)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.28)) +
  labs(title = 'Median probabilites of last 8 weeks',
       x = NULL, y = NULL) +
  theme_bw() 
