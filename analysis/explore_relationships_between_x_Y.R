library(data.table)
library(tidyverse)

data <- readRDS("data/labeled_stats.RDS")

data <- data.frame(data)
data <- data[, which(colMeans(!is.na(data)) > 0.5)]
data <- data.table(data)

numerics <- names(select_if(train, is.numeric))

for (column in numerics) {
  
  plt <- ggplot(data, aes(play_SB, data[[column]])) +
    geom_point() +
    coord_flip() +
    labs(title = paste0(column, " - play_SB"),
         y = paste0(column)) +
    theme_bw()
  
  ggsave(paste0("plots/relationships_loop/", column, " - play_SB.png"), device = 'png', plot = plt)
  
  #Sys.sleep(0.5)
  
}


