# try to find similar teams throughout all seasons in terms of statistics

library(data.table)
library(tidyverse)
library(caret)
library(NbClust)
library(factoextra)


clusters_by_season <- function(season_, num_clusters) {
  
  ### get data ready for clustering
  
  data <- readRDS("data/labeled_stats.RDS")
  
  # drop labels
  
  data[, play_SB := NULL]
  data[, win_SB := NULL]
  
  # to better see results, let's focus on a given season
  
  data <- data[season == season_, ]
  
  # create season_team feature
  
  data[, team := paste0(season, " ", Team)]
  data[, Team := NULL]
  data[, season := NULL]
  
  # drop team feature and have it as rowNames
  # drop NA features
  
  data <- data.frame(data)
  data <- data[, which(colMeans(!is.na(data)) > 0.99)]
  rownames(data) <- data$team
  data$team <- NULL
  
  ### clustering
  
  # number of clusters?
  
  #set.seed(20202020)
  #fviz_nbclust(scale(data), kmeans, method = "wss") +
    #labs(subtitle = "Elbow method")
  
  #set.seed(20202020)
  #fviz_nbclust(scale(data), kmeans, method = "silhouette")+
    #labs(subtitle = "Silhouette method")
  
  #set.seed(20202020)
  #fviz_nbclust(scale(data), kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
    #labs(subtitle = "Gap statistic method")
  
  
  # will try 2, 3, 4, 5 clusters
  
  set.seed(20202020)
  
  plt <- (fviz_cluster(kmeans(scale(data), centers = num_clusters, nstart = 100), 
               data = scale(data), 
               show.clust.cent = F, 
               ellipse.type = "convex",
               labelsize = 10) +
    #geom_text(aes(label = rownames(data))) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste0(num_clusters, " clusters for ", season_, " data")))
  
  
  # checking the 1st and 2nd PCAs to see which features drive the clustering
  
  pc <- data.table()
  pc[, feature := rownames(data.frame(prcomp(data, scale. = T)$rotation))]
  pc[, pc1_value := data.frame(prcomp(data, scale. = T)$rotation)$PC1]
  pc[, pc1_value_abs := abs(data.frame(prcomp(data, scale. = T)$rotation)$PC1)]
  pc[, pc2_value := data.frame(prcomp(data, scale. = T)$rotation)$PC2]
  pc[, pc2_value_abs := abs(data.frame(prcomp(data, scale. = T)$rotation)$PC2)]
  
  return(list(plot = plt, dt = pc))
  
  
}

clusters_by_season(2007, 4)$plot
clusters_by_season(2007, 4)$dt %>% View()


clusters_by_season(2007, 4)$plot +
  labs(title = "2007 clusters show Patriots far ahead along first principal component",
       subtitle = '1st PC: affected mostly by offensive scoring points and TD - Patriots record breaking season')
ggsave("plots/clusters_2007_showing_patriots_scoring_record.png", device = 'png', plot = last_plot())


clusters_by_season(2019, 4)$plot +
  labs(title = "2019 clusters show Patriots-49ers-Ravens in a group",
       subtitle = 'Due to good offensive scoring stats (points, td) and good defensive punt stats (tbs & punts forced)')
ggsave("plots/clusters_2019_pats_9ers_ravens.png", device = 'png', plot = last_plot())


# 2019 example
# pc1: negative weights: offensive scoring stats (points, TD total, rush attempts, etc..)
# pc1: positive weights: defensive scoring stats (allowed points, ypa, etc..)
# pc2: negative weights: defensive pass stats (allowed pass yds, yds / game, etc...)
# pc2: positive weights: defensive punting stats (tbs allowed, punts forced, yds 'enforced')
# Pats, 49ers, Ravens group: similar stats in terms of
# - good offensive scoring stats
# - good defensive punting (~ special teams) stats

