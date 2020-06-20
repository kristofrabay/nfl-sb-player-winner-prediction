library(data.table)
library(rvest)
library(pbapply)
library(dplyr)
library(tidyr)

# 1994 is first year with salary cap
years <- seq(2019, 1994, -1)

# P (passing), R (rushing), KR (kick return), PR (punt return), U (punting), S (scoring), W (downs)
categories <- c("P", "R", "KR", "PR", "U", "S", "W")

# O (Offense), D (Defense)
groups <- c("O", "D")


# build data for 2019, then generalize for other years

links <- list()

for (category in categories) {
    for (group in groups) {
      link <- paste0("https://www.footballdb.com/stats/teamstat.html?lg=NFL&yr=2019&type=reg&cat=", category, "&group=", group, "&conf=")
      links <- unlist(append(links, link))
    }
}


for (link in links) {
  
  if (link %like% "cat=P&group=O") {
    
    print('passing offense')
    
    po <- read_html(link) %>% html_table()
    po <- po[[1]]
    colnames(po) <- paste("off_pass", colnames(po), sep = "_")
    colnames(po)[1] <- 'Team'
    
    
  } else if (link %like% "cat=P&group=D") {
    
    print('passing defense')
    
    pd <- read_html(link) %>% html_table()
    pd <- pd[[1]]
    colnames(pd) <- paste("def_pass", colnames(pd), sep = "_")
    colnames(pd)[1] <- 'Team'
    
    
  } else if (link %like% "cat=R&group=O") {
    
    print('rushing offense')
    
    ro <- read_html(link) %>% html_table()
    ro <- ro[[1]]
    colnames(ro) <- paste("off_rush", colnames(ro), sep = "_")
    colnames(ro)[1] <- 'Team'
    
    
  } else if (link %like% "cat=R&group=D") {
    
    print('rushing defense')
    
    rd <- read_html(link) %>% html_table()
    rd <- rd[[1]]
    colnames(rd) <- paste("def_rush", colnames(rd), sep = "_")
    colnames(rd)[1] <- 'Team'
    
    
  } else if (link %like% "cat=KR&group=O") {
    
    print('kr offense')
    
    kro <- read_html(link) %>% html_table()
    kro <- kro[[1]]
    colnames(kro) <- paste("off_kick_ret", colnames(kro), sep = "_")
    colnames(kro)[1] <- 'Team'
    
    
  } else if (link %like% "cat=KR&group=D") {
    
    print('kr defense')
    
    krd <- read_html(link) %>% html_table()
    krd <- krd[[1]]
    colnames(krd) <- paste("def_kick_ret", colnames(krd), sep = "_")
    colnames(krd)[1] <- 'Team'
    
    
  } else if (link %like% "cat=PR&group=O") {
    
    print('pr offense')
    
    pro <- read_html(link) %>% html_table()
    pro <- pro[[1]]
    colnames(pro) <- paste("off_punt_ret", colnames(pro), sep = "_")
    colnames(pro)[1] <- 'Team'
    
    
  } else if (link %like% "cat=PR&group=D") {
    
    print('pr defense')
    
    prd <- read_html(link) %>% html_table()
    prd <- prd[[1]]
    colnames(prd) <- paste("def_punt_ret", colnames(prd), sep = "_")
    colnames(prd)[1] <- 'Team'
    
    
  } else if (link %like% "cat=U&group=O") {
    
    print('punting offense')
    
    puo <- read_html(link) %>% html_table()
    puo <- puo[[1]]
    colnames(puo) <- paste("off_punting", colnames(puo), sep = "_")
    colnames(puo)[1] <- 'Team'
    
    
  } else if (link %like% "cat=U&group=D") {
    
    print('punting defense')
    
    pud <- read_html(link) %>% html_table()
    pud <- pud[[1]]
    colnames(pud) <- paste("def_punting", colnames(pud), sep = "_")
    colnames(pud)[1] <- 'Team'
    
    
  } else if (link %like% "cat=S&group=O") {
    
    print('scoring offense')
    
    so <- read_html(link) %>% html_table()
    so <- so[[1]]
    so <- so[-1,]
    colnames(so) <- c('Team', 'TD_total', 
                      'TD_rush', 'TD_pass', 'TD_kr', 'TD_pr', 'TD_ir', 'TD_fr', 'TD_blocked_fg', 'TD_blocked_punt', 'TD_missed_fg_ret',
                      'kick_pat', 'kick_fg', 'conversions', 'safeties', 'points')
    colnames(so) <- paste("off_scoring", colnames(so), sep = "_")
    colnames(so)[1] <- 'Team'
    
    
  } else if (link %like% "cat=S&group=D") {
    
    print('scoring defense')
    
    sd <- read_html(link) %>% html_table()
    sd <- sd[[1]]
    sd <- sd[-1,]
    colnames(sd) <- c('Team', 'TD_total', 
                      'TD_rush', 'TD_pass', 'TD_kr', 'TD_pr', 'TD_ir', 'TD_fr', 'TD_blocked_fg', 'TD_blocked_punt', 'TD_missed_fg_ret',
                      'kick_pat', 'kick_fg', 'conversions', 'safeties', 'points')
    colnames(sd) <- paste("def_scoring", colnames(sd), sep = "_")
    colnames(sd)[1] <- 'Team'
    
    
  } else if (link %like% "cat=W&group=O") {
    
    print('downs offense')
    
    do <- read_html(link) %>% html_table()
    do <- do[[1]]
    do <- do[-1,]
    colnames(do) <- c('Team', 'Gms', 
                      'first_downs_rush', 'first_downs_pass', 'first_downs_pen', 'first_downs_tot',
                      'third_downs_att', 'third_downs_made', 'third_downs_pct',
                      'fourth_downs_att', 'fourth_downs_made', 'fourth_downs_pct')
    colnames(do) <- paste("off_downs", colnames(do), sep = "_")
    do <- do[!(do$off_downs_Team == "League Totals"), ]
    colnames(do)[1] <- 'Team'
    
    
  } else if (link %like% "cat=W&group=D") {
    
    print('downs defense')
    
    dd <- read_html(link) %>% html_table()
    dd <- dd[[1]]
    dd <- dd[-1,]
    colnames(dd) <- c('Team', 'Gms', 
                      'first_downs_rush', 'first_downs_pass', 'first_downs_pen', 'first_downs_tot',
                      'third_downs_att', 'third_downs_made', 'third_downs_pct',
                      'fourth_downs_att', 'fourth_downs_made', 'fourth_downs_pct')
    colnames(dd) <- paste("def_downs", colnames(dd), sep = "_")
    dd <- dd[!(dd$def_downs_Team == "League Totals"), ]
    colnames(dd)[1] <- 'Team'
    
  }
  
}



data <- Reduce(merge, list(po, ro, kro, pro, puo, so, do,
                   pd, rd, krd, prd, pud, sd, dd))

rm(po, ro, kro, pro, puo, so, do,
   pd, rd, krd, prd, pud, sd, dd)

View(data)

# clean up feature types


str(data)
names(data[, sapply(data, is.character)])

data <- separate(data, off_scoring_kick_pat, c("off_scoring_kick_pat_made", "off_scoring_kick_pat_att"), sep = "/")
data <- separate(data, off_scoring_kick_fg, c("off_scoring_kick_fg_made", "off_scoring_kick_fg_att"), sep = "/")
data <- separate(data, def_scoring_kick_pat, c("def_scoring_kick_pat_made", "def_scoring_kick_pat_att"), sep = "/")
data <- separate(data, def_scoring_kick_fg, c("def_scoring_kick_fg_made", "def_scoring_kick_fg_att"), sep = "/")


data[ , grepl( "off_scoring" , names(data)) ] <- sapply(data[ , grepl( "off_scoring" , names(data)) ], as.numeric)
data[ , grepl( "off_downs" , names(data)) ] <- sapply(data[ , grepl( "off_downs" , names(data)) ], as.numeric)
data[ , grepl( "def_scoring" , names(data)) ] <- sapply(data[ , grepl( "def_scoring" , names(data)) ], as.numeric)
data[ , grepl( "def_downs" , names(data)) ] <- sapply(data[ , grepl( "def_downs" , names(data)) ], as.numeric)


names(data[, sapply(data, is.character)]) 
# longest returns sometimes end up in TDs
# they get labeled as i.e.: 78t - meaning longest return was 78 yards and ended up in TD
# let's drop the T and just keep the yardage

data[ , grepl( "ret_Lg" , names(data)) ]

data$off_kick_ret_Lg <- gsub("t", "", data$off_kick_ret_Lg)
data$off_punt_ret_Lg <- gsub("t", "", data$off_punt_ret_Lg)
data$def_kick_ret_Lg <- gsub("t", "", data$def_kick_ret_Lg)
data$def_punt_ret_Lg <- gsub("t", "", data$def_punt_ret_Lg)

data[ , grepl( "ret_Lg" , names(data)) ] <- sapply(data[ , grepl( "ret_Lg" , names(data)) ], as.numeric)



# check features that can be dropped 

data[ , grepl( "Gms" , names(data)) ] <- NULL

View(data)

saveRDS(data, "data/2019_data.RDS")

numerics <- names(data[, sapply(data, is.numeric)])
corrplot::corrplot(cor(data[numerics]), 
                   method = "number",
                   tl.cex = 0.66, 
                   tl.col = "black",
                   diag = T, 
                   cl.cex = 0.75, 
                   number.cex = 2/3)
