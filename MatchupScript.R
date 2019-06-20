#Load packages
library(tidyverse)
library(nbastatR)
library(jsonlite)
library(data.table)
library(extrafont)
library(magick)

#set theme
theme_owen <- function () { 
  theme_minimal(base_size=10, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

#get players for their ids
players <- nba_players()

#Enter a player's name in order to get their ID
id <- players$idPlayer[players$namePlayer=="Stephen Curry"]

#Add seasons
years <- c("2017-18", "2018-19")

#Get Regular Season Matchup Data
get_rs_matchups <- function(year, id) {
  
  url <- paste0("https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&LeagueID=00&OffPlayerID=", id, "&Outcome=&PORound=0&PerMode=Totals&Season=", year, "&SeasonType=Regular+Season")
  
  json_data <- fromJSON(paste(readLines(url), collapse=""))
  
  df <- do.call(rbind.data.frame, json_data[["resultSets"]][["rowSet"]])
  col.names <- json_data[["resultSets"]][["headers"]][[1]]
  colnames(df) <- col.names
  df$year <- year
  return(df)
  
}

#Get data
messy.data <- map2(years, id, get_rs_matchups)

cleaned.data <- messy.data %>% rbindlist(., fill = TRUE)
df.rs <- cleaned.data
df.rs$season <- "Regular Season"


#Get Playoff Matchup Data
get_po_matchups <- function(year, id) {
  
  url <- paste0("https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&LeagueID=00&OffPlayerID=", id, "&Outcome=&PORound=0&PerMode=Totals&Season=", year, "&SeasonType=Playoffs")
  
  json_data <- fromJSON(paste(readLines(url), collapse=""))
  
  df <- do.call(rbind.data.frame, json_data[["resultSets"]][["rowSet"]])
  col.names <- json_data[["resultSets"]][["headers"]][[1]]
  colnames(df) <- col.names
  df$year <- year
  return(df)
  
}

messy.data <- map2(years, id, get_po_matchups)

cleaned.data <- messy.data %>% rbindlist(., fill = TRUE)
df.po <- cleaned.data
df.po$season <- "Playoffs"


#Combine regular season and playoff data 
df <- rbind(df.po, df.rs)

#Conver variables to correct class
df$POSS <- as.numeric(as.character(df$POSS))
df$PLAYER_PTS <- as.numeric(as.character(df$PLAYER_PTS))
df$TEAM_PTS <- as.numeric(as.character(df$TEAM_PTS))
df$TOV <- as.numeric(as.character(df$TOV))
df$DEF_PLAYER_NAME <- as.character(df$DEF_PLAYER_NAME)

#Get per 100 possessions, minium 100 possessions
df <- df %>% group_by(OFF_PLAYER_NAME, DEF_PLAYER_NAME) %>% 
  summarise(total.poss = sum(POSS), total.player_pts = sum(PLAYER_PTS), total.team_pts = sum(TEAM_PTS)) %>% 
  mutate(pts.per.100 = (total.player_pts / total.poss) *100, 
         team.pts.per.100  = (total.team_pts / total.poss) * 100) %>%
  filter(total.poss >= 100) %>% 
  select(OFF_PLAYER_NAME, DEF_PLAYER_NAME, total.poss, total.player_pts, pts.per.100, total.team_pts, team.pts.per.100) 

#Arrange data by pts per 100
df <- df %>%
  ungroup() %>% 
  arrange(pts.per.100) %>%               
  mutate(DEF_PLAYER_NAME = factor(DEF_PLAYER_NAME, unique(DEF_PLAYER_NAME))) 

#Make Chart
df %>% 
  ggplot(aes(x = DEF_PLAYER_NAME, y = pts.per.100, size = total.poss, fill = pts.per.100)) + 
  geom_point(alpha = .75, shape = 21, color = 'black') + 
  coord_flip() +
  theme_owen() +
  labs(size = "Total Possessions", 
       title = "[Player Name's] Points Per 100 Possessions When Guarded By ___", 
       subtitle = "Among players that guarded [Player Name] at least 100 possessions (2017-2019)", 
       y = "Points Per 100 Possessions", 
       x = "") +
  scale_y_continuous(limits = c(15, 50), breaks = seq(15, 50, 5)) +
  scale_size_continuous(range = c(2, 7), breaks = c(100, 150, 200, 250), limits = c(100, 270)) +
  theme(legend.position=c(0.15, 0.84), legend.background = element_rect(fill="floralwhite")) +
  scale_fill_gradient2(guide=FALSE, low = ("#0571b0"), mid = "white",
                       high = ("#ca0020"), midpoint = mean(df$pts.per.100)) + 
  theme(plot.title = element_text(face = 'bold', size = 9, hjust = 0.5))  +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5)) + 
  theme(plot.caption = element_text(face = 'italic', size = 6)) + 
  theme(plot.margin=unit(c(.75,.25,.5,0),"cm"))

