#Load packages
library(tidyverse)
library(nbastatR)
library(jsonlite)
library(data.table)
library(extrafont)

#set theme
theme_owen <- function () { 
  theme_minimal(base_size=10, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}


playerName <- "James Harden"

season <- "2018-19"

get_data <- function(playerName, season, possessionMinimum) {
  
  players <- nba_players()
  id <- players$idPlayer[players$namePlayer==playerName]
  
  
  url <- paste0("https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&LeagueID=00&OffPlayerID=", id, "&Outcome=&PORound=0&PerMode=Totals&Season=", season, "&SeasonType=Regular+Season")
  
  json_data <- fromJSON(paste(readLines(url), collapse=""))
  
  df.reg <- do.call(rbind.data.frame, json_data[["resultSets"]][["rowSet"]])
  col.names <- json_data[["resultSets"]][["headers"]][[1]]
  colnames(df.reg) <- col.names
  df.reg$Season <- "Regular Season"

  url <- paste0("https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&LeagueID=00&OffPlayerID=", id, "&Outcome=&PORound=0&PerMode=Totals&Season=", season, "&SeasonType=Playoffs")
  
  json_data <- fromJSON(paste(readLines(url), collapse=""))
  
  df.po <- do.call(rbind.data.frame, json_data[["resultSets"]][["rowSet"]])
  col.names <- json_data[["resultSets"]][["headers"]][[1]]
  colnames(df.po) <- col.names
  df.po$Season <- "Playoffs"
  
  df <- rbind(df.po, df.reg)
  
  df$POSS <- as.numeric(as.character(df$POSS))
  df$PLAYER_PTS <- as.numeric(as.character(df$PLAYER_PTS))
  df$TEAM_PTS <- as.numeric(as.character(df$TEAM_PTS))
  df$TOV <- as.numeric(as.character(df$TOV))
  df$DEF_PLAYER_NAME <- as.character(df$DEF_PLAYER_NAME)
  
  df <- df %>% group_by(OFF_PLAYER_NAME, DEF_PLAYER_NAME) %>% 
    summarise(total.poss = sum(POSS), total.player_pts = sum(PLAYER_PTS), total.team_pts = sum(TEAM_PTS)) %>% 
    mutate(pts.per.100 = (total.player_pts / total.poss) *100, 
           team.pts.per.100  = (total.team_pts / total.poss) * 100) %>%
    filter(total.poss >= possessionMinimum) %>% 
    select(OFF_PLAYER_NAME, DEF_PLAYER_NAME, total.poss, total.player_pts, pts.per.100, total.team_pts, team.pts.per.100) 
  
  df <- df %>%
    ungroup() %>% 
    arrange(pts.per.100) %>%               
    mutate(DEF_PLAYER_NAME = factor(DEF_PLAYER_NAME, unique(DEF_PLAYER_NAME))) 
  
  
  return(df)
}


matchupData <- get_data("Stephen Curry", "2018-19", 100)


#Make a basic chart
matchupData %>% 
  ggplot(aes(x = DEF_PLAYER_NAME, y = pts.per.100, size = total.poss, fill = pts.per.100)) + 
  geom_point(alpha = .75, shape = 21, color = 'black') + 
  coord_flip() +
  theme_owen() +
  labs(size = "Total Possessions", 
       title = paste0(matchupData$OFF_PLAYER_NAME, "'s Points Per 100 Possessions When Guarded By ___"), 
       subtitle = paste0("Among players that guarded ", word(matchupData$OFF_PLAYER_NAME, -1), " at least [Minimum] possessions (2017-2019)"), 
       y = "Points Per 100 Possessions", 
       x = "") +
  scale_fill_gradient2(guide=FALSE, low = ("#0571b0"), mid = "white",
                       high = ("#ca0020"), midpoint = mean(matchupData$pts.per.100)) + 
  theme(plot.title = element_text(face = 'bold', size = 9, hjust = 0.5))  +
  theme(plot.subtitle = element_text(size = 8, hjust = 0.5)) + 
  theme(plot.margin=unit(c(.75,.25,.5,0),"cm")) +
  theme(legend.position=c(0.15, 0.84), legend.background = element_rect(fill="floralwhite")) 
  

