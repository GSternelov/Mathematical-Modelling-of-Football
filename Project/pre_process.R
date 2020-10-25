# Load packages
library(jsonlite) # read json files
library(data.table) # pre-process data
library(ggplot2) # plotting
library(viridis) # plotting
library(ggrepel) # plotting


# pitch plot theme
{
  ggTheme = theme(panel.background = element_rect(fill = "white"), panel.grid = element_blank(),
                  axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
                  legend.position = "bottom", legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  ## define the circle function (from https://github.com/FCrSTATS/SBpitch/blob/master/R/pitchCreate.R)
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  center_circle <- circleFun(c(52.5,32.5),18.3,npoints = 100)
  
  ggPitch = ggplot() +
    xlim(c(-2, 107)) + ylim(c(66, -1)) +
    annotate("rect", xmin = 0, xmax = 105, ymin = 0, ymax = 65, fill = "white", col = "black") + 
    annotate("rect", xmin = 0, xmax = 16.5, ymin = 12.35, ymax = 52.65, fill = "white", col = "black") + 
    annotate("rect", xmin = 105, xmax = 88.5, ymin = 12.35, ymax = 52.65, fill = "white", col = "black") + 
    
    annotate("rect", xmin = 0, xmax = 5.5, ymin = 23.5, ymax = 41.65, fill = "white", col = "black") + 
    annotate("rect", xmin = 105, xmax = 99.5, ymin = 23.5, ymax = 41.65, fill = "white", col = "black") + 
    
    annotate("point", x = c(11, 52.5, 94), y = c(32.5, 32.5, 32.5), size = 1) + 
    
    geom_path(data=center_circle, aes(x=x,y=y)) +
    
    annotate("line", x = c(0, 0), y = c(28.84, 36.16), size = 1) + 
    annotate("line", x = c(105, 105), y = c(28.84, 36.16), size = 1) + 
    
    annotate("segment", x = 52.5, xend = 52.5, y = 0, yend = 65) +
    ggTheme
}

# Function to round values
mround <- function(x,base){ 
  base*round(x/base) 
}

# premier league data, 17/18
load("Project/wy_pl.rda")
# Player data
players <- data.table(fromJSON(txt = "Project/players.json"))


games <- fromJSON(txt = "Project/matches_England.json", simplifyDataFrame = FALSE)
gamesDT = list()
playerMins = list()
for(i in 1:length(games)){
  if(games[[i]]$teamsData[[1]]$side == "home"){
    homeID = games[[i]]$teamsData[[1]]$teamId
    awayID = games[[i]]$teamsData[[2]]$teamId
    
    homePlayers = rbindlist(games[[i]]$teamsData[[1]]$formation$lineup)
    if(games[[i]]$teamsData[[1]]$formation$substitutions != "null"){
      homeSubs = rbindlist(games[[i]]$teamsData[[1]]$formation$substitutions)
      
      homePlayers = merge(homePlayers[, .(playerId)],
                          homeSubs[, .(playerId = playerOut, mins = minute)], by = "playerId", all = TRUE)
      homePlayers[is.na(mins) | mins > 90, mins := 90]
      homePlayers = rbind(homePlayers, homeSubs[minute < 90, .(playerId = playerIn, mins = 90 - minute)])
    }else{
      homePlayers = awayPlayers[, .(playerId, mins = 90)]
    }
    
    awayPlayers = rbindlist(games[[i]]$teamsData[[2]]$formation$lineup)
    if(games[[i]]$teamsData[[2]]$formation$substitutions != "null"){
      awaySubs = rbindlist(games[[i]]$teamsData[[2]]$formation$substitutions)
      
      awayPlayers = merge(awayPlayers[, .(playerId)],
                          awaySubs[, .(playerId = playerOut, mins = minute)], by = "playerId", all = TRUE)
      awayPlayers[is.na(mins) | mins > 90, mins := 90]
      awayPlayers = rbind(awayPlayers, awaySubs[minute < 90, .(playerId = playerIn, mins = 90 - minute)])
    }else{
      awayPlayers = awayPlayers[, .(playerId, mins = 90)]
    }
  }else{
    homeID = games[[i]]$teamsData[[2]]$teamId
    awayID = games[[i]]$teamsData[[1]]$teamId
    
    homePlayers = rbindlist(games[[i]]$teamsData[[2]]$formation$lineup)
    if(games[[i]]$teamsData[[2]]$formation$substitutions != "null"){
      homeSubs = rbindlist(games[[i]]$teamsData[[2]]$formation$substitutions)
      
      homePlayers = merge(homePlayers[, .(playerId)],
                          homeSubs[, .(playerId = playerOut, mins = minute)], by = "playerId", all = TRUE)
      homePlayers[is.na(mins) | mins > 90, mins := 90]
      homePlayers = rbind(homePlayers, homeSubs[minute < 90, .(playerId = playerIn, mins = 90 - minute)])
    }else{
      homePlayers = awayPlayers[, .(playerId, mins = 90)]
    }
    awayPlayers = rbindlist(games[[i]]$teamsData[[1]]$formation$lineup)
    if(games[[i]]$teamsData[[1]]$formation$substitutions != "null"){
      awaySubs = rbindlist(games[[i]]$teamsData[[1]]$formation$substitutions)
      
      awayPlayers = merge(awayPlayers[, .(playerId)],
                          awaySubs[, .(playerId = playerOut, mins = minute)], by = "playerId", all = TRUE)
      awayPlayers[is.na(mins) | mins > 90, mins := 90]
      awayPlayers = rbind(awayPlayers, awaySubs[minute < 90, .(playerId = playerIn, mins = 90 - minute)])
    }else{
      awayPlayers = awayPlayers[, .(playerId, mins = 90)]
    }
    
  }
  Teams = games[[i]]$label
  Teams = strsplit(strsplit(Teams, ",")[[1]][1], " - ")[[1]]
  gameID = games[[i]]$wyId
  gamesDT[[i]] = data.table(homeID, awayID, homeTeam = Teams[1], awayTeam = Teams[2], gameID,
                            Date = games[[i]]$dateutc, gw = games[[i]]$gameweek)
  
  homePlayers[, teamId := homeID]
  homePlayers[, gameID := gameID]
  
  awayPlayers[, teamId := awayID]
  awayPlayers[, gameID := gameID]
  
  playerMins[[i]] = rbind(homePlayers, awayPlayers)
}
gamesDT = rbindlist(gamesDT)
playerMins = rbindlist(playerMins)
playerMins = merge(playerMins, players[, .(playerId = wyId, shortName)], by = "playerId")
playerMins = playerMins[, .(totMins = sum(mins), tot90s = sum(mins) / 90), .(playerId, teamId, shortName)]
playerMins[, totMins2 := sum(totMins), playerId]
playerMins[, tot90s2 := sum(tot90s), playerId]