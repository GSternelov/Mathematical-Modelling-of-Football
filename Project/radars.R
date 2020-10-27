source("Project/pre_process.R")

# import data
load("Project/files/player_xG.rda")
load("Project/files/player_xA.rda")
load("Project/files/playerImpact.rda")

# Shots data
player_xG = merge(player_xG, playerMins[, .(shortName, playerId, tot90s2)], by = c("shortName", "playerId"))
player_xG[, temp := 1:.N, shortName]
player_xG = player_xG[temp == 1]
player_xG[, xG90 := xG / tot90s2]
player_xG[, shots90 := shots / tot90s2]

# Min 8 90s and 10 shots
player_xG = player_xG[tot90s2 >= 8 & shots >= 10]

player_xG[, xG90_perc := ecdf(xG90)(xG90) * 100]
player_xG[, xG_shot_perc := ecdf(xG_shot)(xG_shot) * 100]
player_xG[, shots90_perc := ecdf(shots90)(shots90) * 100]

# assists data
player_xA = merge(player_xA, playerMins[, .(shortName, playerId, tot90s2)], by = c("shortName", "playerId"))
player_xA[, temp := 1:.N, shortName]
player_xA = player_xA[temp == 1]
player_xA[, xA90 := xA / tot90s2]

# Min 8 90s
player_xA = player_xA[tot90s2 >= 8]
player_xA[, xA90_perc := ecdf(xA90)(xA90) * 100]

# pass impact data
playerImpact = merge(playerImpact, playerMins[, .(shortName, playerId, tot90s2)], by = c("shortName", "playerId"))
playerImpact[, temp := 1:.N, shortName]
playerImpact = playerImpact[temp == 1]
playerImpact[, aImpact90 := aImpact / tot90s2]

# Min 8 90s
playerImpact = playerImpact[tot90s2 >= 8]
playerImpact[, aImpact90_perc := ecdf(aImpact90)(aImpact90) * 100]

# data table with player info
  # Find players that have values in all tables
radarPlayers = playerImpact[playerId %in% player_xG[playerId %in% player_xA[, playerId], playerId], playerId]

radarData = cbind(player_xG[playerId %in% radarPlayers,
                            .(playerId, shortName, currentTeamId, xG90_perc, xG_shot_perc, shots90_perc)][order(playerId)],
                  player_xA[playerId %in% radarPlayers, .(playerId, xA90_perc)][order(playerId), .(xA90_perc)],
                  playerImpact[playerId %in% radarPlayers, .(playerId, aImpact90_perc)][order(playerId), .(aImpact90_perc)])

# For presentation
write.csv(radarData[, .(playerId, shortName, currentTeamId,
                        xA90_perc = xA90_perc/100, aImpact90_perc = aImpact90_perc/100)],
          file = 'passMetrics.csv', row.names = FALSE)

# For report
save(radarData, file = "Project/files/radarData.rda")

# Make a radar plot

circle1 <- circleFun(c(0,0),200,npoints = 100)
circle2 <- circleFun(c(0,0),150,npoints = 100)
circle3 <- circleFun(c(0,0),100,npoints = 100)
circle4 <- circleFun(c(0,0),50,npoints = 100)

angle_spilt <- (2*pi) / (5)
angle_spilt_seq <- seq(0,(2*pi),angle_spilt)

TitlePositioning <- data.frame(title = character(), x = numeric(), y = numeric(), stringsAsFactors = F)

## create plot background construction data  
k = 1
for (i in 4:8) {
  k = k+1
  radians_for_segment <- angle_spilt_seq[k]
  
  x <- 117 * cos(radians_for_segment)
  y <- 117 * sin(radians_for_segment)
  title <- colnames(radarData)[i]
  temp <- data.frame(title = title, x = x, y = y, stringsAsFactors = F)
  TitlePositioning <- rbind(temp, TitlePositioning)
}

polydata <- data.frame(Player = character(), value = numeric(), radians = numeric(),
                       x = numeric(), y = numeric(), stringsAsFactors = F)

## create polygon data for the players 
k = 1
for (i in 4:8) {
  k = k+1
  for (p in 1:nrow(radarData)) {
    site2calc <- as.numeric(radarData[p, i, with = FALSE])
    angle_multiplier <- if(i < ncol(radarData)){i}else{1}
    radians_for_segment <- angle_spilt_seq[k]
    x <- site2calc * cos(radians_for_segment)
    y <- site2calc * sin(radians_for_segment)
    Player <- radarData$shortName[p]
    temp <- data.frame(Player = Player, value = site2calc, radians = radians_for_segment, x = x, y = y, stringsAsFactors = F)
    polydata <- rbind(temp, polydata)
  }
}

polydata = data.table(polydata)

RadarFunc = function(P, C){
  p = ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) +
    geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + 
    geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + 
    geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + 
    geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + 
    labs(x = NULL, y = NULL, title = polydata[Player == P, unique(Player)]) +
    theme(panel.grid.major.x = element_line(color = "transparent"), axis.text=element_blank(),
          plot.title = element_text(hjust=0.5)) +
    annotate("text", x = TitlePositioning$x , y = TitlePositioning$y, label = TitlePositioning$title, size= 3.5) +
    geom_polygon(data = polydata[Player == P], aes(x = x, y = y, fill = Player, color = Player), fill = C, col = C, size = 1)
  return(p)
}


radarData[, totRank := sum(xG90_perc, xG_shot_perc, shots90_perc, xA90_perc, aImpact90_perc), playerId]
buildUpTop = radarData[order(-aImpact90_perc), shortName][1:5]
xGtop = radarData[order(-xG90_perc), shortName][1:5]
shotsTop = radarData[order(-shots90_perc), shortName][1:5]
xAtop = radarData[order(-xA90_perc), shortName][1:5]
lpool = radarData[currentTeamId == 1612][order(totRank), shortName]

playerRadarPassIp = list()
playerRadarxG = list()
playerRadarxA = list()
playerRadarShots = list()
playerRadarlpool = list()

k = 0
for(i in 1:5){
  k = k+1
  playerRadarPassIp[[k]] = RadarFunc(P = buildUpTop[i], C = viridis(n = max(c(5, k)), end = 0.9, alpha = 0.5, option = "B")[k])
  playerRadarxG[[k]] = RadarFunc(P = xGtop[i], C = viridis(n = max(c(5, k)), end = 0.9, alpha = 0.5, option = "B")[k])
  playerRadarxA[[k]] = RadarFunc(P = xAtop[i], C = viridis(n = max(c(5, k)), end = 0.9, alpha = 0.5, option = "B")[k])
  playerRadarShots[[k]] = RadarFunc(P = shotsTop[i], C = viridis(n = max(c(5, k)), end = 0.9, alpha = 0.5, option = "B")[k])
}

grid.arrange(grobs = playerRadarPassIp, ncol = 3,
             top=textGrob("Top 5 - Pass impact", gp = gpar(fontsize = 14), hjust=0.75) )

grid.arrange(grobs = playerRadarxG, ncol = 3,
             top=textGrob("Top 5 - xG/90", gp = gpar(fontsize = 14), hjust=0.75) )

grid.arrange(grobs = playerRadarShots, ncol = 3,
             top=textGrob("Top 5 - Shots/90", gp = gpar(fontsize = 14), hjust=0.75) )

grid.arrange(grobs = playerRadarxA, ncol = 3,
             top=textGrob("Top 5 - xA/90", gp = gpar(fontsize = 14), hjust=0.75) )

k = 0
for(i in 1:length(lpool)){
  k = k+1
  playerRadarlpool[[k]] = RadarFunc(P = lpool[i], C = "#C8102E")
  }

grid.arrange(grobs = playerRadarlpool, ncol = 4,
             top=textGrob("Liverpool player radars", gp = gpar(fontsize = 14), hjust=0.75) )



