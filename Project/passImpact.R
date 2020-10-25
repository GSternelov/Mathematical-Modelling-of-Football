# load packages, pitch plot and do some pre-processing of the data
source("Project/pre_process.R")

# Idea
  # Looking at the value of a pass 
    # where the ball was played from / where the ball was 5 seconds before a shot was taken
    # => value of a zone

# Starting with filtering out all shots and find the ones that was preceded by a pass

shotsDT = pl[eventName == "Shot"]
# Get the coordinates to columns
pass_coord = data.table(t(apply(shotsDT, 1, function(x) unlist(x$position))))
shotsDT[, names(pass_coord) := pass_coord]
# Normalize pitch coordinates according to standard(?) dimension 105*65m  
shotsDT[, y1 := y1 / 100 * 65]
shotsDT[, y2 := y2 / 100 * 65]
shotsDT[, x1 := x1 / 100 * 105]
shotsDT[, x2 := x2 / 100 * 105]

# Look at tags to get more information and outcome
shotTags = data.table(apply(shotsDT, 1, function(x) unlist(x$tags)))
shotTags[, id := 1:.N]
# 101 == Goal
# 401 == Left foot
# 402 == Right foot
# 403 == head/body
# 201 == Oppurtunity
# 1201-1223 == Position in/outside goal
# 1801/1802 == accurate/not accurate (on target or not, I assume)
# Foot or head/body the only one of real value I think...
shotTags[is.list(V1), outcome := any(unlist(V1) %in% 101) , id] 
shotTags[is.list(V1), head := any(unlist(V1) %in% 403) , id] 

shotsDT[, head := shotTags[, head]]
shotsDT[, outcome := shotTags[, outcome]]

# plotting sequence before shots in order to understand the data...
pl[id == shotsDT[4, id -1 ] | id == shotsDT[4, id ]]

temp = 42

tempDT = pl[eventSec <= shotsDT[temp, eventSec] & eventSec >= shotsDT[temp, eventSec -10] & 
              matchId == shotsDT[temp, matchId] & matchPeriod == shotsDT[temp, matchPeriod]]
temp_coord = data.table(t(apply(tempDT, 1, function(x) unlist(x$position))))
tempDT[, names(temp_coord) := temp_coord]
# Normalize pitch coordinates according to standard(?) dimension 105*65m  
tempDT[, y1 := y1 / 100 * 65]
tempDT[, y2 := y2 / 100 * 65]
tempDT[, x1 := x1 / 100 * 105]
tempDT[, x2 := x2 / 100 * 105]

# tags
tempTags = data.table(apply(tempDT, 1, function(x) unlist(x$tags)))
tempTags[, id := 1:.N]
tempTags[is.list(V1), head := any(unlist(V1) %in% 403) , id] 
tempDT[, head := tempTags[, head]]

ggPitch +
  geom_segment(data = tempDT[teamId == tempDT[eventName == "Shot", unique(teamId)]],
               aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.02, "npc")), size = 1) +
  geom_label(data = tempDT[teamId == tempDT[eventName == "Shot", unique(teamId)]],
             aes(x = x1, y = y1, label = eventName))


# Probability for where the ball will be passed (learned from data)
# Probability for the pass to be accurate (from pass model)
# Probability to score goal in zone (xg model)
# Probability to make an assist to a shot in zone (learned from data)
  # or
# Probability to be involved in a high xG chance in 5-10 seconds

# From zone z3, player will probably make a pass to zone z4 (p z4|z3 = 0.7)
# probability for pass to be accurate = 0.75
# probabolity to score in zone = 0.01
# Probability to make an assist to a shot in zone = 0.02

  # Goal creation value (xA): 0.75 * 0.01
  # Build-up value (pass impact): 0.75 * 0.02



## Check previous action before a shot
pl[pl[, which(eventName == "Shot") - 1], .N, eventName]
# Want prervious action by same team
# Difficult to avoid a for loop I think...
shotsInd = pl[, which(eventName == "Shot")]

shotAction = list()
k = 0
for(i in shotsInd){
  k = k+1
  # Looking at most 5 actions backward, and then only actions from same team
  temp =  head(pl[i - (1:5)][teamId == shotsDT[k, teamId]], 1)
  temp[, shotInd := i]
  shotAction[[k]] = temp
}
shotAction = rbindlist(shotAction)

shotAction[, .N, eventName]
# 3800 passes leading to shots
  # 8451 shots, but only 8445 shot actions (probably since some did not have any actions frame same team in 5 last actions)
which((shotsInd %in% shotAction[, shotInd]) == FALSE) # shots without previous action from same team

shotActionPass = shotAction[eventName == "Pass"]

pass_coord = data.table(t(apply(shotActionPass, 1, function(x) unlist(x$position))))
shotActionPass[, names(pass_coord) := pass_coord]
# Normalize pitch coordinates according to standard(?) dimension 105*65m  
shotActionPass[, y1 := y1 / 100 * 65]
shotActionPass[, y2 := y2 / 100 * 65]
shotActionPass[, x1 := x1 / 100 * 105]
shotActionPass[, x2 := x2 / 100 * 105]


# Pass location prob for a zone
# Filter out passes
all_pass = pl[eventName == "Pass"]
# Get the coordinates to columns
pass_coord = data.table(t(apply(all_pass, 1, function(x) unlist(x$position))))
all_pass[, names(pass_coord) := pass_coord]
# Normalize pitch coordinates according to standard(?) dimension 105*65m  
all_pass[, y1 := y1 / 100 * 65]
all_pass[, y2 := y2 / 100 * 65]
all_pass[, x1 := x1 / 100 * 105]
all_pass[, x2 := x2 / 100 * 105]

all_pass[, GridX := cut(x = x1, breaks = 5, labels = FALSE)]
all_pass[, GridY := cut(x = y1, breaks = 5, labels = FALSE)]
all_pass[, GridX2 := cut(x = x2, breaks = 5, labels = FALSE)]
all_pass[, GridY2 := cut(x = y2, breaks = 5, labels = FALSE)]
all_pass[, startMinX := (GridX-1) / 5 * 105]
all_pass[, startMaxX := GridX / 5 * 105]
all_pass[, startMinY := (GridY-1) / 5 * 65]
all_pass[, startMaxY := GridY / 5 * 65]

ggPitch + 
  geom_rect(data = all_pass[GridX == 4 & GridY == 2, .N, .(GridX2, GridY2)],
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = N)) +
  scale_fill_viridis(alpha = 0.75) + 
  annotate("rect", xmin = all_pass[GridX == 4 & GridY == 2, min(startMinX)], 
           xmax = all_pass[GridX == 4 & GridY == 2, max(startMaxX)],
           ymin = all_pass[GridX == 4 & GridY == 2, min(startMinY)],
           ymax = all_pass[GridX == 4 & GridY == 2, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of passes from red zone")


# Passes per zone
passesZone = all_pass[, .N, .(GridX, GridY)]

# Passes leading to shots
# 5x5 grid
shotActionPass[, GridX := cut(x = x1, breaks = 5, labels = FALSE)]
shotActionPass[, GridY := cut(x = y1, breaks = 5, labels = FALSE)]
shotActionPass2 = shotActionPass[, .(Count = .N), .(GridX, GridY)]
shotActionPass2[, startMinX := (GridX-1) / 5 * 105]
shotActionPass2[, startMaxX := GridX / 5 * 105]
shotActionPass2[, startMinY := (GridY-1) / 5 * 65]
shotActionPass2[, startMaxY := GridY / 5 * 65]

# Heatmap of pass locations for passes to shots
ggPitch + 
  geom_rect(data = shotActionPass2,
            aes(xmin = startMinX, xmax = startMaxX,
                ymin = startMinY, ymax = startMaxY,
                fill = Count)) +
  scale_fill_viridis(alpha = 0.75) +
  labs(title = "Heatmap of pass locations for passes to shots")

# Proportion of passes that becomes assists to a shot
shotActionPass2 = merge(shotActionPass2, passesZone, by = c("GridX", "GridY"))
shotActionPass2[, Prob := Count / N]
ggPitch + 
  geom_rect(data = shotActionPass2,
            aes(xmin = startMinX, xmax = startMaxX,
                ymin = startMinY, ymax = startMaxY,
                fill = Prob)) +
  scale_fill_viridis(alpha = 0.75, breaks = c(0, 0.125, 0.25), limits = c(0, 0.25)) +
  labs(title = "Proportion of passes that becomes assists to a shot")

save(shotActionPass2, file = "shotActionPass2.rda")


# Probability to assist a goal
# Probability that the pass will be accurate
  # => Chance creation value

# Probability to move to ball to a valuable area
# Probability that pass will be accurate
  # => Build-up value

# So far, I'm only looking at the latter (build-up value)


# load pass evaluation model
load("Project//passmodel.rda")

  # Create featurs used by model
all_pass[, distOrigin := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
all_pass[, distEnd := round(sqrt((x2 - 105)^2 + (y2- 32.5)^2), 2)]

# Angle to goal
all_pass[, angleOrigin := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
all_pass[angleOrigin < 0, angleOrigin := angleOrigin + pi]
all_pass[, angleOrigin := angleOrigin * 180 / pi]

all_pass[, angleEnd := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 32.5)^2 - (7.32/2)^2))]
all_pass[angleEnd < 0, angleEnd := angleOrigin + pi]
all_pass[, angleEnd := angleEnd * 180 / pi]

all_pass[, inBoxOrigin := ifelse(x1 >= 88.5 & y1 >= 12.35 & y1 <= 52.65, TRUE, FALSE)]
all_pass[, inBoxEnd := ifelse(x2 >= 88.5 & y2 >= 12.35 & y2 <= 52.65, TRUE, FALSE)]

# Angle to center of goal
all_pass[, angleCenterOrigin := atan2((105 - x1) - 0, abs(y1 - 32.5) - 0) * 180 / pi]
all_pass[, angleCenterEnd := atan2((105 - x2) - 0, abs(y2 - 32.5) - 0) * 180 / pi]

all_pass[, passLength := abs(distEnd - distOrigin)]
all_pass[, angleCenterDiff := angleCenterEnd - angleCenterOrigin]

# Distance to center
all_pass[, centerDist := abs(y1 - 32.5)]
all_pass[, centerDistEnd := abs(y2 - 32.5)]

# Pass types
all_pass[subEventName %in% c("Simple pass", "Smart pass",  "High pass", "Launch"), passType := "passFoot"]
all_pass[subEventName == "Head pass", passType := "passHead"]
all_pass[subEventName == "Cross", passType := "cross"]
all_pass[subEventName == "Hand pass", passType := "passHand"]

# Make prediction
all_pass[, passProb := predict(logit_features, newdata = all_pass, type = "response")]

# Look at tags to determine outcome
# 1801 == accurate
pass_tags = data.table(apply(all_pass, 1, function(x) unlist(x$tags)))
pass_tags[, id := 1:.N]
pass_tags[is.list(V1), outcome := any(unlist(V1) %in% 1801), id] 
all_pass[, outcome := pass_tags[, outcome]]

# Probability by zone that a shot will be created is merged from shotActionPass2
all_pass = merge(all_pass, shotActionPass2[, .(GridX2 = GridX, GridY2 = GridY, createShotProb = Prob)], by = c("GridX2", "GridY2"))
# passProb - how probable/dfficult a pass is
# createShotProb - the value of the pass

# Separating expected pass location by pass type, feet or head

# Footed passes
zoneX = 3
zoneY = 2
ggPitch + 
  geom_rect(data = all_pass[GridX == zoneX & GridY == zoneY & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)],
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = N)) +
  scale_fill_viridis(alpha = 0.75) + 
  annotate("rect", xmin = all_pass[GridX == zoneX & GridY == zoneY, min(startMinX)], 
           xmax = all_pass[GridX == zoneX & GridY == zoneY, max(startMaxX)],
           ymin = all_pass[GridX == zoneX & GridY == zoneY, min(startMinY)],
           ymax = all_pass[GridX == zoneX & GridY == zoneY, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone")

# Headed passes
ggPitch + 
  geom_rect(data = all_pass[GridX == zoneX & GridY == zoneY & passType == "passHead", .N, .(GridX2, GridY2)],
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = N)) +
  scale_fill_viridis(alpha = 0.75) + 
  annotate("rect", xmin = all_pass[GridX == zoneX & GridY == zoneY, min(startMinX)], 
           xmax = all_pass[GridX == zoneX & GridY == zoneY, max(startMaxX)],
           ymin = all_pass[GridX == zoneX & GridY == zoneY, min(startMinY)],
           ymax = all_pass[GridX == zoneX & GridY == zoneY, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of headed passes from red zone")


footLoc = all_pass[passType %in% c("passFoot", "cross"), .N, .(GridX, GridY, GridX2, GridY2)][order(GridX, GridY)]
footLoc[, zoneP := N / sum(N), .(GridX, GridY)]

# Probability by zone that a shot will be created is merged from shotActionPass2
footLoc = merge(footLoc, shotActionPass2[, .(GridX2 = GridX, GridY2 = GridY, createShotProb = Prob)], by = c("GridX2", "GridY2"))
# Expected value by zone
footLoc = footLoc[, .(xPassVal = sum(zoneP * createShotProb)), .(GridX, GridY)]
# equation:
  # p(z(1:t) | z3) * build-up value|z(1:t) => sum over all t zones => expected value from zone z3
# 

all_pass = merge(all_pass, footLoc, by = c("GridX", "GridY"))


# Pass prob from one zone to all other zones

passProbZone = data.table(expand.grid(x1 = seq(0, 105, 7), x2 = seq(0, 105, 7), y1 = seq(0, 65, 4),
                                      y2 = seq(0, 65, 4), passType = "passFoot"))
passProbZone[, passType := factor(passType, levels = c("passHead", "cross", "passHand", "passFoot"))]
passProbZone[, distOrigin := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
passProbZone[, distEnd := round(sqrt((x2 - 105)^2 + (y2- 32.5)^2), 2)]

# Angle to goal
passProbZone[, angleOrigin := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
passProbZone[angleOrigin < 0, angleOrigin := angleOrigin + pi]
passProbZone[, angleOrigin := angleOrigin * 180 / pi]

passProbZone[, angleEnd := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 32.5)^2 - (7.32/2)^2))]
passProbZone[angleEnd < 0, angleEnd := angleOrigin + pi]
passProbZone[, angleEnd := angleEnd * 180 / pi]

passProbZone[, inBoxOrigin := ifelse(x1 >= 88.5 & y1 >= 12.35 & y1 <= 52.65, TRUE, FALSE)]
passProbZone[, inBoxEnd := ifelse(x2 >= 88.5 & y2 >= 12.35 & y2 <= 52.65, TRUE, FALSE)]

# Angle to center of goal
passProbZone[, angleCenterOrigin := atan2((105 - x1) - 0, abs(y1 - 32.5) - 0) * 180 / pi]
passProbZone[, angleCenterEnd := atan2((105 - x2) - 0, abs(y2 - 32.5) - 0) * 180 / pi]

passProbZone[, passLength := abs(distEnd - distOrigin)]
passProbZone[, angleCenterDiff := angleCenterEnd - angleCenterOrigin]

# Distance to center
passProbZone[, centerDist := abs(y1 - 32.5)]
passProbZone[, centerDistEnd := abs(y2 - 32.5)]

passProbZone = passProbZone[passLength != 0]

passProbZone[, passProb := predict(logit_features, newdata = passProbZone, type = "response")]

passProbZone[, GridX := cut(x = x1, breaks = 5, labels = FALSE)]
passProbZone[, GridY := cut(x = y1, breaks = 5, labels = FALSE)]
passProbZone[, GridX2 := cut(x = x2, breaks = 5, labels = FALSE)]
passProbZone[, GridY2 := cut(x = y2, breaks = 5, labels = FALSE)]

passProbZone = passProbZone[, .(passProb = mean(passProb)), .(GridX, GridY, GridX2, GridY2)]

passProbZone[, MinX := (GridX2-1) / 5 * 105]
passProbZone[, MaxX := GridX2 / 5 * 105]
passProbZone[, MinY := (GridY2-1) / 5 * 65]
passProbZone[, MaxY := GridY2 / 5 * 65]

zoneX = 3
zoneY = 2
ggPitch + 
  geom_rect(data = passProbZone[GridX == zoneX & GridY == zoneY],
            aes(xmin = MinX, xmax = MaxX,
                ymin = MinY, ymax = MaxY,
                fill = passProb)) +
  scale_fill_viridis(alpha = 0.75, limits = c(0.3, 1)) + 
  annotate("rect", xmin = passProbZone[GridX2 == zoneX & GridY2 == zoneY, min(MinX)], 
           xmax = passProbZone[GridX2 == zoneX & GridY2 == zoneY, max(MaxX)],
           ymin = passProbZone[GridX2 == zoneX & GridY2 == zoneY, min(MinY)],
           ymax = passProbZone[GridX2== zoneX & GridY2 == zoneY, max(MaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Pass probability of footed passes from red zone")+
  theme(legend.position = "bottom")

# Now, finally, time for comparing expected impact (build-up value) and actual impact

# For all passes
all_pass[, xImpact := createShotProb * passProb]
all_pass[, aImpact := ifelse(outcome, createShotProb, 0)]

# Then filtering out passes played by feet (since epected value is calculated given footed passes)

playerImpact = all_pass[passType %in% c("passFoot", "cross"), 
                        .(xImpact = sum(xImpact), aImpact = sum(aImpact), impactDiff = sum(aImpact - xImpact), .N), playerId]
playerImpact = playerImpact[N > 500]
playerImpact[, xImpactAvg := xImpact / N]
playerImpact[, aImpactAvg := aImpact / N]
playerImpact[,impactDiffAvg := impactDiff / N]

# Merging with player data
selPlayer = players[wyId %in% playerImpact[, playerId], .(shortName, playerId = wyId, role.name, currentTeamId)]

playerImpact = merge(playerImpact, selPlayer, by = "playerId")

playerImpact[order(-xImpact)][1:10] # Playes passes with highest total expected impact
playerImpact[order(-xImpactAvg)][1:10] # Playes passes with highest average expected impact

playerImpact[order(-aImpact)][1:10] # Playes passes with highest total impact
playerImpact[order(-aImpactAvg)][1:10] # Playes passes with highest average impact

playerImpact[order(-impactDiff)][role.name != "Goalkeeper"][1:10] # Playes passes with highest total diff to average
playerImpact[order(-impactDiffAvg)][role.name != "Goalkeeper"][1:10] # Playes passes with highest average diff to average

# Excluding goalkeepers
playerImpact = playerImpact[role.name != "Goalkeeper"]

ggplot(playerImpact, aes(x = impactDiff, y = aImpact, color = currentTeamId == 1612)) + geom_point(size = 2) +
  geom_text_repel(data = playerImpact[aImpact > quantile(aImpact, 0.95) |
                                        abs(impactDiff) > quantile(abs(impactDiff), 0.95)],
                  aes(label = shortName), size = 3, color = "black") +
  scale_color_manual(values = c("grey72", "red3"), name = NULL, labels = c("", "Liverpool players"))

ggplot(playerImpact, aes(x = impactDiffAvg, y = aImpactAvg, color = currentTeamId == 1612)) + geom_point(size = 2) +
  geom_text_repel(data = playerImpact[aImpactAvg > quantile(aImpactAvg, 0.95) |
                                        abs(impactDiffAvg) > quantile(abs(impactDiffAvg), 0.975)],
                  aes(label = shortName), size = 3, color = "black") +
  scale_color_manual(values = c("grey72", "red3"), name = NULL, labels = c("", "Liverpool players"))

playerImpact[order(-aImpact), aImpactRank := 1:.N]
playerImpact[order(-aImpactAvg), aImpactAvgRank := 1:.N]
playerImpact[order(-impactDiff), impactDiffRank := 1:.N]
playerImpact[order(-impactDiffAvg), impactDiffAvgRank := 1:.N]

save(playerImpact, file = "playerImpact.rda")

# Liverpool players
playerImpact[order(-aImpact)][currentTeamId == 1612]
playerImpact[order(-aImpactAvg)][currentTeamId == 1612]
playerImpact[order(-impactDiffAvg)][currentTeamId == 1612]

# Look at Salah, Firmino, Milner and Gini/Hendo
# Salah id 120353
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 120353 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1]
# GridX == 4 & GridY == 4
salahPasses = all_pass[playerId == 120353 & GridX == 4 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
salahPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 4 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
salahPasses = merge(salahPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
salahPasses[is.na(zoneProb.x), zoneProb.x := 0]
salahPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = salahPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = salahPasses[, min(zoneDiff) * c(1, -1)],
                       name = NULL, breaks = c(-0.1, 0, 0.1)) +
  annotate("rect", xmin = all_pass[GridX == 4 & GridY == 4, min(startMinX)], 
           xmax = all_pass[GridX == 4 & GridY == 4, max(startMaxX)],
           ymin = all_pass[GridX == 4 & GridY == 4, min(startMinY)],
           ymax = all_pass[GridX == 4 & GridY == 4, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Mohamed Salah - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom") + coord_cartesian(xlim = c(40, 107))

# Firmino id 15808
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 15808 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1]
# GridX == 4 & GridY == 4
bobbyPasses = all_pass[playerId == 15808 & GridX == 4 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
bobbyPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 4 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
bobbyPasses = merge(bobbyPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
bobbyPasses[is.na(zoneProb.x), zoneProb.x := 0]
bobbyPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = bobbyPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = salahPasses[, min(zoneDiff) * c(1, -1)],
                       name = NULL, breaks = c(-0.1, 0, 0.1)) +
  annotate("rect", xmin = all_pass[GridX == 4 & GridY == 4, min(startMinX)], 
           xmax = all_pass[GridX == 4 & GridY == 4, max(startMaxX)],
           ymin = all_pass[GridX == 4 & GridY == 4, min(startMinY)],
           ymax = all_pass[GridX == 4 & GridY == 4, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Roberto Firmino - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")  + coord_cartesian(xlim = c(40, 107))


# Wijnaldum id 116 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 116 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1:2]
# GridX == 3 & GridY == 2
giniPasses = all_pass[playerId == 116 & GridX == 3 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
giniPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 3 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
giniPasses = merge(giniPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
giniPasses[is.na(zoneProb.x), zoneProb.x := 0]
giniPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = giniPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = giniPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.08, 0, 0.08)) +
  annotate("rect", xmin = all_pass[GridX == 3 & GridY == 2, min(startMinX)], 
           xmax = all_pass[GridX == 3 & GridY == 2, max(startMaxX)],
           ymin = all_pass[GridX == 3 & GridY == 2, min(startMinY)],
           ymax = all_pass[GridX == 3 & GridY == 2, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Gini Wijnaldum - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")

# Milner id 8319 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 8319 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1:2]
# GridX == 3 & GridY == 2
milnerPasses = all_pass[playerId == 8319 & GridX == 3 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
milnerPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 3 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
milnerPasses = merge(milnerPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
milnerPasses[is.na(zoneProb.x), zoneProb.x := 0]
milnerPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = milnerPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = giniPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.08, 0, 0.08)) +
  annotate("rect", xmin = all_pass[GridX == 3 & GridY == 2, min(startMinX)], 
           xmax = all_pass[GridX == 3 & GridY == 2, max(startMaxX)],
           ymin = all_pass[GridX == 3 & GridY == 2, min(startMinY)],
           ymax = all_pass[GridX == 3 & GridY == 2, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "James Milner - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")

# Fabregas id 3350 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 3350 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1:3]
# GridX == 3 & GridY == 4
cescPasses = all_pass[playerId == 3350 & GridX == 3 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
cescPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 3 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
cescPasses = merge(cescPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
cescPasses[is.na(zoneProb.x), zoneProb.x := 0]
cescPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = cescPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = cescPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.04, 0, 0.04)) +
  annotate("rect", xmin = all_pass[GridX == 3 & GridY == 4, min(startMinX)], 
           xmax = all_pass[GridX == 3 & GridY == 4, max(startMaxX)],
           ymin = all_pass[GridX == 3 & GridY == 4, min(startMinY)],
           ymax = all_pass[GridX == 3 & GridY == 4, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Cesc Fabregas - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")

# Xhaka id 49876 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 49876 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1:3]
# GridX == 3 & GridY == 2
xhakaPasses = all_pass[playerId == 49876 & GridX == 3 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
xhakaPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 3 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
xhakaPasses = merge(xhakaPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
xhakaPasses[is.na(zoneProb.x), zoneProb.x := 0]
xhakaPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = xhakaPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = cescPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.04, 0, 0.04)) +
  annotate("rect", xmin = all_pass[GridX == 3 & GridY == 2, min(startMinX)], 
           xmax = all_pass[GridX == 3 & GridY == 2, max(startMaxX)],
           ymin = all_pass[GridX == 3 & GridY == 2, min(startMinY)],
           ymax = all_pass[GridX == 3 & GridY == 2, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Granit Xhaka - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")


# Mezut Özil id 3319 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 3319 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1]
# GridX == 4 & GridY == 2
ozilPasses = all_pass[playerId == 3319 & GridX == 4 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
ozilPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 4 & GridY == 2 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
ozilPasses = merge(ozilPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
ozilPasses[is.na(zoneProb.x), zoneProb.x := 0]
ozilPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = ozilPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = ozilPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.08, 0, 0.08)) +
  annotate("rect", xmin = all_pass[GridX == 4 & GridY == 2, min(startMinX)], 
           xmax = all_pass[GridX == 4 & GridY == 2, max(startMaxX)],
           ymin = all_pass[GridX == 4 & GridY == 2, min(startMinY)],
           ymax = all_pass[GridX == 4 & GridY == 2, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Mezut Özil - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")



# Kevin de Bruyne id 38021 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 38021 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1]
# GridX == 3 & GridY == 3
bruynePasses = all_pass[playerId == 38021 & GridX == 4 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
bruynePasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 4 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
bruynePasses = merge(bruynePasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
bruynePasses[is.na(zoneProb.x), zoneProb.x := 0]
bruynePasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = bruynePasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = ozilPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.08, 0, 0.08)) +
  annotate("rect", xmin = all_pass[GridX == 4 & GridY == 4, min(startMinX)], 
           xmax = all_pass[GridX == 4 & GridY == 4, max(startMaxX)],
           ymin = all_pass[GridX == 4 & GridY == 4, min(startMinY)],
           ymax = all_pass[GridX == 4 & GridY == 4, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Kevin de Bruyne - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")


# save files for markdown report
save(shotActionPass2, file = "Project/files/shotActionPass2.rda")
save(all_pass, file = "Project/files/all_pass.rda")
save(footLoc, file = "Project/files/footLoc.rda")
save(passProbZone, file = "Project/files/passProbZone.rda")
save(playerImpact, file = "Project/files/playerImpact.rda")
save(salahPasses, file = "Project/files/salahPasses.rda")
save(bobbyPasses, file = "Project/files/bobbyPasses.rda")
save(milnerPasses, file = "Project/files/milnerPasses.rda")
save(giniPasses, file = "Project/files/giniPasses.rda")

save(xhakaPasses, file = "Project/files/xhakaPasses.rda")
save(cescPasses, file = "Project/files/cescPasses.rda")
save(ozilPasses, file = "Project/files/ozilPasses.rda")
save(bruynePasses, file = "Project/files/bruynePasses.rda")

# for analysis of xA
passShots = shotAction[, which(shotInd %in% shotsInd & eventName == "Pass") ]
passShots = shotsDT[passShots, 1:18]
# passShots[, playerAssistId := shotActionPass[, playerId]]
save(passShots, file = "Project/files/passShots.rda")
save(shotActionPass, file = "Project/files/shotActionPass.rda")


save(shotActionPass2, file = "Project/files/shotActionPass2.rda")

load("Project/files/shotActionPass2.rda")



