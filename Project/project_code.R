# Load packages
library(jsonlite) # read json files
library(data.table) # pre-process data
library(ggplot2) # plotting
library(viridis) # plotting

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
load("wy_pl.rda")
games <- fromJSON(txt = "Wyscout/matches/matches_England.json", simplifyDataFrame = FALSE)
gamesDT = list()
for(i in 1:length(games)){
  if(games[[i]]$teamsData[[1]]$side == "home"){
    homeID = games[[i]]$teamsData[[1]]$teamId
    awayID = games[[i]]$teamsData[[2]]$teamId
  }else{
    homeID = games[[i]]$teamsData[[2]]$teamId
    awayID = games[[i]]$teamsData[[1]]$teamId
  }
  Teams = games[[i]]$label
  Teams = strsplit(strsplit(Teams, ",")[[1]][1], " - ")[[1]]
  gameID = games[[i]]$wyId
  gamesDT[[i]] = data.table(homeID, awayID, homeTeam = Teams[1], awayTeam = Teams[2], gameID,
                            Date = games[[i]]$dateutc, gw = games[[i]]$gameweek)
}
gamesDT = rbindlist(gamesDT)

# A missed pass, a lost duel, a saved or blocked shot -> Loss of possesion
  # Location of the particular event
  # Defensive action 0-6 seconds after
  # Was possession regained or not?

  # Other variables
    # Length of possession (time)
    # Game state

# Start with looking at one game, watford-liverpool

watLiv = pl[matchId == 2499727]

# Get the coordinates to columns
pass_coord = data.table(t(apply(watLiv, 1, function(x) unlist(x$position))))
watLiv[, names(pass_coord) := pass_coord]
# Normalize pitch coordinates according to standard(?) dimension 105*65m  
watLiv[, y1 := y1 / 100 * 65]
watLiv[, y2 := y2 / 100 * 65]
watLiv[, x1 := x1 / 100 * 105]
watLiv[, x2 := x2 / 100 * 105]

# Look at tags to determine outcome for passes and duels
  # 701 == lost (duel)
  # 703 == won (duel)
  # 1801 == accurate
  # 1802 == not accurate
tags = data.table(apply(watLiv, 1, function(x) unlist(x$tags)))
tags[, id := 1:.N]
tags[is.list(V1), outcome := any(unlist(V1) %in% 1801) | any(unlist(V1) %in% 703) , id] 
tagsOutcome = tags[, outcome]
watLiv[, outcome := tagsOutcome]
# In final third or not
watLiv[, finalThird := ifelse(x1 >= 105 * (2/3), 1, 0)]
watLiv[, finalThirdEnd := ifelse(x2 >= 105 * (2/3), 1, 0)]


gamesDT[, .(homeID, homeTeam)][, unique(homeID), homeTeam]
  # Liverpool == 1612
# Plot of all liverpools passes in or into the final third
ggPitch +
  geom_segment(data = watLiv[finalThirdEnd == 1 & eventName == "Pass" & teamId == 1612],
               aes(x = x1, xend = x2, y = y1, yend = y2, color = outcome), arrow = arrow(length = unit(0.02, "npc")))

# Duels in the final third
ggPitch +
  geom_point(data = watLiv[finalThird == 1 & eventName == "Duel" & teamId == 1612],
             aes(x = x1, y = y1, color = outcome))

# If a pass, knows that Liverpool were in possession,
  # if a duel, not necessary the case
  # So, using the sub event group attacking duel

ggPitch +
  geom_point(data = watLiv[finalThird == 1 & subEventName == "Ground attacking duel" & teamId == 1612],
             aes(x = x1, y = y1, color = outcome))


# So, now want to list all the defensive actions made, up to x seconds after the ball was lost in the attacking third

lostPoss = watLiv[teamId == 1612 & ((eventName == "Pass" & finalThirdEnd == 1) |
                                      (subEventName == "Ground attacking duel" & finalThirdEnd == 1)) & outcome == FALSE]
ggPitch +
  geom_point(data = lostPoss, aes(x = x2, y = y2, color = eventName))

watLiv[eventSec >= lostPoss[1, eventSec + 0.1] & eventSec < lostPoss[1, eventSec + 10]]

temp = 18
watLiv[eventSec >= lostPoss[temp, eventSec] & eventSec < lostPoss[temp, eventSec + 10] & matchPeriod == lostPoss[temp, matchPeriod]]
  # must be in same match period

# Duel won === possession regained?
  # Look if a liverpool player won a duel or intercepted the ball up to 10 seconds after they lost the ball

# Hierarchical model that has a team specific intercept + x and y to predict the probability of winning the ball back in final third

# However, must then remove cases where the ball directly went out of play or there was an offside
  # Tricky since it often might be touch followed by out of play

# Conclusion, using this data I probably should be doing something else



# xG model as foundation
  # where the ball was played from / where the ball was 5 seconds before
    # => value of a zone

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

# Add points where prob is 0
# from very far, for example
shotsDT = rbind(shotsDT, 
                expand.grid(x1 = seq(0, 55, 5), y1 = seq(0, 65, 5), outcome = FALSE, head = c(TRUE, FALSE)),
                fill = TRUE)

# Distance to center of goal
shotsDT[, distCenter := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
# Angle to goal
shotsDT[, angleGoal := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
shotsDT[angleGoal < 0, angleGoal := angleGoal + pi]
shotsDT[, angleGoal := angleGoal * 180 / pi]

ggplot(shotsDT[, mean(outcome), .(angleGoal = round(angleGoal, 0))], aes(x = angleGoal, y = V1)) +
  geom_point() + geom_line() + geom_smooth(formula = y ~ x + I(x^2) + I(x^3), method = "lm")


xg_mod =  glm(outcome ~ distCenter + I(distCenter^2) + angleGoal + head,
              data = shotsDT, family = "binomial")
summary(xg_mod)

testData = data.table(expand.grid(x1 = 65:105, y1 = 10:55, head = FALSE ))
# Distance to center of goal
testData[, distCenter := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
# Angle to goal
testData[, angleGoal := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
testData[angleGoal < 0, angleGoal := angleGoal + pi]
testData[, angleGoal := angleGoal * 180 / pi]

testData[, xG := predict(xg_mod, newdata = testData, type = "response")]

save(testData, file="xG_testData.rda")

ggPitch + 
  geom_point(data = testData, aes(x = x1, y = y1, color = xG), size = 2) +
  scale_color_viridis(alpha = 0.5, direction = -1, option = "B", limits = c(0.05, 1), na.value = "transparent") +
  labs(title = "xG model probabilities. Cut-off at 5 %")

# looking at a couple of shots in order to understand the data...
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

# Distance to center of goal
tempDT[, distCenter := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
# Angle to goal
tempDT[, angleGoal := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
tempDT[angleGoal < 0, angleGoal := angleGoal + pi]
tempDT[, angleGoal := angleGoal * 180 / pi]

tempDT[, xG := predict(xg_mod, newdata = tempDT, type = "response")]
tempDT[eventName == "Shot", x2 := 105]
tempDT[eventName == "Shot", y2 := 32.5]

ggPitch +
  geom_segment(data = tempDT[teamId == tempDT[eventName == "Shot", unique(teamId)]],
               aes(x = x1, y = y1, xend = x2, yend = y2, color = xG),
               arrow = arrow(length = unit(0.02, "npc")), size = 1) +
  geom_label(data = tempDT[teamId == tempDT[eventName == "Shot", unique(teamId)]],
             aes(x = x1, y = y1, label = eventName)) +
  scale_color_viridis()


# Probability for where the ball will be passed (learned from data)
# Probability for the pass to be accurate (from pass model)
# Probability to score goal in zone
# Probability to make an assist in zone
# or
# Probability to be involved in a high xG chance in 5-10 seconds

# From zone z3, player will probably make a pass to zone z4 (p z4|z3 = 0.7)
# probability for pass to be accurate = 0.75
# probabolity to score in zone = 0.001
# Probability to make an assist in zone = 0.075


  # Goal creation value: 0.75 * 0.001
  # Build-up value: 0.75 * 0.075


## Check previous action before shot
pl[pl[, which(eventName == "Shot") - 1], .N, eventName]
  # Want prervious action by same team
# Difficult to avoid a for loop I think...
shotsInd = pl[, which(eventName == "Shot")]

shotAction = list()
k = 0
for(i in shotsInd){
  k = k+1
  shotAction[[k]] = tail(pl[i - (1:5)][teamId == shotsDT[k, teamId]], 1)
}
shotAction = rbindlist(shotAction)

shotAction[, .N, eventName]
# 4200 passes leading to shots
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
  scale_fill_viridis(alpha = 0.75)

# Proportion of passes that becomes assists to a shot
shotActionPass2 = merge(shotActionPass2, passesZone, by = c("GridX", "GridY"))
shotActionPass2[, Prob := Count / N]
ggPitch + 
  geom_rect(data = shotActionPass2,
            aes(xmin = startMinX, xmax = startMaxX,
                ymin = startMinY, ymax = startMaxY,
                fill = Prob)) +
  scale_fill_viridis(alpha = 0.75, breaks = c(0, 0.025, 0.05), limits = c(0, 0.05))

save(shotActionPass2, file = "shotActionPass2.rda")
  # Separate passes and headers?

# Prob to assist to an high xG shot (xG >= 0.05)
passShots = shotAction[, which(eventName == "Pass")]
passShots = shotsDT[passShots]
passShots[, xG := predict(xg_mod, newdata = passShots, type = "response")]
xGpasses = shotActionPass[passShots[, which(xG >= 0.05)]]
xGpasses = xGpasses[, .(Count = .N), .(GridX, GridY)]
xGpasses[, startMinX := (GridX-1) / 5 * 105]
xGpasses[, startMaxX := GridX / 5 * 105]
xGpasses[, startMinY := (GridY-1) / 5 * 65]
xGpasses[, startMaxY := GridY / 5 * 65]
xGpasses = merge(xGpasses, passesZone, by = c("GridX", "GridY"))
xGpasses[, Prob := Count / N]

ggPitch + 
  geom_rect(data = xGpasses,
            aes(xmin = startMinX, xmax = startMaxX,
                ymin = startMinY, ymax = startMaxY,
                fill = Prob)) +
  scale_fill_viridis(alpha = 0.75, breaks = c(0, 0.03), limits = c(0, 0.03))


# Locations of shots from passes
passShots[, GridX := cut(x = x1, breaks = 10, labels = FALSE)]
passShots[, GridY := cut(x = y1, breaks = 10, labels = FALSE)]
passShots2 = passShots[, .(mean = mean(xG)), .(GridX, GridY)]
passShots2[, startMinX := (GridX-1) / 10 * 105]
passShots2[, startMaxX := GridX / 10 * 105]
passShots2[, startMinY := (GridY-1) / 10 * 65]
passShots2[, startMaxY := GridY / 10 * 65]
ggPitch + 
  geom_rect(data = passShots2,
            aes(xmin = startMinX, xmax = startMaxX,
                ymin = startMinY, ymax = startMaxY,
                fill = mean)) +
  scale_fill_viridis(alpha = 0.75)


# Probability to assist a goal
  # Probability that the pass will be accurate
# => Chance creation value


# Probability to move to ball to a valuable area
  # Probability that pass will be accurate
# => Build-up value

# load pass evaluation model
load("passmodel.rda")

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

# Expected value: createShotProb * passProb
  # or
# Expected value: createShotProb * 100 *passProb

all_pass[, xPassValue := createShotProb * 100 * passProb]
all_pass[, aPassValue := ifelse(outcome, createShotProb * 100, 0)]

passValue = all_pass[, .(xPassValue = sum(xPassValue), aPassValue = sum(aPassValue), .N), playerId]
passValue[, avgValueX := xPassValue / N]
passValue[, avgValueA := aPassValue / N]
passValue[, Diff := aPassValue - xPassValue]
passValue = passValue[N > 500]

# Load player data
players <- data.table(fromJSON(txt = "wyscout/players.json"))
# Merge with player data
selPlayer = players[wyId %in% passValue[, playerId], .(shortName, playerId = wyId, role.name, currentTeamId)]

passValue = merge(passValue, selPlayer, by = "playerId")
passValue[order(-avgValueX)][1:10]
passValue[order(-avgValueA)][1:10]
passValue[order(-Diff)][role.name != "Goalkeeper"][1:10]

passValue[order(-avgValueX), valueRankX := 1:.N]
passValue[order(-avgValueA), valueRankA := 1:.N]
passValue[order(-Diff), diffRank := 1:.N]

# Liverpool players
passValue[order(-avgValueX)][currentTeamId == 1612]
passValue[order(-avgValueA)][currentTeamId == 1612]
passValue[order(-Diff)][currentTeamId == 1612]



# Other ways of comparing, measuring output with this values?
  # Using the most probable pass zone for the location and compare the outcome to the most probable choice?

# example
  # from zone [x3, y2] it is most common to pass to zone [x3, y3]
  # that pass would have a probability of qq
  # the value of that pass would be ww

# Compare that value to the value of the actual pass


# Must(?) get pass location given pass type
  # given that the player chose to do this kind of pass...
  # Or, just separate headed and footed passes?
  # And remove hand passes?

# Footed passes
zoneX = 5
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
  geom_rect(data = all_pass[GridX == 2 & GridY == 2 & passType == "passHead", .N, .(GridX2, GridY2)],
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = N)) +
  scale_fill_viridis(alpha = 0.75) + 
  annotate("rect", xmin = all_pass[GridX == 2 & GridY == 2, min(startMinX)], 
           xmax = all_pass[GridX == 2 & GridY == 2, max(startMaxX)],
           ymin = all_pass[GridX == 2 & GridY == 2, min(startMinY)],
           ymax = all_pass[GridX == 2 & GridY == 2, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of headed passes from red zone")

save(all_pass, file = "all_pass.rda")


footLoc = all_pass[passType %in% c("passFoot", "cross"), .N, .(GridX, GridY, GridX2, GridY2)][order(GridX, GridY)]
footLoc[, zoneP := N / sum(N), .(GridX, GridY)]

# Probability by zone that a shot will be created is merged from shotActionPass2
footLoc = merge(footLoc, shotActionPass2[, .(GridX2 = GridX, GridY2 = GridY, createShotProb = Prob)], by = c("GridX2", "GridY2"))
  # Expected value by zone
footLoc = footLoc[, .(xPassVal = sum(zoneP * createShotProb)), .(GridX, GridY)]
# equation:
  # p(z1 | z3) * build-up value|z1 => sum over all zones => expected value from zone
    # 

save(footLoc, file = "footLoc.rda")

all_pass = merge(all_pass, footLoc, by = c("GridX", "GridY"))

# xPassVal is the expected average value for a pass from that zone
# createShotProb is the actual value of the pass
  # So, a positive diff gives that the player tries to do more valuable passes

passValue = all_pass[, .(xPassVal = sum(xPassVal), aPassValue = sum(createShotProb), .N), playerId]
passValue[, avgValueX := xPassVal / N]
passValue[, avgValueA := aPassValue / N]
passValue[, Diff := aPassValue - xPassVal]
passValue = passValue[N > 500]

# Load player data
players <- data.table(fromJSON(txt = "wyscout/players.json"))
# Merge with player data
selPlayer = players[wyId %in% passValue[, playerId], .(firstName, lastName, playerId = wyId, role.name, currentTeamId)]

passValue = merge(passValue, selPlayer, by = "playerId")
passValue[order(-avgValueX)][1:10]
passValue[order(-avgValueA)][1:10]
passValue[order(-Diff)][role.name != "Goalkeeper"][1:10]

passValue[order(-avgValueX), valueRankX := 1:.N]
passValue[order(-avgValueA), valueRankA := 1:.N]
passValue[order(-Diff), diffRank := 1:.N]

# Liverpool players
passValue[order(-avgValueX)][currentTeamId == 1612]
passValue[order(-avgValueA)][currentTeamId == 1612]
passValue[order(-Diff)][currentTeamId == 1612]


# Have not included any info regarding pass probability
  # Weight passes by their probability?
all_pass[, xPassValW := xPassVal * passProb]
all_pass[, createShotProbW := createShotProb * passProb]

passValue2 = all_pass[, .(xPassValW = sum(xPassValW), aPassValue = sum(createShotProbW), .N), playerId]
passValue2[, avgValueX := xPassValW / N]
passValue2[, avgValueA := aPassValue / N]
passValue2[, Diff := avgValueA - avgValueX]
passValue2 = passValue2[N > 500]

passValue2 = merge(passValue2, selPlayer, by = "playerId")
passValue2[order(-avgValueX)][1:10] # Most often in situations where a valuable pass could be made
passValue2[order(-avgValueA)][1:10] # Players who does most valuable passing
passValue2[order(-Diff)][role.name != "Goalkeeper"][1:10] # Tendency to chose more valuable passes
passValue2[order(Diff)][role.name != "Goalkeeper"][1:10] # Tendency to chose less valuable passes

passValue2[order(-avgValueX), valueRankX := 1:.N]
passValue2[order(-avgValueA), valueRankA := 1:.N]
passValue2[order(-Diff), diffRank := 1:.N]

# Liverpool players
passValue2[order(-avgValueX)][currentTeamId == 1612]
passValue2[order(-avgValueA)][currentTeamId == 1612]
passValue2[order(-Diff)][currentTeamId == 1612]

# xPass tells if a player often is in situtaions where a valuable pass could be made
# aPass tells how often a player did a valuable pass
passValue2[order(-avgValueA)][role.name == "Defender"][1:10] 
passValue2[order(-avgValueA)][role.name == "Midfielder"][1:10] 
passValue2[order(-avgValueA)][role.name == "Forward"][1:10] 


# Yet to include success rate of the passes

# if passProb = 0.8
  # 80 % to keep actual pass value
all_pass[, xValue := (createShotProb - xPassVal) * passProb]
all_pass[, aValue := ifelse(outcome, createShotProb - xPassVal, 0)] # value of a missed pass is 0

passValue3 = all_pass[, .(xValue = sum(aValue - xValue), .N), playerId]
passValue3[, avgValue := xValue / N]
passValue3 = passValue3[N > 500]
passValue3 = merge(passValue3, selPlayer, by = "playerId")

passValue3[order(-xValue)][1:10]
passValue3[order(-avgValue)][1:10]

passValue3[order(-avgValue)][role.name == "Defender"][1:10] 
passValue3[order(-avgValue)][role.name == "Midfielder"][1:10] 
passValue3[order(-avgValue)][role.name == "Forward"][1:10] 

# Results here, weirder than previous approach

# Previous approach was not neccessarily that bad but
  # Weight diff against average value?

passValue2[, quantile(avgValueA, c(0.2))]

ggplot(passValue2, aes(x = avgValueA, y = Diff)) + geom_point() +
  ggrepel::geom_text_repel(data = passValue2[Diff > quantile(Diff, 0.95) | avgValueA > quantile(avgValueA, 0.95)],
    aes(label = lastName), size = 3)

# Include success rate in some way?


# Right now I have the following
  # Average build-up value of pass location
  # Average build-up value of pass
  # Average difficulty of pass
  # Average pass completion

# Can add
  # Average xA value of pass location
  # Average xA value of pass


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

save(passProbZone, file = "passProbZone.rda")


# For all passes
all_pass[, xImpact := createShotProb * passProb]
all_pass[, aImpact := ifelse(outcome, createShotProb, 0)]

save(all_pass, file = "all_pass.rda")

playerImpact = all_pass[, .(xImpact = sum(xImpact), aImpact = sum(aImpact), impactDiff = sum(aImpact - xImpact), .N), playerId]
playerImpact = playerImpact[N > 500]
playerImpact[, xImpactAvg := xImpact / N]
playerImpact[, aImpactAvg := aImpact / N]
playerImpact[,impactDiffAvg := impactDiff / N]
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

# Look at Salah and Henderson
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
  theme(legend.position = "bottom")

save(salahPasses, file = "salahPasses.rda")


# Henderson id 7964 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 7964 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1]
# GridX == 3 & GridY == 4
hendoPasses = all_pass[playerId == 7964 & GridX == 3 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
hendoPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 3 & GridY == 4 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
hendoPasses = merge(hendoPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
hendoPasses[is.na(zoneProb.x), zoneProb.x := 0]
hendoPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = hendoPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = hendoPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.06, 0, 0.06)) +
  annotate("rect", xmin = all_pass[GridX == 3 & GridY == 4, min(startMinX)], 
           xmax = all_pass[GridX == 3 & GridY == 4, max(startMaxX)],
           ymin = all_pass[GridX == 3 & GridY == 4, min(startMinY)],
           ymax = all_pass[GridX == 3 & GridY == 4, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Jordan Henderson - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")

save(hendoPasses, file = "hendoPasses.rda")

# Xhaka id 49876 
# Take his most common zone
# Compare his average location of passes to the average players' pass locations
all_pass[playerId == 49876 & passType %in% c("passFoot", "cross"), .N, .(GridX, GridY)][order(-N)][1]
# GridX == 3 & GridY == 3
xhakaPasses = all_pass[playerId == 49876 & GridX == 3 & GridY == 3 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
xhakaPasses[, zoneProb := N / sum(N)]
# Merge with zone average table
zoneAvg = all_pass[GridX == 3 & GridY == 3 & passType %in% c("passFoot", "cross"), .N, .(GridX2, GridY2)]
zoneAvg[, zoneProb := N / sum(N)]
xhakaPasses = merge(xhakaPasses, zoneAvg, by = c("GridX2", "GridY2"), all = TRUE)
xhakaPasses[is.na(zoneProb.x), zoneProb.x := 0]
xhakaPasses[, zoneDiff := zoneProb.x - zoneProb.y]

ggPitch + 
  geom_rect(data = xhakaPasses,
            aes(xmin = (GridX2-1) / 5 * 105, xmax = GridX2 / 5 * 105,
                ymin = (GridY2-1) / 5 * 65, ymax = GridY2 / 5 * 65,
                fill = zoneDiff)) +
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = xhakaPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.04, 0, 0.04)) +
  annotate("rect", xmin = all_pass[GridX == 3 & GridY == 3, min(startMinX)], 
           xmax = all_pass[GridX == 3 & GridY == 3, max(startMaxX)],
           ymin = all_pass[GridX == 3 & GridY == 3, min(startMinY)],
           ymax = all_pass[GridX == 3 & GridY == 3, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Granit Xhaka - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")

save(xhakaPasses, file = "xhakaPasses.rda")

# Fabian Delph id 8464 
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
  scale_fill_gradient2(low = "#80435e", mid = "#c7c4b875", high = "#5e8043", 0, limits = delphPasses[, max(abs(zoneDiff)) * c(-1, 1)],
                       name = NULL, breaks = c(-0.03, 0, 0.03)) +
  annotate("rect", xmin = all_pass[GridX == 4 & GridY == 4, min(startMinX)], 
           xmax = all_pass[GridX == 4 & GridY == 4, max(startMaxX)],
           ymin = all_pass[GridX == 4 & GridY == 4, min(startMinY)],
           ymax = all_pass[GridX == 4 & GridY == 4, max(startMaxY)],
           fill = "transparent", color = "red", size = 1) +
  labs(title = "Location of footed passes from red zone", 
       subtitle = "Kevin de Bruyne - Compared to average location of passes from same zone")+
  theme(legend.position = "bottom")

# xG 

shotsDT[, xG := predict(xg_mod, type = "response")]

player_xG = merge(shotsDT[!is.na(playerId)],
                  players[, .(shortName, playerId = wyId, role.name, currentTeamId)],
                  by = "playerId")

player_xG = player_xG[, .(xG = sum(xG), G = sum(outcome), xGdiff = sum(outcome) - sum(xG),
                          shots = .N, xG_shot = sum(xG) / .N, G_shot = sum(outcome) / .N),
                      .(playerId, shortName, currentTeamId)]

player_xG = player_xG[shots > 30]

ggplot(player_xG, aes(x = xG, y = G, color = currentTeamId == 1612)) +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x, aes(group = 1), color = "grey62") + 
  geom_point() +
  geom_text_repel(data = player_xG[abs(xGdiff) > quantile(abs(xGdiff), 0.9) | G > quantile(G, 0.95)], 
                  aes(label = shortName), size = 3, color = "grey78") +
  scale_color_manual(values = c("grey58", "#C8102E"), name = NULL, labels = c("", "Liverpool\nplayers")) 

ggplot(player_xG, aes(x = xG_shot, y = G_shot, color = currentTeamId == 1612)) +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x) + 
  geom_point() +
  geom_text_repel(data = player_xG[xG_shot > quantile(xG_shot, 0.95) |
                                     xG_shot < quantile(xG_shot, 0.05) | 
                                     G_shot > quantile(G_shot, 0.95) |
                                     G_shot < quantile(G_shot, 0.1)], 
                  aes(label = shortName), size = 3)

save(player_xG, file = "player_xG.rda")

# xA 
  # passShots has xG 
player_xA = merge(passShots,
                  players[, .(shortName, playerId = wyId, role.name, currentTeamId)],
                  by = "playerId")

player_xA = player_xA[, .(xA = sum(xG), A = sum(outcome), xAdiff = sum(outcome) - sum(xG),
                          passes = .N, xA_pass = sum(xG) / .N, A_pass = sum(outcome) / .N),
                      .(playerId, shortName, currentTeamId)]

player_xA = player_xA[passes > 20]


ggplot(player_xA, aes(x = xA, y = A, color = currentTeamId == 1612)) +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x, aes(group = 1), color = "grey62") + 
  geom_point() +
  geom_text_repel(data = player_xA[abs(xAdiff) > quantile(abs(xAdiff), 0.9) | A > quantile(A, 0.95)], 
                  aes(label = shortName), size = 3, color = "grey78") +
  scale_color_manual(values = c("grey58", "#C8102E"), name = NULL, labels = c("", "Liverpool\nplayers")) 

ggplot(player_xA, aes(x = xA_pass, y = A_pass, color = currentTeamId == 1612)) +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x, aes(group = 1)) + 
  geom_point() +
  geom_text_repel(data = player_xA[xA_pass > quantile(xA_pass, 0.95) |
                                     xA_pass < quantile(xA_pass, 0.05) | 
                                     A_pass > quantile(A_pass, 0.95) |
                                     A_pass < quantile(A_pass, 0.1)], 
                  aes(label = shortName), size = 3)

save(player_xA, file = "player_xA.rda")


# To radar:
  # xG per shot
  # Shots

# Would like to make these per 90, i.e. get number of minutes played for each player



