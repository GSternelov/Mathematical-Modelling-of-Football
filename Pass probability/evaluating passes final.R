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

# Read the data
# Takes a few minutes..
ll <- fromJSON(txt = "Wyscout/events/events_Spain.json")
ll <- data.table(ll)
# load("wy_ll.rda") # r format version of la liga data, faster to load than json

# Some pre-processing
# Filter out passes
all_pass = ll[eventName == "Pass"]

# Get the coordinates to columns
pass_coord = data.table(t(apply(all_pass, 1, function(x) unlist(x$position))))
all_pass[, names(pass_coord) := pass_coord]
# Normalize pitch coordinates according to standard(?) dimension 105*65m  
all_pass[, y1 := y1 / 100 * 65]
all_pass[, y2 := y2 / 100 * 65]
all_pass[, x1 := x1 / 100 * 105]
all_pass[, x2 := x2 / 100 * 105]

# Pass types
all_pass[subEventName %in% c("Simple pass", "Smart pass",  "High pass", "Launch"), passType := "passFoot"]
all_pass[subEventName == "Head pass", passType := "passHead"]
all_pass[subEventName == "Cross", passType := "cross"]
all_pass[subEventName == "Hand pass", passType := "passHand"]


# Look at tags to determine outcome
# 1801 == accurate
pass_tags = data.table(apply(all_pass, 1, function(x) unlist(x$tags)))
pass_tags[, id := 1:.N]
pass_tags[is.list(V1), outcome := any(unlist(V1) %in% 1801), id] 
all_pass[, outcome := pass_tags[, outcome]]

all_pass = all_pass[, .(subEventName, playerId, matchId, teamId, eventSec, id,
                        y1, y2, x1, x2, passType, outcome)]


# In the first model use only the start co-ordinates of the pass. to predict success.  (5 points)
logit_startCoord =  glm(outcome ~ x1 + y1, data = all_pass, family = "binomial")


# Improve your model as much as possible by including x2 , y2 , x*y, goal angle,
# end co-ordinates of the pass etc.
# You can also include whether the pass was a cross or another type of ball.  (3 points)

# Playing around with features and polynomials...
# Distance to center of goal
all_pass[, distOrigin := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
all_pass[, distEnd := round(sqrt((x2 - 105)^2 + (y2- 32.5)^2), 2)]

# Angle to goal
all_pass[, angleOrigin := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
all_pass[angleOrigin < 0, angleOrigin := angleOrigin + pi]
all_pass[, angleOrigin := angleOrigin * 180 / pi]

all_pass[, angleEnd := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 32.5)^2 - (7.32/2)^2))]
all_pass[angleEnd < 0, angleEnd := angleOrigin + pi]
all_pass[, angleEnd := angleEnd * 180 / pi]

# In penalty box or not
all_pass[, inBoxOrigin := ifelse(x1 >= 88.5 & y1 >= 12.35 & y1 <= 52.65, TRUE, FALSE)]
all_pass[, inBoxEnd := ifelse(x2 >= 88.5 & y2 >= 12.35 & y2 <= 52.65, TRUE, FALSE)]

# Angle to center of goal
all_pass[, angleCenterOrigin := atan2((105 - x1) - 0, abs(y1 - 32.5) - 0) * 180 / pi]
all_pass[, angleCenterEnd := atan2((105 - x2) - 0, abs(y2 - 32.5) - 0) * 180 / pi]

# Length of pass
all_pass[, passLength := abs(distEnd - distOrigin)]

# Diff in angle to center, after - before
all_pass[, angleCenterDiff := (angleCenterEnd - angleCenterOrigin)]

# Distance to center
all_pass[, centerDist := abs(y1 - 32.5)]
all_pass[, centerDistEnd := abs(y2 - 32.5)]

# Headed passes should not be longer than 40 m?
all_pass[passType == "passHead", quantile(passLength, probs = seq(0, 1, 0.05))]
all_pass = all_pass[(passType == "passHead" & abs(passLength) > 40) == FALSE]

# Hand passes should not be longer than 70 m?
all_pass[passType == "passHand", quantile(passLength, probs = seq(0, 1, 0.05))]
all_pass = all_pass[(passType == "passHand" & abs(passLength) > 70) == FALSE]

# Removes some weird passes in the data
all_pass = all_pass[angleEnd < 100]

# Passes to the exact coordinates of the corners [0,0] and [105, 65] seems a bit weird 
# (often long and with a very low conversion rate, but just for these two corners not all four)
all_pass = all_pass[(x2 == 0 & y2 == 0) == FALSE]
all_pass = all_pass[(x2 == 105 & y2 == 65) == FALSE]

# Improved model
logit_features = glm(outcome ~
                       passLength + I(passLength^2) + I(passLength^3) +
                       distOrigin + I(distOrigin^2) +
                       angleCenterDiff + I(angleCenterDiff^2) +  I(angleCenterDiff^3) +
                       passType +
                       inBoxEnd +
                       angleCenterOrigin +
                       angleOrigin +
                       angleEnd + I(angleEnd^2) +
                       centerDist +
                       centerDistEnd,
                     data = all_pass, family = "binomial")
summary(logit_features)
# save(logit_features, file = "passmodel.rda")

# Show example of passes and their predicted probability
predData = data.table(x1 = c(30, 30, 30, 30, 30,
                             18, 97, 10, 94, 40),
                      x2 = c(30, 45, 60, 75, 90, 
                             23, 99, 35, 94, 70),
                      y1 = c(45, 45, 45, 45, 45, 
                             28, 58, 35, 41, 32.5),
                      y2 = c(20, 20, 20, 20, 20,
                             23, 34, 60, 29, 32.5),
                      passType = c("passFoot", "passFoot", "passFoot", "passFoot", "passFoot",
                                   "passHead", "cross", "passHand", "passFoot", "passFoot"),
                      group = c(1, 1, 1, 1, 1,
                                2, 2, 2, 2, 2))

predData[, distOrigin := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
predData[, distEnd := round(sqrt((x2 - 105)^2 + (y2- 32.5)^2), 2)]

# Angle to goal
predData[, angleOrigin := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
predData[angleOrigin < 0, angleOrigin := angleOrigin + pi]
predData[, angleOrigin := angleOrigin * 180 / pi]

predData[, angleEnd := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 32.5)^2 - (7.32/2)^2))]
predData[angleEnd < 0, angleEnd := angleOrigin + pi]
predData[, angleEnd := angleEnd * 180 / pi]

predData[, inBoxOrigin := ifelse(x1 >= 88.5 & y1 >= 12.35 & y1 <= 52.65, TRUE, FALSE)]
predData[, inBoxEnd := ifelse(x2 >= 88.5 & y2 >= 12.35 & y2 <= 52.65, TRUE, FALSE)]

# Angle to center of goal
predData[, angleCenterOrigin := atan2((105 - x1) - 0, abs(y1 - 32.5) - 0) * 180 / pi]
predData[, angleCenterEnd := atan2((105 - x2) - 0, abs(y2 - 32.5) - 0) * 180 / pi]

predData[, passLength := abs(distEnd - distOrigin)]
predData[, angleCenterDiff := angleCenterEnd - angleCenterOrigin]

# Distance to center
predData[, centerDist := abs(y1 - 32.5)]
predData[, centerDistEnd := abs(y2 - 32.5)]

# Make prediction
predData[, passProb := predict(logit_features, newdata = predData, type = "response")]

# Visualize passes and predictions
ggPitch +
  geom_segment(data = predData[group == 1], aes(x = x1, xend = x2, y = y1, yend = y2),
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_label(data = predData[group == 1], aes(x = x2, y = y2, label = round(passProb, 2)), nudge_y = 2, size = 4)

ggPitch +
  geom_segment(data = predData[group == 2], aes(x = x1, xend = x2, y = y1, yend = y2),
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_label(data = predData[group == 2],
             aes(x = x2, y = y2, label = paste(passType, "\n", round(passProb, 2))), nudge_y = 4, size = 4)


# Difficult / valuable passes : Passes into the final third with probability < median

  # Predict all passes
all_pass[, predProb := round(predict(logit_features, type = "response"), 4)]

# In final third or not
all_pass[, finalThird := ifelse(x2 >= 105 * (2/3), 1, 0)]

# Rank players
  # Passes in or to final third
playerEval = all_pass[finalThird == 1,
                      .(predProb = mean(predProb), meanCompl = mean(outcome), passes = .N),
                      playerId][passes > 300]

# Hard passes, prob < median
hard_passes = all_pass[finalThird == 1 & predProb < quantile(predProb, 0.5),
                     .(hardPassesProb = mean(predProb), meanComplHardPasses = mean(outcome), hardPasses = .N),
                     playerId][hardPasses > 200]

playerEval = merge(playerEval, hard_passes, by = "playerId")

# Better or worse than predicted?
playerEval[, complDiff := round(meanCompl - predProb, 3)]
# Success rate for hard passes
playerEval[, hardDiff := round(meanComplHardPasses - hardPassesProb, 3)]

# Load player data 
players <- data.table(fromJSON(txt = "wyscout/players.json"))

# Merge ranking with player data
selPlayer = players[wyId %in% playerEval[order(-meanComplHardPasses)][1:5, playerId],
                    .(firstName, lastName, playerId = wyId)]

selPlayer = merge(playerEval, selPlayer, by = "playerId")
# Order by hard passes success
selPlayer = selPlayer[order(-meanComplHardPasses), .(predProb, meanCompl, passes, hardPasses, hardPassesProb, meanComplHardPasses, firstName, lastName)]

print(selPlayer)

# Fix names
selPlayer[, name := paste(firstName, lastName)]
selPlayer[2, name := "Sergi Roberto"]
selPlayer[5, name := "Rodri"]
selPlayer[4, name := "Luka Modric"]

# player pass plots

kroos = all_pass[playerId == playerEval[order(-meanComplHardPasses)][1, playerId]]
lobotka = all_pass[playerId == playerEval[order(-meanComplHardPasses)][3, playerId]]

# Kroos
  # 5x5 grid
kroos[, GridX := cut(x = x1, breaks = 5, labels = FALSE)]
kroos[, GridY := cut(x = y1, breaks = 5, labels = FALSE)]
kroos[, startMinX := (GridX-1) / 5 * 105]
kroos[, startMaxX := GridX / 5 * 105]
kroos[, startMinY := (GridY-1) / 5 * 65]
kroos[, startMaxY := GridY / 5 * 65]
  # calculate pass angle 
kroos[, passAngle := atan2(y2 - y1, x2 - x1) * -1]
kroos[, quantile(passAngle)]
kroos[, passAngle2 := mround(passAngle, 0.5)]

# Average distance and count for different directions
kroos_summary = kroos[, .(passLength = mean(passLength), .N), .(startMinX, startMaxX,  startMinY, startMaxY, passAngle2)][N > 5]
kroos_summary[, avgEndX := (startMinX+startMaxX)/2 + passLength * cos(passAngle2)]
kroos_summary[, avgEndY := (startMinY+startMaxY)/2 + passLength * sin(passAngle2) * -1]

# Lobotka
# 5x5 grid
lobotka[, GridX := cut(x = x1, breaks = 5, labels = FALSE)]
lobotka[, GridY := cut(x = y1, breaks = 5, labels = FALSE)]
lobotka[, startMinX := (GridX-1) / 5 * 105]
lobotka[, startMaxX := GridX / 5 * 105]
lobotka[, startMinY := (GridY-1) / 5 * 65]
lobotka[, startMaxY := GridY / 5 * 65]

lobotka[, passAngle := atan2(y2 - y1, x2 - x1) * -1]
lobotka[, quantile(passAngle)]
lobotka[, passAngle2 := mround(passAngle, 0.5)]

# Average distance and count for different directions
lobotka_summary = lobotka[, .(passLength = mean(passLength), .N), .(startMinX, startMaxX,  startMinY, startMaxY, passAngle2)][N > 5]
lobotka_summary[, avgEndX := (startMinX+startMaxX)/2 + passLength * cos(passAngle2)]
lobotka_summary[, avgEndY := (startMinY+startMaxY)/2 + passLength * sin(passAngle2) * -1]

  # max pass length, used for normalization of arrows in plots
passLengthMax = max(c(kroos_summary[, max(passLength)], lobotka_summary[, max(passLength)]))

# Min-max normalization [1.5, 11.5]
kroos_summary[, passLength_n := ((passLength - min(passLength)) * (10)) / (passLengthMax - min(passLength)) + 1.5]
kroos_summary[, avgEndX_n := (startMinX+startMaxX)/2 + passLength_n * cos(passAngle2)]
kroos_summary[, avgEndY_n := (startMinY+startMaxY)/2 + passLength_n * sin(passAngle2) * -1]

lobotka_summary[, passLength_n := ((passLength - min(passLength)) * (10)) / (passLengthMax - min(passLength)) + 1.5]
lobotka_summary[, avgEndX_n := (startMinX+startMaxX)/2 + passLength_n * cos(passAngle2)]
lobotka_summary[, avgEndY_n := (startMinY+startMaxY)/2 + passLength_n * sin(passAngle2) * -1]

# Plots of all passes in or into final third, direction and relative length
ggPitch + 
  geom_rect(data = kroos_summary, aes(xmin = startMinX, xmax = startMaxX, ymin = startMinY, ymax = startMaxY),
            fill = "transparent", color = "seagreen") + 
  geom_segment(data = kroos_summary,
               aes(x = (startMinX+startMaxX)/2, xend = avgEndX_n, y = (startMinY+startMaxY)/2, yend = avgEndY_n, color = N),
               arrow = arrow(length = unit(0.02, "npc")), size = 1) + 
  scale_size(range = c(0.15, 1.5), name = NULL) +
  scale_color_viridis(option = "B", direction = -1, end = 0.8, name = "Number of\npasses") +
  labs(title = "Toni Kroos - pass distribution", subtitle = "Arrow length is normalized")

ggPitch + 
  geom_rect(data = lobotka_summary, aes(xmin = startMinX, xmax = startMaxX, ymin = startMinY, ymax = startMaxY),
            fill = "transparent", color = "seagreen") + 
  geom_segment(data = lobotka_summary,
               aes(x = (startMinX+startMaxX)/2, xend = avgEndX_n, y = (startMinY+startMaxY)/2, yend = avgEndY_n, color = N),
               arrow = arrow(length = unit(0.02, "npc")), size = 1) + 
  scale_size(range = c(0.15, 1.5), name = NULL) +
  scale_color_viridis(option = "B", direction = -1, end = 0.8, name = "Number of\npasses") +
  labs(title = "Stanislav Lobotka - pass distribution", subtitle = "Arrow length is normalized")


# Origin of passes into final third
kroos_ft = kroos[finalThird == 1 & predProb < quantile(predProb, 0.5), .(passLength = mean(passLength), .N), 
                 .(startMinX, startMaxX,  startMinY, startMaxY, passAngle2)][N > 5]
kroos_ft[, passLength_n := ((passLength - min(passLength)) * (10)) / (passLengthMax - min(passLength)) + 1.5]
kroos_ft[, avgEndX_n := (startMinX+startMaxX)/2 + passLength_n * cos(passAngle2)]
kroos_ft[, avgEndY_n := (startMinY+startMaxY)/2 + passLength_n * sin(passAngle2) * -1]

lobotka_ft = lobotka[finalThird == 1 & predProb < quantile(predProb, 0.5), .(passLength = mean(passLength), .N),
                   .(startMinX, startMaxX,  startMinY, startMaxY, passAngle2)][N > 5]
lobotka_ft[, passLength_n := ((passLength - min(passLength)) * (10)) / (passLengthMax - min(passLength)) + 1.5]
lobotka_ft[, avgEndX_n := (startMinX+startMaxX)/2 + passLength_n * cos(passAngle2)]
lobotka_ft[, avgEndY_n := (startMinY+startMaxY)/2 + passLength_n * sin(passAngle2) * -1]

  # Max number of passes, used for color scale in plots
maxN = max(c(kroos_ft[, max(N)], lobotka_ft[, max(N)]))

# Difficult passes 
ggPitch + 
  geom_rect(data = kroos_ft, aes(xmin = startMinX, xmax = startMaxX, ymin = startMinY, ymax = startMaxY),
            fill = "transparent", color = "seagreen") + 
  geom_segment(data = kroos_ft,
               aes(x = (startMinX+startMaxX)/2, xend = avgEndX_n, y = (startMinY+startMaxY)/2, yend = avgEndY_n, color = N),
               arrow = arrow(length = unit(0.02, "npc")), size = 1) + 
  scale_size(range = c(0.15, 1.5), name = NULL) +
  scale_color_viridis(option = "B", direction = -1, end = 0.8, name = "Number of\npasses") +
  labs(title = "Toni Kroos - difficult passes", subtitle = "Arrow length is normalized")


ggPitch + 
  geom_rect(data = lobotka_ft, aes(xmin = startMinX, xmax = startMaxX, ymin = startMinY, ymax = startMaxY),
            fill = "transparent", color = "seagreen") + 
  geom_segment(data = lobotka_ft,
               aes(x = (startMinX+startMaxX)/2, xend = avgEndX_n, y = (startMinY+startMaxY)/2, yend = avgEndY_n, color = N),
               arrow = arrow(length = unit(0.02, "npc")), size = 1) + 
  scale_size(range = c(0.15, 1.5), name = NULL) +
  scale_color_viridis(option = "B", direction = -1, end = 0.8, name = "Number of\npasses", limits = c(6, 57)) +
  labs(title = "Stanislav Lobotka - difficult passes", subtitle = "Arrow length is normalized")

