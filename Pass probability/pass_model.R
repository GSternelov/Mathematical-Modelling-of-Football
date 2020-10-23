
# Using wyscout data to train a model to do pass evaluation

library(jsonlite)
library(data.table)
library(ggplot2)
library(viridis)

# pitch
## Visualising actions ##
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




# Takes a few minutes..
# bundesliga <- fromJSON(txt = "wyscout/events/events_Germany.json")
# bundesliga <- data.table(bundesliga)
# save(bundesliga, file = "wy_bundesliga.rda")

# serieA <- fromJSON(txt = "wyscout/events/events_Italy.json")
# serieA <- data.table(serieA)
# save(serieA, file = "wy_seriea.rda")

# pl <- fromJSON(txt = "wyscout/events/events_England.json")
# pl <- data.table(pl)
# save(pl, file = "wy_pl.rda")

load("wy_bundesliga.rda")
load("wy_seriea.rda")
load("wy_pl.rda")

# Filter out passes
all_pass = rbindlist(list(bundesliga[eventName == "Pass"],
                          serieA[eventName == "Pass"],
                          pl[eventName == "Pass"]))
rm(list = c("pl", "bundesliga", "serieA"))
gc()

# Get the coordinates to columns
pass_coord = data.table(t(apply(all_pass, 1, function(x) unlist(x$position))))
all_pass[, names(pass_coord) := pass_coord]
  # Normalize pitch coordinates according to standard(?) dimension 105*65m  
all_pass[, y1 := y1 / 100 * 65]
all_pass[, y2 := y2 / 100 * 65]
all_pass[, x1 := x1 / 100 * 105]
all_pass[, x2 := x2 / 100 * 105]

  # Pass types
all_pass[, unique(subEventName)]
all_pass[subEventName %in% c("Simple pass", "Smart pass",  "High pass", "Launch"), passType := "passFoot"]
all_pass[subEventName == "Head pass", passType := "passHead"]
all_pass[subEventName == "Cross", passType := "cross"]
all_pass[subEventName == "Hand pass", passType := "passHand"]

# Look at heatmap of pass origins
all_pass[, GridX := cut(x = x1, breaks = 5, labels = FALSE)]
all_pass[, GridY := cut(x = y1, breaks = 5, labels = FALSE)]

passSummary = all_pass[, .N, .(GridX, GridY, passType)]

ggplot(passSummary, aes(x = GridX, y = GridY, fill = N)) + geom_tile() +
  scale_fill_viridis() + facet_wrap(~passType)

# Look at tags to determine outcome
  # 1801 == accurate
pass_tags = data.table(apply(all_pass, 1, function(x) unlist(x$tags)))
pass_tags[, id := 1:.N]
pass_tags[is.list(V1), outcome := any(unlist(V1) %in% 1801), id] 
all_pass[, outcome := pass_tags[, outcome]]
pass_tags[, mean(outcome)]

all_pass = all_pass[, .(subEventName, playerId, matchId, teamId, eventSec, id,
                        y1, y2, x1, x2, passType, outcome, GridX, GridY)]

passCompl = all_pass[, .(Mean = mean(outcome), .N), .(GridX, GridY)]

ggplot(passCompl[N > 1000], aes(x = GridX, y = GridY, fill = Mean)) + geom_tile() +
  scale_fill_viridis(limits = c(0, 1), option = "B")

# Additional features
  # Angle
# Angle to goal when making the pass?
# Combined with distance to the goal?
# Angle to goal when pass reach dest?
# Dist to goal when pass reach dest?

# Distance to center of goal
all_pass[, distOrigin := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
all_pass[, distEnd := round(sqrt((x2 - 105)^2 + (y2- 32.5)^2), 2)]

all_pass[, GridX := cut(x = x1, breaks = 10, labels = FALSE)]
all_pass[, GridY := cut(x = y1, breaks = 10, labels = FALSE)]

ggplot(all_pass[, .(mean(distOrigin)), .(GridX, GridY)],
       aes(x = GridX, y = GridY, fill = V1)) + geom_tile() +
  scale_fill_viridis()

# Angle to goal
all_pass[, angleOrigin := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
all_pass[angleOrigin < 0, angleOrigin := angleOrigin + pi]
all_pass[, angleOrigin := angleOrigin * 180 / pi]

all_pass[, angleEnd := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 32.5)^2 - (7.32/2)^2))]
all_pass[angleEnd < 0, angleEnd := angleOrigin + pi]
all_pass[, angleEnd := angleEnd * 180 / pi]

ggplot(all_pass[, .(median(angleOrigin)), .(GridX, GridY)],
       aes(x = GridX, y = GridY, fill = V1)) + geom_tile() +
  scale_fill_viridis()

all_pass[, inBoxOrigin := ifelse(x1 >= 88.5 & y1 >= 12.35 & y1 <= 52.65, TRUE, FALSE)]
all_pass[, inBoxEnd := ifelse(x2 >= 88.5 & y2 >= 12.35 & y2 <= 52.65, TRUE, FALSE)]
ggPitch + geom_point(data = all_pass[1:5000], aes(x = x1, y = y1, color = inBoxOrigin))

# Angle to center of goal (same regardless of distance to goal)

all_pass[, angleCenterOrigin := atan2((105 - x1) - 0, abs(y1 - 32.5) - 0) * 180 / pi]
all_pass[, angleCenterEnd := atan2((105 - x2) - 0, abs(y2 - 32.5) - 0) * 180 / pi]

ggplot(all_pass[, .(mean(angleCenterDiff)), .(GridX, GridY)],
       aes(x = GridX, y = GridY, fill = V1)) + geom_tile() +
  scale_fill_viridis()

# Angle toward goal and angle toward center? First is dependent on distance to goal, second is not
# Could be interesting to use both?
all_pass[, passLength := distEnd - distOrigin]
all_pass[, angleCenterDiff := abs(angleCenterEnd - angleCenterOrigin)]

ggplot(all_pass[, mean(outcome), .(angleCenterDiff = round(angleCenterDiff, 0))], aes(x = angleCenterDiff, y = V1)) +
  geom_point() + geom_line() + geom_smooth(formula = y ~ x + I(x^2) + I(x^3), method = "lm")

# Headed passes should not be longer than 40 m?
all_pass[passType == "passHead", quantile(passLength, probs = seq(0, 1, 0.05))]
all_pass = all_pass[(passType == "passHead" & abs(passLength) > 40) == FALSE]

# Hand passes should not be longer than 70 m?
all_pass[passType == "passHand", quantile(passLength, probs = seq(0, 1, 0.05))]
all_pass = all_pass[(passType == "passHand" & abs(passLength) > 70) == FALSE]


# Train logistic regression model model
bench_logit = glm(outcome ~ x1 + y1, data = all_pass, family = "binomial")
summary(bench_logit)
bench_logit$aic

# Add additional features
  # Type: passFoot, cross, passHead, passHand
  # Angle
  # End coordinates

endCoord_logit = glm(outcome ~ x1 + y1 + x2 + y2, data = all_pass, family = "binomial")
summary(endCoord_logit)
endCoord_logit$aic - bench_logit$aic

endCoord_passType_logit = glm(outcome ~ x1 + y1 + x2 + y2 + passType,
                              data = all_pass, family = "binomial")
summary(endCoord_passType_logit)
endCoord_passType_logit$aic - endCoord_logit$aic


dist_angle_logit = glm(outcome ~ distOrigin + I(distOrigin^2) + I(distOrigin^3) +
                         distEnd + I(distEnd^2) + I(distEnd^3) +
                         angleOrigin + I(angleOrigin^2) +
                         angleEnd + I(angleEnd^2) + 
                         distOrigin * angleOrigin +
                         distEnd * angleEnd +
                         inBoxOrigin + inBoxEnd +
                         passType,
                       data = all_pass, family = "binomial")
summary(dist_angle_logit)
dist_angle_logit$aic - endCoord_passType_logit$aic
exp(dist_angle_logit$coefficients)


logit_feature2 = glm(outcome ~ passLength + I(passLength^2) + I(passLength^3) + I(passLength^4) +
                       angleCenterDiff + I(angleCenterDiff^3) + I(angleCenterDiff^4) +
                       angleOrigin + I(angleOrigin^2) + angleEnd + I(angleEnd^2) + 
                       inBoxOrigin + inBoxEnd + 
                       passType +
                       passLength * angleCenterDiff,
                     data = all_pass, family = "binomial")
summary(logit_feature2)

logit_feature2$aic - dist_angle_logit$aic


predData = data.table(x1 = c(75, 20, 88, 25, 20),
                      x2 = c(90, 65, 97, 27, 100),
                      y1 = c(15, 45, 5, 45, 10),
                      y2 = c(28, 57, 36, 20, 60),
                      passType = c("passFoot", "passFoot", "cross", "passFoot", "passFoot"))
predData[, distOrigin := round(sqrt((x1 - 105)^2 + (y1- 32.5)^2), 2)]
predData[, distEnd := round(sqrt((x2 - 105)^2 + (y2- 32.5)^2), 2)]

predData[, angleOrigin := atan(7.32 * (105 - x1) / ((105 - x1)^2 + (y1 - 32.5)^2 - (7.32/2)^2))]
predData[angleOrigin < 0, angleOrigin := angleOrigin + pi]
predData[, angleOrigin := angleOrigin * 180 / pi]

predData[, angleEnd := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 32.5)^2 - (7.32/2)^2))]
predData[angleEnd < 0, angleEnd := angleOrigin + pi]
predData[, angleEnd := angleEnd * 180 / pi]

predData[, inBoxOrigin := ifelse(x1 >= 88.5 & y1 >= 12.35 & y1 <= 52.65, TRUE, FALSE)]
predData[, inBoxEnd := ifelse(x2 >= 88.5 & y2 >= 12.35 & y2 <= 52.65, TRUE, FALSE)]

predData[, angleCenterOrigin := atan2((105 - x1) - 0, abs(y1 - 32.5) - 0) * 180 / pi]
predData[, angleCenterEnd := atan2((105 - x2) - 0, abs(y2 - 32.5) - 0) * 180 / pi]

predData[, passLength := distEnd - distOrigin]
predData[, angleCenterDiff := angleCenterEnd - angleCenterOrigin]


predict(bench_logit, newdata = predData, type = "response")
predict(endCoord_logit, newdata = predData, type = "response")
predict(endCoord_passType_logit, newdata = predData, type = "response")
predict(dist_angle_logit, newdata = predData, type = "response")
predict(logit_feature2, newdata = predData, type = "response")

predData[, pred := predict(logit_feature2, newdata = predData, type = "response")]

ggPitch + geom_segment(data = predData, aes(x = x1, xend = x2, y = y1, yend = y2),
                       arrow = arrow(length = unit(2, "mm"))) +
  geom_text(data = predData, aes(x = x1, y = y1, label = round(pred, 2)), size = 3, nudge_x = 1, nudge_y = 1)

# Looks like there is some room for improvement?

all_pass[, lm_pred :=  predict(logit_feature2, type = "response")]
all_pass[, mean(lm_pred)]
all_pass[, mean(outcome)]

all_pass[, quantile(lm_pred, probs = c(0.005, 0.995))]

ggPitch + 
  geom_segment(data = all_pass[lm_pred > 0.9734510], aes(x = x1, xend = x2, y = y1, yend = y2),
               arrow = arrow(length = unit(2, "mm")), alpha = .25) + 
  facet_wrap(~passType)

ggPitch + 
  geom_segment(data = all_pass[lm_pred < 0.1447856], aes(x = x1, xend = x2, y = y1, yend = y2),
               arrow = arrow(length = unit(2, "mm")), alpha = .25) + 
  facet_wrap(~passType)


# Test some other type of model?  
  # knn, svm, random forest


