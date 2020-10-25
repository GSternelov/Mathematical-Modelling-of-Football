source("Project/pre_process.R")


# Train xG model

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

# Visual check of relationship between outcome and dist
ggplot(shotsDT[, mean(outcome), .(distCenter = round(distCenter, 0))], aes(x = distCenter, y = V1)) +
  geom_point() + geom_line() + geom_smooth(formula = y ~ x + I(x^2), method = "lm")

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

ggPitch + 
  geom_point(data = testData, aes(x = x1, y = y1, color = xG), size = 2) +
  scale_color_viridis(alpha = 0.5, direction = -1, option = "B", limits = c(0.05, 1), na.value = "transparent") +
  labs(title = "xG model probabilities. Cut-off at 5 %")


# Player analysis
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
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x, aes(group = 1)) + 
  geom_point() +
  geom_text_repel(data = player_xG[xG_shot > quantile(xG_shot, 0.95) |
                                     xG_shot < quantile(xG_shot, 0.05) | 
                                     G_shot > quantile(G_shot, 0.95) |
                                     G_shot < quantile(G_shot, 0.1)], 
                  aes(label = shortName), size = 3)



# xA 
# passShots from passImpact is needed to get passes that lead to shots
load("Project/files/passShots.rda")
load("Project/files/shotActionPass.rda")

# Distance to center of goal
shotActionPass[, distCenter := round(sqrt((x2 - 105)^2 + (y2- 32.5)^2), 2)]
# Angle to goal
shotActionPass[, angleGoal := atan(7.32 * (105 - x2) / ((105 - x2)^2 + (y2 - 32.5)^2 - (7.32/2)^2))]
shotActionPass[angleGoal < 0, angleGoal := angleGoal + pi]
shotActionPass[, angleGoal := angleGoal * 180 / pi]

# add from shots if it was a header or not
shotActionPass[, head := passShots[, head]]
shotActionPass[, outcome := passShots[, outcome]]

shotActionPass[, xG := predict(xg_mod, newdata = shotActionPass, type = "response")]

player_xA = merge(shotActionPass,
                  players[, .(shortName, playerId = wyId, role.name, currentTeamId)],
                  by = "playerId")

player_xA = player_xA[, .(xA = sum(xG), A = sum(outcome), xAdiff = sum(outcome) - sum(xG),
                          passes = .N, xA_pass = sum(xG) / .N, A_pass = sum(outcome) / .N),
                      .(playerId, shortName, currentTeamId)]

player_xA = player_xA[passes > 20]


ggplot(player_xA, aes(x = xA, y = A, color = currentTeamId == 1612)) +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ x, aes(group = 1), color = "grey62") + 
  geom_point() +
  geom_text_repel(data = player_xA[abs(xA) > quantile(abs(xA), 0.9) | A > quantile(A, 0.95)], 
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

# Check again, quite different to previous results

# Looks like really low numbers compared to fbref

# xG also a bit low?

  # Do not use passes from corners or free kicks
  # Not shots from free kicks or penalties

# Files for markdown report 

save(player_xA, file = "Project/files/player_xA.rda")

save(player_xG, file = "Project/files/player_xG.rda")

save(testData, file = "Project/files/xG_testData.rda")

