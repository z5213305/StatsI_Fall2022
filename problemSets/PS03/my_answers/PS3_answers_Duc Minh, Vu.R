#################
# Problem set 3
# StudentID: TCD-22996761 , UCD-19211156
# StudentName: Duc Minh, Vu
#################

################################################################################
#####  Question 1:
### Part 1:
setwd("C:/Users/Admin/Documents/GitHub/StatsI_Fall2022/datasets")
incumb_data <- read.csv("incumbents_subset.csv")
library(ggplot2)
library(texreg)
setwd("C:/Users/Admin/Documents/GitHub/StatsI_Fall2022/problemSets/PS03/my_answers")

vote.lm <- lm(data = incumb_data, voteshare ~ difflog)
summary(vote.lm)
texreg(vote.lm, 
       caption = "Vote share (voteshare) and log differences in campaign spending (difflog)", 
       custom.model.names = "Model 1", 
       float.pos = "H", digits = 4)

### Part 2:
png(filename = "1.2. Scatter_plot_voteshare_difflog.png",
    width = 600,
    height = 350)
ggplot(data = incumb_data, aes(x = difflog, y = voteshare)) +
  geom_point(shape=1) +
  geom_smooth(method = "lm") +
  ylab("Vote share") + xlab("Log of differences in campaign spending") +
  theme_minimal()
dev.off()

### Part 3:
vote.lm.resid <- resid(vote.lm)

###############################################################################

#####  Question 2:
### Part 1:
presvote.lm <- lm(data = incumb_data, presvote ~ difflog)
summary(presvote.lm)
texreg(presvote.lm,
       caption = "Vote share of presidental candidate (presvote) and log differences in campaign spending (difflog)",
       custom.model.names = "Model 2", 
       float.pos = "H", digits = 4)

### Part 2:
png(filename = "2.2. Scatter_plot_presvote_difflog.png",
    width = 600,
    height = 350)
ggplot(data = incumb_data, aes(x = difflog, y = presvote)) +
  geom_point(shape=1) +
  geom_smooth(method = "lm") +
  ylab("Vote share of president candidate") + xlab("Log of differences in campaign spending") +
  theme_minimal()
dev.off()

### Part 3:
presvote.lm.resid <- resid(presvote.lm)

###############################################################################

#####  Question 3:
### Part 1:
vote.lm.2 <- lm(data = incumb_data, voteshare ~ presvote)
summary(vote.lm.2)
texreg(vote.lm.2,
       caption = "Incumbent's electoral success (voteshare) and Vote share of presidental candidate (presvote)",
       custom.model.names = "Model 3", 
       float.pos = "H", digits = 4)

### Part 2:
png(filename = "3.2. Scatter_plot_voteshare_presvote.png",
    width = 600,
    height = 350)
ggplot(data = incumb_data, aes(x = presvote, y = voteshare)) +
  geom_point(shape=1) +
  geom_smooth(method = "lm") +
  ylab("Vote share") + xlab("Vote share of president candidate") +
  theme_minimal()
dev.off()

###############################################################################

#####  Question 4:
### Part 1:
resid.lm <- lm(vote.lm.resid ~ presvote.lm.resid)
summary(resid.lm)
texreg(resid.lm,
       caption = "Question 1 residuals (vote.lm.resid) and Question 2 residuals (presvote.lm.resid)",
       custom.model.names = "Model 4",
       float.pos = "H", digits = 4)

### Part 2:
png(filename = "4.2. Scatter_plot_residuals.png",
    width = 600,
    height = 350)
ggplot(data = incumb_data, aes(x = presvote.lm.resid, y = vote.lm.resid)) +
  geom_point(shape=1) +
  geom_smooth(method = "lm") +
  ylab("Residuals from Question 1") + xlab("Residuals from Question 2") +
  theme_minimal()
dev.off()

###############################################################################

#####  Question 5:
### Part 1:
vote.lm.3 <- lm(data = incumb_data, voteshare ~ difflog + presvote)
summary(vote.lm.3)
texreg(vote.lm.3,
       caption = "Vote shares (voteshare) with difference in spending (difflog) and president's popularity (presvote)",
       custom.model.names = "Model 5",
       float.pos = "H", digits = 4)

### Part 2:
texreg(list(resid.lm, vote.lm.3),
       custom.model.names = c("Model 4","Model 5"),
       caption = "Comparison between model from Question 4 and 5", 
       float.pos = "H", digits = 4)

round(resid.lm$coefficients, digits = 4)
round(vote.lm.3$coefficients, digits =4)
