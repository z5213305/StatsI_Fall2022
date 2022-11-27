#################
# Problem set 4
# StudentID: TCD-22996761 , UCD-19211156
# StudentName: Duc Minh, Vu
#################

#install.packages("car")
library(car)
data(Prestige)
help(Prestige)
library(texreg)

################################################################################
#####  Question 1:
### Part a: recoding variable `type`
summary(Prestige$type)
Prestige$professional <- as.factor(ifelse(Prestige$type == "prof", 1, 
                             ifelse(Prestige$type == "bc" | Prestige$type == "wc",0,NA)))
summary(Prestige$professional)

### Part b: linear model with prestige as outcome
presti_lm <- lm(data = Prestige, prestige~income + professional + income*professional)
summary(presti_lm)
texreg(presti_lm,
       caption = "Regressing prestige on income, professional type and their interaction effect",
       float.pos = "H", digits = 4)
presti_lm.coff <- presti_lm$coefficients #Saving the coefficients for subsequent sections

### Part f: effect on prestige score for a professional with in income of $1,000 
presti.part.f_1 <- 21.1423 + 1000*0.0032 + 1*37.7813 + 1*1000*(-0.0023)
presti.part.f_2 <- 21.1423 + 0*0.0032 + 1*37.7813 + 1*0*(-0.0023)
presti.part.f_diff <- presti.part.f_1 - presti.part.f_2
# presti_lm.coff[1] + 1000*presti_lm.coff[2] + 1*presti_lm.coff[3] + 1*1000*presti_lm.coff[4]

### Part g: Effect on prestige score from changing profession for a person with income of $6,000
presti.part.g.prof <- 21.1423 + 6000*0.0032 + 1*37.7813 + 1*6000*(-0.0023)
presti.part.g.non.prof <- 21.1423 + 6000*0.0032 + 0*37.7813 + 0*6000*(-0.0023)
presti.part.g.diff <- presti.part.g.prof - presti.part.g.non.prof

################################################################################
#####  Question 2:
### Part a: Hypothesis test for yard signs in a precinct 
test.stat.yard = 0.042/0.016
p.val.yard = 2*pt(test.stat.yard, df = 128, lower.tail = F)

### Part b: Hypothesis test for being next to precincts with these yard signs
test.stat.nxt = 0.042/0.013
p.val.nxt = 2*pt(test.stat.nxt, df = 128, lower.tail = F)

### Part c: Hypothesis test for being next to precincts with these yard signs
test.stat.con = 0.302/0.011
p.val.con = 2*pt(test.stat.con, df = 128, lower.tail = F)

