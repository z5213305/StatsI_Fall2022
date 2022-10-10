#################
# Problem set 2
# StudentID: TCD-22996761 , UCD-19211156
# StudentName: Duc Minh, Vu
#################

################################################################################
##### Question 1: Political science #####
### Create the dataset:
crossroad_data = matrix(data = c(14, 7, 6, 7, 7, 1), nrow = 2, ncol = 3)
rownames(crossroad_data) <- c("Upper class", "Lower class")
colnames(crossroad_data) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
crossroad_data_tbl <- as.table(crossroad_data)

### 1(a) Calculate chi-square statistic
#Calculating the row and column total
total <- sum(crossroad_data_tbl) #total sample size
row_total <- rowSums(crossroad_data_tbl) #row totals
col_total <- colSums(crossroad_data_tbl) #column totals

#Calculate expected values based on the total
exp_up_clss <- row_total[1]*col_total/total
exp_lw_clss <- row_total[2]*col_total/total
expected_value <- as.table(rbind(exp_up_clss,exp_lw_clss))
rownames(expected_value) <- c("Upper class", "Lower class")

#Calculate test statistic
mnl_chi_test_stat <- sum((abs(crossroad_data_tbl - expected_value))^2/expected_value) #
auto_chi_test_stat <- chisq.test(crossroad_data_tbl)

### 1(b) Calculate the p-value from the test statistic from 1(a)
#Calculate degree of freedom
df = (nrow(crossroad_data_tbl) - 1) * (ncol(crossroad_data_tbl) - 1)

#Use R to find p-value
p_value <- pchisq(mnl_chi_test_stat, df, lower.tail = FALSE)

### 1(c) Calculate the standardized residuals
#Calculate standard error for each expected value in each cell
se_1_1 <- expected_value[1]*(1 - row_total[1]/total)*(1 - col_total[1]/total) #row 1, col 1
se_1_2 <- expected_value[2]*(1 - row_total[2]/total)*(1 - col_total[1]/total) #row 2, col 1
se_2_1 <- expected_value[1,2]*(1 - row_total[1]/total)*(1 - col_total[2]/total) #row 1, col 2
se_2_2 <- expected_value[2,2]*(1 - row_total[2]/total)*(1 - col_total[2]/total) #row 2, col 2
se_3_1 <- expected_value[1,3]*(1 - row_total[1]/total)*(1 - col_total[3]/total) #row 1, col 3
se_3_2 <- expected_value[2,3]*(1 - row_total[2]/total)*(1 - col_total[3]/total) #row 2, col 3
#Create a table for these SE and taking their square roots
se <- matrix(data = c(se_1_1,se_1_2,se_2_1,se_2_2,se_3_1,se_3_2),nrow = 2, ncol = 3)
rownames(se) <- c("Upper class", "Lower class")
colnames(se) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
se <- as.table(sqrt(se))

#Calculate standardized residuals
std_res <- (crossroad_data_tbl - expected_value)/se
chisq.test(crossroad_data_tbl)$stdres
  

##### Question 2: Economics #####
###Import data:
female_policy_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
###Exploring the dataset
head(female_policy_data)
unique(female_policy_data$village)

### 2(b) Run a bivariate regression to test hypothesis
# A regression modelling with water as the independent/outcome variable and indication of seats reserved for woman
female_policy_model <- lm(data = female_policy_data, water ~ reserved)
summary(female_policy_model)



expectedValues <- outer(row_total, col_total, "*") / total

trafficViolations <- matrix(c(14, 6, 7, 7, 7, 1), byrow=T, nrow=2)
rownames(trafficViolations) <- c("Upper class", "Lower class")
colnames(trafficViolations) <- c("Not stopped", "Bribe", "Stopped/warned")
asdasd <- as.table(trafficViolations)

prop_expected_value <- prop.table(expected_value)
se <- sqrt(expected_value*(1 - row_total/total)*(1 - col_total/total))
se_2_1 <- row_total*col_total*((total - row_total)*(total - col_total)/total^3)
se_2_2 <- row_total[2]*col_total*((total - row_total[2])*(total - col_total)/total^3)
se_2 <- as.table(rbind(se_2_1, se_2_2))
