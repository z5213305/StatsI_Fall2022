#################
# Problem set 1
# StudentID: TCD-22996761 , UCD-19211156
# StudentName: Duc Minh, Vu
#################

##### Set working directory #######
#setwd("C:/Users/Admin/Documents/GitHub/StatsI_Fall2022/problemSets/PS01/Assignment Submission")
setwd("C:/Users/Admin/Documents/GitHub/StatsI_Fall2022")
getwd()

################################################################################
##### Question 1 #####
### Create data set
y <-  c (105 , 69 , 86 , 100 , 82 , 111 , 104 , 110 , 87 , 108 , 87 , 90 , 94 , 113 , 112 , 98 , 80 , 97 , 95 , 111 , 114 , 89 , 95 , 126 , 98)

###### 1. Find 90% confidence interval for the average student IQ in the school
### 1.1. Calculate mean
#Manual calculation of mean
ybar_mnl <- sum(y)/length(y)
#Calculation of mean using R
ybar_r_cal <- mean(y) 
#Check mean
ybar_r_cal == ybar_mnl

### 1.2. Calculate variance and standard deviation
#Manual calculation of variance & standard deviation 
var_mnl <- (sum((y - ybar_mnl)^2))/(length(y)-1)
sd_mnl <- sqrt(var_mnl)
#Calculation of variance & standard deviation using R
var_r_cal <- var(y)
sd_r_cal <- sd(y)
#Check variance and standard deviation
var_r_cal == var_mnl
round(var_r_cal,4) == round(var_mnl,4) #rounding off the results
sd_r_cal == sd_mnl

### 1.3. Determine area under the curve
area_right <- (1 - 0.90)/2#Area under the curve to the right
area_left <- 0.90/2

### 1.4. Find Z-score associated with that number
z90 <- qnorm(area_right, lower.tail = FALSE)
t10 <- qt(p=0.1/2, df = length(y) - 1, lower.tail = F) #t-score
?qt()

### 1.5. Use these values to calculate confidence interval
#Z-score
SE <- sd_mnl/sqrt(length(y))
lower_90 <- ybar_mnl - (z90 * sd_mnl/sqrt(length(y)))
upper_90 <- ybar_mnl + (z90 * sd_mnl/sqrt(length(y)))
conf_int <- c(lower_90, upper_90)
#T-score
lower_90_t <- ybar_mnl - (t10 * sd_mnl/sqrt(length(y)))
upper_90_t <- ybar_mnl + (t10 * sd_mnl/sqrt(length(y)))
conf_int_t <- c(lower_90_t, upper_90_t)

###### 2. Whether student average student IQ is higher than average score (100) 
### 2.1. Set up hypothesis
#H0: ybar lesser than or equal to 100
#H1: ybar is larger than 100

### 2.2. Calculate test statistic
test_stat <- (ybar_mnl-100)/(SE)

### 2.3. Calculate P-value
#p.value_z_val <- pnorm(test_stat,lower.tail = FALSE)
p.value_t_val <- pt(test_stat,df=length(y)-1,lower.tail = F)

### 2.4. Draw a conclusion
p.value_z_val < 0.05
p.value_t_val < 0.05
#Fail to reject the null hypothesis

################################################################################
##### Question 2 #####
### Import data
setwd("C:/Users/Admin/Documents/GitHub/StatsI_Fall2022/datasets")
data.exp <- read.table("expenditure.txt", header = T)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Admin/Documents/GitHub/StatsI_Fall2022/problemSets/PS01/Assignment Submission/Graph")
### 1. Plot the relationships among Y, X1, X2, X3
png(filename = "1.corr_plot_all_vars.png",
    width = 600,
    height = 350)
pairs(~X1 + X2 + X3 + Y, data = data.exp, upper.panel = NULL)
dev.off()

cor(subset(data.exp,select = c(Y,X1,X2,X3)))
# 1.1 Relationship between Y and X1
png(filename = "1.1.corr_plot_Y_and_X1.png",
    width = 600,
    height = 350)
ggplot(data.exp,aes(x = X1, y = Y)) + 
  geom_point() +
  labs(x = "X1-per capita personal income", 
       y = "Y-per capita expenditure on housing assistance")
dev.off()
# 1.2 Relationship between Y and X2
png(filename = "1.2.corr_plot_Y_and_X2.png",
    width = 600,
    height = 350)
ggplot(data.exp,aes(x = X2, y = Y)) + 
  geom_point() +
  labs(x = "X2-Residents per 100,000 that are financially insecure", 
       y = "Y-per capita expenditure on housing assistance")
dev.off()
# 1.3 Relationship between Y and X3
png(filename = "1.3.corr_plot_Y_and_X3.png",
    width = 600,
    height = 350)
ggplot(data.exp,aes(x = X3, y = Y)) + 
  geom_point() +
  labs(x = "X3-People per 1,000 residing in urban areas", 
       y = "Y-per capita expenditure on housing assistance")
dev.off()
dev.off()
# 1.4 Relationship between X1 and X2
png(filename = "1.4.corr_plot_X1_and_X2.png",
    width = 600,
    height = 350)
ggplot(data.exp,aes(x = X1, y = X2)) + 
  geom_point() +
  labs(x = "X1-per capita personal income", 
       y = "X2-Residents per 100,000 that are financially insecure")
dev.off()
# 1.5 Relationship between X1 and X3
png(filename = "1.5.corr_plot_X1_and_X3.png",
    width = 600,
    height = 350)
ggplot(data.exp,aes(x = X1, y = X3)) + 
  geom_point() +
  labs(x = "X1-per capita personal income", 
       y = "X3-People per 1,000 residing in urban areas")
dev.off()
# 1.6 Relationship between X2 and X3
png(filename = "1.6.corr_plot_X2_and_X3.png",
    width = 600,
    height = 350)
ggplot(data.exp,aes(x = X2, y = X3)) + 
  geom_point() +
  labs(x = "X2-Residents per 100,000 that are financially insecure", 
       y = "X3-People per 1,000 residing in urban areas") 
dev.off()


### 2. Plot the relationships between Y and Region
data.exp$Region_name <- ifelse(data.exp$Region == 1, "1-Northeast",
                               ifelse(data.exp$Region == 2, "2-North Central",
                                      ifelse(data.exp$Region == 3, "3-South", 
                                             ifelse(data.exp$Region == 4, "4-West", "NA"))))

png(filename = "2.corr_plot_Y_and_Region.png",
    width = 600,
    height = 350)
ggplot(data.exp,
       aes(x = Region_name, y = Y, group = Region_name)) + 
  geom_boxplot() +
  labs(x = "Regions", y = "Y-per capita expenditure on housing assistance")
dev.off()

region_summary <- data.exp %>% 
  group_by(Region, Region_name) %>% 
  summarise(ave_per_capita = mean(Y), max_per_cap = max(Y), min_per_cap = min(Y))


### 3. Plot the relationship between Y and X1. Then added Region
#theme_set(
#  theme_minimal() +
#    theme(legend.position = "right"))

#data_group <- data.exp %>% 
#  group_by(Region)
#as.factor(data.exp$Region)
#data.exp$Region.F <- as.factor(data.exp$Region)

png(filename = "3.corr_plot_Y_and_X1_and_Region.png",
    width = 600,
    height = 350)
ggplot(data.exp,aes(x = X1, y = Y)) + 
  geom_point(aes(color = Region_name, shape = Region_name)) + 
  theme(legend.position = "top") +
  scale_color_manual(values = c("#D16103", "#56B4E9", "#FFDB6D", "#009E73")) +
  scale_shape_manual(values=c(2, 3, 18, 8)) +
  labs(colour = "Region:", 
       shape = "Region:",
       x = "X1-per capita personal income", 
       y = "Y-per capita expenditure on housing assistance")
dev.off()
