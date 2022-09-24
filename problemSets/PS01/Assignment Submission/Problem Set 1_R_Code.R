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
#Calculation of variance & standard deviation using R
var_r_cal <- var(y)
sd_r_cal <- sd(y)
#Manual calculation of variance & standard deviation 
var_mnl <- (sum((y - ybar_mnl)^2))/(length(y)-1)
sd_mnl <- sqrt(var_mnl)
#Check variance and standard deviation
var_r_cal == var_mnl
round(var_r_cal,4) == round(var_mnl,4)
sd_r_cal == sd_mnl

### 1.3. Determine area under the curve
area_right <- (1 - 0.90)/2#Area under the curve to the right
area_left <- 0.90/2

### 1.4. Find Z-score associated with that number
z90 <- qnorm(area_right, lower.tail = FALSE)

### 1.5. Use these values to calculate confidence interval
lower_90 <- ybar_mnl - (z90 * sd_mnl/sqrt(length(y)))
upper_90 <- ybar_mnl + (z90 * sd_mnl/sqrt(length(y)))
conf_int <- c(lower_90, upper_90)

###### 2. Whether student average student IQ is higher than average score (100) 
### 2.1. Set up hypothesis
#H0: ybar lesser than or equal to 100
#H1: ybar is larger than 100

### 2.2. Calculate test statistic
test_stat <- (ybar_mnl-100)/(sd_mnl/sqrt(length(y)))

### 2.3. Calculate P-value
p.value_z_val <- pnorm(abs(test_stat),lower.tail = FALSE)
p.value_t_val <- pt(-abs(test_stat),df=length(y)-1)

### 2.4. Draw a conclusion
p.value_z_val < 0.05
p.value_t_val < 0.05
#Fail to reject the null hypothesis


################################################################################
##### Question 2 #####
### Import data
setwd("C:/Users/Admin/Documents/GitHub/StatsI_Fall2022/datasets")
data.exp <- read.table("expenditure.txt", header = T)

plot(data.exp$Y, data.exp$X1)
