###############
# Data Analysis
###############

# Packages
#library() # load our packages here

# Import data
data <- read.csv("Data/height.csv")

# Explore data
summary(data)
str(data)
head(data)

# Visualise
hist(data$height,
     #breaks = 12,
     main = "Histogram of Height",
     xlab = "Height (cm)"
     )

plot(density(data$height),
     main = "Pdf of Height",
     xlab = "Height (cm)"
     )

?hist
?density

# Use a QQ plot to determine if our height variable is
# normally distributed
qqnorm(data$height)
qqline(data$height,
       distribution = qnorm)

?qqnorm

# Calculate 90 percent confidence intervals using normal distribution
CI_lower <- qnorm(0.05, 
                  mean = mean(data$height), 
                  sd = (sd(data$height)/sqrt(length(data$height))) # the equation for the standard error of the mean
)

CI_upper <- qnorm(0.95,
                  mean = mean(data$height),
                  sd = (sd(data$height)/sqrt(length(data$height)))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))

# Calculate 90 percent confidence intervals using a t distribution
se <- sd(data$height)/sqrt(length(data$height)) # Create an object with our standard error
t_score <- qt(.05, df = length(data$height)-1, lower.tail = FALSE)
CI_lower_t <- mean(data$height) - (se * t_score)
CI_upper_t <- mean(data$height) + (se * t_score)

# Check our working
t.test(data$height, conf.level = 0.9, alternative = "two.sided")
