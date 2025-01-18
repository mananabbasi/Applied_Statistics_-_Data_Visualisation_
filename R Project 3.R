install.packages("datarium") 
install.packages("qqplotr") 
install.packages("ggplot2")
library(ggplot2) 
library(datarium) 
library(qqplotr) 

# Visually confirm if the plot appears to be on a normal curve 
ggplot(mapping = aes(sample=mice2$before)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")

random_sample <- rnorm(500, 0, 1) 
ggplot(mapping = aes(sample = random_sample)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")

random_sample2 <- rexp(500) 
ggplot(mapping = aes(sample = random_sample2)) + 
  stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + 
  xlab("Theoretical") + ylab("Sample")



#make this example reproducible 
set.seed(10) 
#Test 1 
#create data that follows a normal distribution 
normal_data <- rnorm(200) 
#perform shapiro-wilk test 
shapiro.test(normal_data)


#Test 2 

#create data that follows an exponential distribution 
non_normal_data <- rexp(200, rate=3) 

#perform shapiro-wilk test 
shapiro.test(non_normal_data)


set.seed(10) 

#Test 1 

#create data that follows a normal distribution 
normal_data <- rnorm(200) 

#perform kolmogorov-smirnov test 
ks.test(normal_data, 'pnorm')


df <- data.frame(y = rexp(100, rate = 0.1)) 
head(df)
hist(df$y, 
     main = "Histogram of Skewed 'y'", 
     xlab = "y", 
     col = "lightblue", 
     border = "black")

df$log_y <- log(df$y) 

ggplot(df, aes(x = log_y)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed 'y'", x = "log(y)", y = "Frequency")

df$sqrt_y <- sqrt(df$y) 
hist(df$sqrt_y, main = "Histogram of Square Root-transformed 'y'", xlab = 
       "sqrt(y)", col = "lightpink", border = "black") 


df$cube_root_y <- df$y^(1/3) 
# Visualize the cube root-transformed data 
hist(df$cube_root_y, main = "Histogram of Cube Root-transformed 'y'", xlab 
     = "y^(1/3)", col = "lightyellow", border = "black")

shapiro_test_y <- shapiro.test(df$y)
print("Shapiro-Wilk Test for Original y:")
print(shapiro_test_y)

# Set seed for reproducibility
set.seed(123)

# Create a data frame with exponentially distributed data
df <- data.frame(y = rexp(100, rate = 0.1))

# Apply log transformation
df$log_y <- log(df$y)

# Perform Shapiro-Wilk normality test on original y
shapiro_test_y <- shapiro.test(df$y)

# Perform Shapiro-Wilk normality test on log-transformed log_y
shapiro_test_log_y <- shapiro.test(df$log_y)

# Print the results
print("Shapiro-Wilk Test for Original y:")
print(shapiro_test_y)

print("Shapiro-Wilk Test for Log-Transformed log_y:")
print(shapiro_test_log_y)

choc_weight <-read.csv("chocolate_weights.csv",header=TRUE, row.names = 1) 

head(choc_weight)

summary(choc_weight) 

dim(choc_weight)
mean(choc_weight$chocolate_data)

sd(choc_weight$chocolate_data)
hist(choc_weight$chocolate_data) 


t.test(choc_weight$chocolate_data, mu=18, alternative="less") 
##  
## One Sample t-test 
##  
## data:  choc_weight$chocolate_data 
## t = -1.4102, df = 9, p-value = 0.09605 
## alternative hypothesis: true mean is less than 18 
## 95 percent confidence interval: 


data("mtcars") 
head(mtcars) 
##      -Inf 18.24483 
## sample estimates: ## mean of x  
##  17.18361 
# The p-value is 0.09605 so we cannot reject the null hypothesis 