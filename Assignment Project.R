install.packages("readxl")
install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("skimr")
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(ggplot2)
library(qqplotr)
library(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
library(skimr)

#Dataset
# Read the Excel file
concrete.compressive.strength <- read_excel("concrete compressive strength.xlsx", col_names = TRUE)
#Dataset<- read_excel("concrete compressive strength.xlsx", col_names = TRUE)

head(concrete.compressive.strength , 5)



# Rename the columns (adjust the number of names according to your dataset structure)
colnames(concrete.compressive.strength) <- c("Cement", 
                                             "Blast_Furnace_Slag", 
                                             "Fly_Ash", 
                                             "Water", 
                                             "Superplasticizer", 
                                             "Coarse_Aggregate", 
                                             "Fine_Aggregate", 
                                             "Age", 
                                             "Concrete_Category", 
                                             "Contains_Fly_Ash", 
                                             "Concrete_compressive_strength")

# Check the updated column names
head(concrete.compressive.strength, 5)



# Check the number of columns
ncol(concrete.compressive.strength)

# List the current column names 
colnames(concrete.compressive.strength)

str(concrete.compressive.strength)#dataframe types


# Load necessary library
library(dplyr)

# Check for NA values in each column
na_counts <- colSums(is.na(concrete.compressive.strength))
print(na_counts)



summary(concrete.compressive.strength)

#for better visualziation of summary
summary_table <- summary(concrete.compressive.strength)
kable(summary_table, format = "html", caption = "Summary Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))




# Apply the standard deviation function only to numeric columns
numeric_columns <- sapply(concrete.compressive.strength, is.numeric)
sapply(concrete.compressive.strength[, numeric_columns], sd, na.rm = TRUE)

#Now distribution of different Variables
#Cement

# Histogram
hist(concrete.compressive.strength$Cement, 
     probability=TRUE, 
     main="Cement Density and Histogram", 
     xlab="Cement Content", 
     col="lightgray", 
     border="black")

# Density plot
lines(density(concrete.compressive.strength$Cement), 
      col="blue", 
      lwd=2)


# Histogram
hist(concrete.compressive.strength$Age, 
     probability=TRUE, 
     main="Age Density and Histogram", 
     xlab="Age", 
     col="lightgray", 
     border="black")

# Density plot
lines(density(concrete.compressive.strength$Age), 
      col="blue", 
      lwd=2)



#for the Concrete_compressive_strength

ggplot(concrete.compressive.strength, aes(x = Concrete_compressive_strength)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Concrete Compressive Strength", 
       x = "Compressive Strength (MPa)")

#for the WATER

ggplot(concrete.compressive.strength, aes(x = Water)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Water", 
       x = "Water")

#for the Superplasticizer

ggplot(concrete.compressive.strength, aes(x = Superplasticizer)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Superplasticizer", 
       x = "Superplasticizer")




# For Numerical Variable sDistribution
install.packages("dplyr")     
install.packages("tidyverse") 
library(dplyr)    
library(ggplot2)  
library(tidyr)    

#IMP
concrete.compressive.strength %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numeric Variables",
       x = "Value", y = "Count")







# For Cetoegorical Varialbe


library(ggplot2)
#For Concerte Category
ggplot( concrete.compressive.strength, aes(x = Concrete_Category)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Concrete Categories",
       x = "Concrete Category",
       y = "Count")
#for Fly Ash 
ggplot( concrete.compressive.strength, aes(x = Contains_Fly_Ash)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Contain Fly_Ash",
       x = "Contains_Fly_Ash",s
       y = "Count")



#Part 2
#Co realtion between continious variables
#by Spearman Method

cor(concrete.compressive.strength$Cement, concrete.compressive.strength$Concrete_compressive_strength,method = "spearman")
cor(concrete.compressive.strength$Water,concrete.compressive.strength$Concrete_compressive_strength,method = "spearman")
cor(concrete.compressive.strength$Fly_Ash,concrete.compressive.strength$Concrete_compressive_strength,method = "spearman")
cor(concrete.compressive.strength$Blast_Furnace_Slag,concrete.compressive.strength$Concrete_compressive_strength,method = "spearman")
cor(concrete.compressive.strength$Superplasticizer,concrete.compressive.strength$Concrete_compressive_strength,method = "spearman")
cor(concrete.compressive.strength$Coarse_Aggregate, concrete.compressive.strength$Concrete_compressive_strength,method = "spearman")
cor(concrete.compressive.strength$Fine_Aggregate, concrete.compressive.strength$Concrete_compressive_strength,method = "spearman")



#Creating Corelation Matrix


library(corrplot)
library(dplyr)

numeric_vars <- concrete.compressive.strength %>% 
  select(Cement, Blast_Furnace_Slag, Fly_Ash, Water, Superplasticizer, 
         Coarse_Aggregate, Fine_Aggregate, Age, Concrete_compressive_strength)

# Calculate correlation matrix
cor_matrix <- cor(numeric_vars)

library(ggplot2)
library(reshape2)  
cor_long <- melt(cor_matrix)

# Plot with ggplot2
ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Correlation", x = "", y = "")


#Checking distribution between differnet variables and Concrete Strength

install.packages("corrplot")
library(corrplot)
library(dplyr)



library(ggplot2)


# Scatter plot: Cement vs Compressive Strength
ggplot(concrete.compressive.strength, aes(x = Cement, y = Concrete_compressive_strength)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Cement vs Compressive Strength", 
       x = "Cement (kg/m³)", y = "Compressive Strength (MPa)")




# Ensure that Concrete_Category is a factor (if it's not already)
concrete.compressive.strength$Concrete_Category <- as.factor(concrete.compressive.strength$Concrete_Category)

# Create the boxplot for concrete category
ggplot(concrete.compressive.strength, aes(x = Concrete_Category, y = Concrete_compressive_strength)) +
  geom_boxplot() +
  labs(title = "Compressive Strength by Concrete Category", 
       x = "Concrete Category", 
       y = "Compressive Strength (MPa)") +
  theme_minimal() 




#Scatter plot: Age vs Compressive Strength
ggplot(concrete.compressive.strength, aes(x = Age, y = Concrete_compressive_strength)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Age vs Compressive Strength", 
       x = "Age (days)", y = "Compressive Strength (MPa)")



# Scatter plot: Water vs Compressive Strength
ggplot(concrete.compressive.strength, aes(x = Water, y = Concrete_compressive_strength)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Water vs Compressive Strength", 
       x = "Water (kg/m³)", y = "Compressive Strength (MPa)")

#Bar plot: Contains Fly Ash
ggplot(concrete.compressive.strength, aes(x = Contains_Fly_Ash)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Samples Containing Fly Ash")

install.packages("dplyr")
library(dplyr)

# Water-Cement Ratio Column Creation

concrete.compressive.strength <- concrete.compressive.strength %>%
  mutate(Water_Cement_Ratio = Water / Cement)

install.packages("ggplot2")
library(ggplot2)
ggplot(concrete.compressive.strength, aes(x = Water_Cement_Ratio, y = Concrete_compressive_strength)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Water-Cement Ratio vs Compressive Strength", 
       x = "Water-Cement Ratio", y = "Compressive Strength (MPa)")









#Direction: The correlation is positive, meaning that as the amount of cement increases, 
#the concrete compressive strength tends to increase as well.
#value near to 1 mean high corealtion







# 3)Regression Testing

#Simple Linear Regression (SLR)
#Choosing cement as independent and Concrete strength as Dependent variable 

#We want to examine the possible linear relation between Concrete Strength and one IV. 
#To find out which IV is possibly better can explain the Concrete Strength  , we will build 
#a correlation matrix between all numerical variables (remind yourself that Person 
#Correlation and SLR are all about numerical variables). 



model_1 <-lm(Concrete_compressive_strength ~ Cement , concrete.compressive.strength) 
summary.lm(model_1) 

#Visualizing the fitted regression line
plot(Concrete_compressive_strength ~ Cement, concrete.compressive.strength, 
     col = "blue", 
     main = "Regression: Concrete Compressive Strength & Cement Content", 
     xlab = "Cement Content (kg/m³)", 
     ylab = "Concrete Compressive Strength (MPa)")

abline(model_1, col = "red")


#Checking Linearlity
#1)Scatter Plot
plot(concrete.compressive.strength$Cement, concrete.compressive.strength$Concrete_compressive_strength,
     xlab = "Cement Content (kg/m³)", 
     ylab = "Concrete Compressive Strength (MPa)",
     main = "Concrete Compressive Strength vs Cement",
     col = "blue", pch = 16)

# Add the regression line
abline(lm(Concrete_compressive_strength ~ Cement, data = concrete.compressive.strength), col = "red")
lines(lowess(concrete.compressive.strength$Cement, concrete.compressive.strength$Concrete_compressive_strength), col = "blue")
#2 Residuals’ Independence
plot(model_1, 1)
#3 Normality of residuals:
plot(model_1, 2)
#4 Equal variances of the residuals (Homoscedasticity) 
plot(model_1, 3) 

# 5: Report the results 
#All 4 assumptions were approved, and we can confirm that the fitted regression line is: 
#formula for SLR  �
#Concrete_compressive_strength= 13.442795 +  0.079580 × Cement

#2 now for MLR(Non Linear Regression)
#I will Use cement , Age and Super plaster

# Visualize correlation matrix with enhanced aesthetics

ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Correlation", x = "", y = "")

#with 4 IV
model_19 <-lm(Concrete_compressive_strength ~ Cement +  Age + Blast_Furnace_Slag +  
                Superplasticizer, concrete.compressive.strength) 
summary.lm(model_19) 

#with 6 IV

model_2 <-lm(Concrete_compressive_strength ~ Cement + Blast_Furnace_Slag + 
               Age + Fly_Ash + Water + Superplasticizer, concrete.compressive.strength) 
summary.lm(model_2) 

#With 8 IV

model_11 <-lm(Concrete_compressive_strength ~ Cement + Blast_Furnace_Slag +  Age + Fly_Ash
              + Water + Superplasticizer + Coarse_Aggregate 
              + Fine_Aggregate, concrete.compressive.strength) 
summary.lm(model_11)
#final Equation
#Concrete_compressive_strength = 35.491872+ 0.081291×Cement+ 0.060130×Blast_Furnace_Slag+0.109262×Age
#−0.197010× Water   + 0.613852×Superplasticizer  

#1. Linearity:

#imp
column_names <- colnames(concrete.compressive.strength)   # Get column names
indices <- seq_along(column_names)                        # Get column indexes
column_info <- data.frame(Index = indices, Name = column_names)
print(column_info)

pairs(concrete.compressive.strength[,c(11,1,2,3,4,5,6,7,8)], lower.panel = NULL, pch = 19,cex = 0.2) 

#As the linearity is not match in Blast Furnace, Age and Water Applying Transformation
# Create a new column with the log of 'Age' , 'Water' , 'Blast_Furnace_Slag'
concrete.compressive.strength$log_Blast_Furnace_Slag <- log(concrete.compressive.strength$Blast_Furnace_Slag)
concrete.compressive.strength$log_Water <- log(concrete.compressive.strength$Water)
concrete.compressive.strength$log_Age <- log(concrete.compressive.strength$Age)

#Now Again Making the Model using the transformation

concrete.compressive.strength = concrete.compressive.strength

# Assuming df is your dataframe
# Replace NaN values with NA
concrete.compressive.strength[sapply(concrete.compressive.strength, is.nan)] <- NA

# Replace Inf values with NA
concrete.compressive.strength[sapply(concrete.compressive.strength, is.infinite)] <- NA

# Remove rows with any NA values
concrete.compressive.strength <- na.omit(concrete.compressive.strength)

# Now you can proceed with your analysis or modeling

model_transformed <-lm(Concrete_compressive_strength ~ Cement + log_Blast_Furnace_Slag +  log_Age + log_Water , concrete.compressive.strength) 
summary.lm(model_transformed) 


#imp
column_names <- colnames(concrete.compressive.strength)   # Get column names
indices <- seq_along(column_names)                        # Get column indexes
column_info <- data.frame(Index = indices, Name = column_names)
print(column_info)


pairs(concrete.compressive.strength[,c(11,1,13,14,15)], lower.panel = NULL, pch = 19,cex = 0.2) 

#AS tHE LINEARIT IS NOT MATCHED SO I will be using the Gam Model

#GAM Model
library(mgcv)
# Cement is linear , rest are not linear so I write s FUNCTION with them
gam_model <- gam(Concrete_compressive_strength ~ Cement + s(Blast_Furnace_Slag) + s(Superplasticizer) + s(Water) + s(Age) +s(Fly_Ash) + s(Coarse_Aggregate) + s(Fine_Aggregate) , data = concrete.compressive.strength, method = 'REML')
summary(gam_model)

plot(gam_model, pages = 1, shade = TRUE, seWithMean = TRUE)




#Now Part 3 for Regression
#Logistic Regression in R

#We want to fit the possible logistic regression between the  binary variable as the 
#dependent variable, DV, and multiple possible Independent Variables, IVs
#imp
column_names <- colnames(concrete.compressive.strength)   # Get column names
indices <- seq_along(column_names)                        # Get column indexes
column_info <- data.frame(Index = indices, Name = column_names)
print(column_info)






# Check for separation



#I will be giving a specific value in to concerte_STrength as High or Low as In logistic regression the 
#dependent shiuld be categorical

concrete.compressive.strength <- concrete.compressive.strength %>%
  mutate(Concrete_compressive_strength_Category = cut(Concrete_compressive_strength, 
                                                      breaks = c(0, 36, 84),
                                                      labels = c("Low", "High"),
                                                      include.lowest = TRUE,
                                                      right = FALSE))



# Display the first 20 rows of specified columns
head(concrete.compressive.strength[, c("Concrete_compressive_strength_Category", "Concrete_compressive_strength")], 20)




# Now I will be running the Logistic Regresion:
install.packages("detectseparation")  # If not already installed
library(detectseparation)







# We will  choose  model WHICH ATC Value is less  i.e model_logistic_simplified

#Logistic Model for all Independent varialbes
model_logistic_simplified <- glm(Concrete_compressive_strength_Category ~ Cement + Water + Fly_Ash +
                                   Blast_Furnace_Slag + Superplasticizer + Age + Fine_Aggregate +
                                   Coarse_Aggregate + Concrete_Category,
                                 data = concrete.compressive.strength,
                                 family = "binomial")

summary(model_logistic_simplified)

#Logistic Model for 5 Independent varialbes highly significant
 model_logistic_simplified_3IV <- glm(Concrete_compressive_strength_Category ~ Cement  + Water
                                      + Blast_Furnace_Slag+ Superplasticizer + Age,
                                                                         data = concrete.compressive.strength,
                                                                        family = "binomial")
 
 summary(model_logistic_simplified_3IV)







#Now checking the assumption

probs <- predict(model_logistic_simplified, data=concrete.compressive.strength,type="response")  
concrete.compressive.strength$probs <- probs 

logits <- log(probs/(1-probs)) 
concrete.compressive.strength$logits <- logits 

data.frame(colnames(concrete.compressive.strength)) 

# Assuming you have already loaded your data into R and it is named Concrete.compressive.strength.with.age

#If I pick only 3 vaiables cement Age and superplasticer there is linear relationship
# Create scatterplot matrix for all columns
pairs(concrete.compressive.strength[, c(15, 1,2,3,4,5,6,7,8,9)],
      lower.panel = NULL,         # No plots in the lower panel
      upper.panel = panel.smooth, # Smooth lines in the upper panel
      pch = 19,                   # Use filled circle points
      cex = 0.2                   # Reduce point size for clarity
)

plot(model_logistic_simplified, which = 4, id.n = 3) 



install.packages("car")
# Install the car package if you haven't already
library(car)
vif(model_logistic_simplified)








#As my muticolinearity is not match so I will apply scaling
 
#First Scaling the VALUES as the VIF values for more than 5 so I scaled the IV

concrete.compressive.strength$Cement_scaled <- scale(concrete.compressive.strength$Cement)
concrete.compressive.strength$Blast_Furnace_Slag_scaled <- scale(concrete.compressive.strength$Blast_Furnace_Slag)
concrete.compressive.strength$Water_scaled <- scale(concrete.compressive.strength$Water)
concrete.compressive.strength$Fine_Aggregate_scaled <- scale(concrete.compressive.strength$Fine_Aggregate)
concrete.compressive.strength$Coarse_Aggregate_scaled <- scale(concrete.compressive.strength$Coarse_Aggregate)



column_names <- colnames(concrete.compressive.strength)   # Get column names
indices <- seq_along(column_names)                        # Get column indexes
column_info <- data.frame(Index = indices, Name = column_names)
print(column_info)


#Logistic Model for Scaled Independent varialbes
model_logistic_simplified_Scaled <- glm(Concrete_compressive_strength_Category ~ Cement_scaled + 
                                        Blast_Furnace_Slag_scaled + Concrete_Category + Contains_Fly_Ash +
                                          Superplasticizer + Water_scaled + Age + Fine_Aggregate_scaled + Coarse_Aggregate_scaled,
                                   data = concrete.compressive.strength,
                                   family = "binomial")

summary(model_logistic_simplified_Scaled)





probs <- predict(model_logistic_simplified_Scaled, data=concrete.compressive.strength,type="response")  
concrete.compressive.strength$probs <- probs 

logits <- log(probs/(1-probs)) 
concrete.compressive.strength$logits <- logits 

data.frame(colnames(concrete.compressive.strength)) 

# Ensure you are selecting columns by their numeric indices or by column names
pairs(concrete.compressive.strength[, c(15, 5, 9, 16, 17, 18, 19, 20)])



plot(model_logistic_simplified_Scaled, which = 4, id.n = 3) 


#Multicollinearity 

vif(model_logistic_simplified_Scaled)




#4Hypothesis Testing




#Conducting Normalization test for the variables and evaluate the results in the context of your chosen scenario. 

install.packages("ggplot2")
library(ggplot2)


ggplot(mapping = aes(sample = concrete.compressive.strength$Cement)) +
  stat_qq(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  xlab("Theoretical_Cement") +
  ylab("Sample_Cement")

ggplot(mapping = aes(sample = concrete.compressive.strength$Water)) +
  stat_qq(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  xlab("Theoretical_Water") +
  ylab("Sample_Water")

ggplot(mapping = aes(sample = concrete.compressive.strength$Fly_Ash)) +
  stat_qq(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  xlab("Theoretical_Fly_Ash") +
  ylab("Sample_Fly_Ash")

ggplot(mapping = aes(sample = concrete.compressive.strength$Superplasticizer)) +
  stat_qq(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  xlab("Theoretical_Superplasticizer") +
  ylab("Sample_Superplasticizer")


ggplot(mapping = aes(sample = concrete.compressive.strength$Blast_Furnace_Slag)) +
  stat_qq(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  xlab("Theoretical_Blast_Furnace_Slag") +
  ylab("Sample_Blast_Furnace_Slag")

#Conducting Shapiro Test for Normality if p value less than 0.05 Data is not Normal, we reject the null hypo that the data is normal
shapiro.test(concrete.compressive.strength$Cement)
shapiro.test(concrete.compressive.strength$Water)
shapiro.test(concrete.compressive.strength$Fly_Ash)
shapiro.test(concrete.compressive.strength$Blast_Furnace_Slag)
shapiro.test(concrete.compressive.strength$Superplasticizer)
shapiro.test(concrete.compressive.strength$Age)
shapiro.test(concrete.compressive.strength$Coarse_Aggregate)
shapiro.test(concrete.compressive.strength$Fine_Aggregate)



# Conducting Shapiro Test for Normality for better view
cement_result <- shapiro.test(concrete.compressive.strength$Cement)
water_result <- shapiro.test(concrete.compressive.strength$Water)
fly_ash_result <- shapiro.test(concrete.compressive.strength$Fly_Ash)
slag_result <- shapiro.test(concrete.compressive.strength$Blast_Furnace_Slag)
superplasticizer_result <- shapiro.test(concrete.compressive.strength$Superplasticizer)
age_result <- shapiro.test(concrete.compressive.strength$Age)
coarse_aggregate_result <- shapiro.test(concrete.compressive.strength$Coarse_Aggregate)
fine_aggregate_result <- shapiro.test(concrete.compressive.strength$Fine_Aggregate)

# Creating a Data Frame to Store Results
shapiro_results <- data.frame(
  Variable = c("Cement", "Water", "Fly_Ash", "Blast_Furnace_Slag", 
               "Superplasticizer", "Age", "Coarse_Aggregate", "Fine_Aggregate"),
  W_Statistic = c(cement_result$statistic, water_result$statistic, 
                  fly_ash_result$statistic, slag_result$statistic,
                  superplasticizer_result$statistic, age_result$statistic, 
                  coarse_aggregate_result$statistic, fine_aggregate_result$statistic),
  P_Value = c(cement_result$p.value, water_result$p.value, 
              fly_ash_result$p.value, slag_result$p.value, 
              superplasticizer_result$p.value, age_result$p.value, 
              coarse_aggregate_result$p.value, fine_aggregate_result$p.value),
  Normality = ifelse(c(cement_result$p.value, water_result$p.value, 
                       fly_ash_result$p.value, slag_result$p.value, 
                       superplasticizer_result$p.value, age_result$p.value, 
                       coarse_aggregate_result$p.value, fine_aggregate_result$p.value) < 0.05, 
                     "Not Normal", "Normal")
)

# Printing the Table
print(shapiro_results)

#Remember the null hypothesis for both scenarios are, the data set has a normal distribution.

#non of the column matches the  normaly test

#Making Cement Normalization




hist(concrete.compressive.strength$Cement, main = "Histogram of Skewed 'y'", xlab = "y", col =
                                      "lightblue", border = "black")

concrete.compressive.strength$log_y <- log(concrete.compressive.strength$Cement)

hist(concrete.compressive.strength$log_y, main = "Histogram of Log-transformed 'y'", xlab = "log(y)",
     col = "lightgreen", border = "black")

concrete.compressive.strength$sqrt_y <- sqrt(concrete.compressive.strength$Cement)


hist(concrete.compressive.strength$sqrt_y, main = "Histogram of Log-transformed 'y'", xlab = "log(y)",
     col = "lightgreen", border = "black")


concrete.compressive.strength$cube_root_y <- concrete.compressive.strength$Cement^(1/3)

hist(concrete.compressive.strength$cube_root_y, main = "Histogram of Cube Root-transformed 'y'", xlab 
     = "y^(1/3)", col = "lightyellow", border = "black") 


ggplot(mapping = aes(sample = concrete.compressive.strength$cube_root_y)) +
  stat_qq(size = 2, color = "blue") +
  stat_qq_line(color = "orange") +
  xlab("Theoreticalcube_root_y") +
  ylab("Samplecube_root_y")


#



#Non Parametric ttesr #Wilcox Test

 #FIRST DIviding the dataset into couarse and fine





# Subset the data by categories
coarse_data <- concrete.compressive.strength[concrete.compressive.strength$Concrete_Category == "Coarse", ]
fine_data <- concrete.compressive.strength[concrete.compressive.strength$Concrete_Category == "Fine", ]

#Checking Normality


#Now checking if the data is normaly distributed
ggplot(concrete.compressive.strength, aes(x = Concrete_compressive_strength, fill = Concrete_Category)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
  facet_wrap(~Concrete_Category) +
  theme_minimal() +
  labs(title = "Distribution of Compressive Strength by Concrete Category",
       x = "Compressive Strength", y = "Count")



# QQ Plot for Coarse

ggplot(coarse_data, aes(sample = Concrete_compressive_strength)) +
  stat_qq(size = 2, color = "blue") +  # Use stat_qq for the points
  stat_qq_line(color = "orange") +     # Use stat_qq_line for the reference line
  labs(title = "QQ Plot for Coarse Category", x = "Theoretical Quantiles", y = "Sample Quantiles")

# QQ Plot for Fine
ggplot(fine_data, aes(sample = Concrete_compressive_strength)) +
  stat_qq(size = 2, color = "red") +      # Use stat_qq for the points
  stat_qq_line(color = "orange") +        # Use stat_qq_line for the reference line
  labs(title = "QQ Plot for Fine Category", x = "Theoretical Quantiles", y = "Sample Quantiles")








cement_median_coarse <- median(coarse_data$Cement, na.rm = TRUE)  # na.rm=TRUE ignores NA values
cement_median_coarse #251.37



cement_median_fine <- median(fine_data$Cement, na.rm = TRUE)  # na.rm=TRUE ignores NA values
cement_median_fine #288.4


wilcox.test(Cement ~ Concrete_Category, data = concrete.compressive.strength)
#Null Hypothesis (H0): There is a difference in the distribution of Cement(Median) values between the two groups defined by Concrete_Category.
#Alternative Hypothesis (H1): There is no difference in the distribution of Cement(Median) values between the two groups.


#NonparametricAnova

#Hypothese#2
#Non Parametric Anova Testing

head(concrete.compressive.strength)

library(dplyr)

#Creating new data frame with 3 categories of Age column
concrete.compressive.strength <- concrete.compressive.strength %>%
  mutate(age_category = cut(Age, 
                            breaks = c(0, 120, 240, 365),
                            labels = c("A", "B", "C"),
                            include.lowest = TRUE,
                            right = FALSE))

head(concrete.compressive.strength)[c("Age", "age_category", "Concrete_compressive_strength")]


kruskal.test(Concrete_compressive_strength ~ age_category, data=concrete.compressive.strength)





# Null Hypothese to be rejected,The Age doesnot effect the Concrete strength

# Alternative to be approved By changing the age the concrete strentgh gets effected

#perfoming pOst Hoc test

install.packages("dunn.test")
library(dunn.test)


dunn_test <- dunn.test(concrete.compressive.strength$Concrete_compressive_strength, 
                       concrete.compressive.strength$age_category, 
                       method = "bonferroni")  


#NonparametricAnova 

#Hypothese#3
#Non Parametric Anova Testing


column_names <- colnames(concrete.compressive.strength)   # Get column names
indices <- seq_along(column_names)                        # Get column indexes
column_info <- data.frame(Index = indices, Name = column_names)
print(column_info)

head(concrete.compressive.strength)

library(dplyr)
install.packages("ggplot2")

library(ggplot2)

ggplot(concrete.compressive.strength, aes(x = Water)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Water in Concrete Compressive Strength",
       x = "Water",
       y = "Frequency")

# Breaknig the Water Column

concrete.compressive.strength <- concrete.compressive.strength %>%
  mutate(Water_category = cut(Water, 
                              breaks = c(121, 163, 245, 247),
                              labels = c("A", "B", "C"),
                              include.lowest = TRUE,
                              right = FALSE))


head(concrete.compressive.strength)[c("Water", "Water_category", "Concrete_compressive_strength")]


kruskal.test(Concrete_compressive_strength ~ Water_category, data=concrete.compressive.strength)


# Null Hypothese to be rejected,The Age doesnot effect the Concrete strength

# Alternative to be approved By changing the age the concrete strentgh gets effected

#Post Hoc test

dunn_test <- dunn.test(concrete.compressive.strength$Concrete_compressive_strength, 
                       concrete.compressive.strength$Water_category, 
                       method = "bonferroni")  




#Hypothese#4
#Non Parametric Anova Testing Superplasticizer 0-33

concrete.compressive.strength <- concrete.compressive.strength %>%
  mutate(Superplasticizer_category = cut(Superplasticizer, 
                              breaks = c(0, 11, 22, 33),
                              labels = c("A", "B", "C"),
                              include.lowest = TRUE,
                              right = FALSE))

head(concrete.compressive.strength)[c("Superplasticizer", "Superplasticizer_category", "Concrete_compressive_strength")]

kruskal.test(Concrete_compressive_strength ~ Superplasticizer_category, data=concrete.compressive.strength)



dunn_test <- dunn.test(concrete.compressive.strength$Concrete_compressive_strength, 
                       concrete.compressive.strength$Superplasticizer_category, 
                       method = "bonferroni")  

