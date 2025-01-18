data()
- cars

# Load the dataset
- cars

# View the first few rows of the dataset
head(cars)

# Summary statistics of the dataset
summary(cars)

plot(cars) 

hist(cars$speed)
hist(cars$dist) 

plot(density(cars$speed), 
     main = "Density Plot of Car Speeds",
     xlab = "Speed (mph)", 
     ylab = "Density",
     col = "blue")



Thai_tourist <- read.csv("C:\\Users\\rehma\\Desktop\\ASDV R Script\\ASDV LAB 4\\Thaitourism1.csv", header=TRUE)
names(Thai_tourist)  
head(Thai_tourist)  
tail(Thai_tourist)  
str(Thai_tourist) 
summary(Thai_tourist)


Thai_tourist_full <- read.csv("C:\\Users\\rehma\\Desktop\\ASDV R Script\\ASDV LAB 4\\Thaitourism2.csv", header= TRUE) 
names(Thai_tourist_full)  

head(Thai_tourist_full)  
tail(Thai_tourist_full)  
str(Thai_tourist_full) 
summary(Thai_tourist_full)  

Thai_2016<-Thai_tourist[Thai_tourist$Year==2016,] 
Thai_2016 


Thai_UK <- Thai_tourist_full[Thai_tourist_full$nationality == "UnitedKingdom", ]
Thai_UK


barplot(Thai_2016$Tourists_1000s,names.arg=Thai_2016$Region)

barplot(Thai_2016$Tourists_1000s, 
        names.arg = Thai_2016$Region, 
        col = "#FFCC00")  # Corrected color format
barplot(Thai_2016$Tourists_1000s, 
        names.arg = Thai_2016$Region, 
        col = rainbow(8), 
        main = "Number of Tourists by Region in 2016",
        xlab = "Region", 
        ylab = "Tourists (in thousands)")

hist(Thai_UK$tourists) 

hist(Thai_UK$tourists,breaks=16)
hist(Thai_UK$tourists, labels = TRUE) 
Thai_Europe <-droplevels(subset(Thai_tourist_full, region=="Europe")) 
boxplot(Thai_Europe$tourists, data=Thai_Europe)

boxplot(Thai_Europe$tourists, 
        main = "Boxplot of Tourists in Europe", 
        ylab = "Number of Tourists", 
        col = "lightblue")

boxplot(tourists~nationality, data=Thai_Europe,col=rainbow(8))
Thai_Europe

iris

install.packages("ggplot2")      
library(ggplot2) 

iris_data <- read.csv("C:\\Users\\rehma\\Desktop\\ASDV R Script\\ASDV LAB 4\\iris.csv", header = TRUE)
iris_data

head(iris_data) 
tail(iris_data) 
summary(iris_data) 

ggplot(iris_data, aes(petal_length, petal_width)) + 
  geom_point()

ggplot(iris_data, aes(petal_length, petal_width, colour = 
                        species)) + geom_point() 

ggplot(iris_data, aes(petal_length, petal_width)) + 
  geom_point() + geom_smooth(method="lm")


ggplot(iris_data, aes(petal_length)) +  geom_histogram() 


ggplot(iris_data, aes(petal_length, fill = Species)) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of Petal Length by Species",
       x = "Petal_length (cm)",
       y = "Count") +
  theme_minimal()

names(iris_data)



# Check the structure of iris_data
print(head(iris_data))  # This will show the first few rows of the data

# Check the column names to ensure they are correct
print(names(iris_data))

# Create the histogram
# Load the ggplot2 library
library(ggplot2)

# Create the histogram with the correct column names
ggplot(iris_data, aes(x = petal_length, fill = species)) + 
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of Petal Length by Species",
       x = "Petal Length (cm)",
       y = "Count") +
  theme_minimal()

ggplot(iris_data, aes(petal_length, species, fill = species)) + 
  geom_boxplot()
