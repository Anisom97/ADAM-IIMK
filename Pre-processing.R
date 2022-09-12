
cars <- read.csv ("cars.csv")
carsn<-cars
View(cars)
carsn1 <- cars

sum (is.na (carsn))

#TO IDENTIFY THE NUMBER OF MISSING VALUES
sum(is.na(carsn))
#[2,2] =31.9 Us & 8

# IF ANY MISSING VALUES ARE THERE, SOMETIME WE CAN USE RAW WISE DELETION APPROACH
data3 <- na.omit(carsn)

## Missing DATA HANDLING (31.9, US, 8)
carsn <- edit(carsn)
carsn[2,2]<- NA
carsn[3,9] <- NA
carsn[4,3]<- NA
View(carsn)
sum(is.na(carsn))
summary(carsn)
carsn <-cars
###############

summary(carsn$mpg)
##################
#mean & median replacement
mean(carsn$mpg, na.rm = T)
carsn$mpg[which(is.na(carsn$mpg))]<- mean(carsn$mpg, na.rm = TRUE)
carsn$mpg[which(is.na(carsn$mpg))]<- median(carsn$mpg, na.rm = TRUE)
#########
Mode replacement
str(carsn)
carsn$brand <- as.character(carsn$brand)
carsn$brand <- as.factor(carsn$brand)
max(table (carsn$brand))

#####
carsn$brand[which(is.na(carsn$brand))]<- max(carsn$brand, na.rm = TRUE)
mode(carsn$brand)
#OR
carsn$brand[which(is.na(carsn$brand))]<- "US"

###random number/constant
carsn$cylinders[which(is.na(carsn$cylinders))]<-12

##DATA IMPUTATION
install.packages("mice")
?mice
library(mice)
carsn[2,2]<- NA
carsn[3,9] <- NA
carsn[4,3]<- NA
str(carsn)
mice
carsn$brand <- as.factor (carsn$brand)

md.pattern(carsn)
set.seed (1234)
impute <- mice(carsn[,2:9], m = 3,set.seed = 1234)
impute$imp$mpg
impute$imp$brand
impute$imp$cylinders
str(carsn1)

carsn1$brand <- as.factor(carsn1$brand)

# Complete data
newdata1 <- complete(impute, 1)
newdata2 <- complete(impute, 2)
newdata3 <- complete(impute, 3)
## GRAPHICAL METHODS FOR IDENTIFYING OUTLIERS
# Set up the plot area
par(mfrow = c(1,3))
# Create the histogram bars
summary(cars$weightlbs)
hist(cars$weightlbs,
     breaks = 30,
     xlim = c(1500, 5000),
     col = "blue",
     border = "black",
     ylim = c(0, 20),
     xlab = "Weight",
     ylab = "Counts",
     main = "Histogram
of Car Weights")
# Make a box around # the plot
box(which = "plot",
    lty = "solid",
    col = "black")

?plot

# Create a Scatterplot
?plot
plot(cars$weightlbs,
     cars$mpg,
     xlim = c(1500, 5000),
     ylim = c(0, 50),
     xlab = "Weight",
     ylab = "MPG",
     main = "Scatterplot of MPG by Weight",
     type = "p",
     pch = 20,
     col = "blue")
#Add open black
# circles
points(cars$weight,
       cars$mpg,
       type = "p",
       col = "black")
## can also draw box plot
boxplot(mpg ~ cylinders, data = carsn, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

box <- boxplot(mpg ~ brand, data = carsn, xlab = "brand",
        ylab = "Miles Per Gallon", main = "Mileage Data")

box
box <- boxplot(carsn$mpg,  xlab = "box",
               ylab = "Miles Per Gallon", main = "Mileage Data")$out


box

outliers_data1 <- carsn[which(carsn$mpg %in% box),]
outliers_data1
data2 <- carsn[-which(carsn$mpg %in% box),]

library(dplyr)

boxplot(data2$mpg)

outliers <- boxplot(carsn$mpg,plot=FALSE)$out
outliers
outliers_data <- carsn[which(carsn$mpg %in% outliers),]
outliers_data
data2 <- carsn[-which(carsn$mpg %in% outliers),]



boxplot(data2$mpg)
### DESCRIPTIVE STATISTICS

mean(cars$weightlbs) # Mean
median(cars$weightlbs) # Median
length(cars$weightlbs) # Number of observations
sd(cars$weightlbs) # Standard deviation
summary(cars$weightlbs) # Min, Q1,Median, Mean,Q3, Max

# Transformations
# Min-max normalization

summary(cars$weightlbs)
mi <- min(cars$weightlbs)
ma <- max(cars$weightlbs)
minmax.weight <- (cars$weightlbs - mi)/(ma - mi)
minmax.weight

# Z-score standardization
data2 <-scale(cars$weightlbs, center = TRUE, scale = TRUE)
#or

z.weight <- scale(cars$weight)
z.weight

# Decimal scaling
max(abs(cars$weightlbs)) # 4 digits
d.weight <- cars$weightlbs/(10^4)
d.weight

######## TRANSFORMATIONS FOR NAORMALITY

par(mfrow = c(1,2))
# Create two histograms
hist(cars$weightlbs, breaks = 20,
     xlim = c(1500, 5000),
     main = "Histogram of Weight",
     xlab = "Weight",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col = "black")

hist(z.weight,
     breaks = 20,
     xlim = c(-2, 3),
     main = "Histogram of Zscore
of Weight",
     xlab = "Z-score of Weight",
     ylab = "Counts")
box(which = "plot",
    lty = "solid",
    col = "black")

# Skewness
(3*(mean(cars$weightlbs) - median(cars$weightlbs)))/sd(cars$weightlbs)
(3*(mean(d.weight) - median(d.weight)))/sd(d.weight)

# Transformations for Normality
sqrt.weight <- sqrt(cars$weightlbs) # Square root
sqrt.weight_skew <- (3*(mean(sqrt.weight) - median(sqrt.weight))) / sd(sqrt.weight)
sqrt.weight_skew

ln.weight <- log(cars$weightlbs) # Natural log
ln.weight_skew <- (3*(mean(ln.weight) - median(ln.weight))) / sd(ln.weight)
ln.weight_skew

invsqrt.weight <- 1 / sqrt(cars$weightlbs) # Inverse square root
invsqrt.weight_skew <- (3*(mean(invsqrt.weight) - median(invsqrt.weight))) /sd(invsqrt.weight)
invsqrt.weight_skew

###Histogram with Normal Distribution Overlay
par(mfrow=c(1,1))
x <- rnorm(1000000,
           mean = mean
           (invsqrt.weight),
           sd = sd(invsqrt.weight))
hist(invsqrt.weight,
     breaks = 30,
     xlim=c(0.0125, 0.0275),
     col = "lightblue",
     prob = TRUE,
     border = "black",
     xlab="Inverse Square
Root of Weight",
     ylab = "Counts",
     main = "Histogram of
Inverse Square Root
of Weight")
box(which = "plot",
    lty = "solid",
    col="black")
# Overlay with
Normal density
lines(density(x), col="red")

# Normal Q-Q Plot
qqnorm(invsqrt.weight,
       datax = TRUE,
       col = "red",
       ylim = c(0.01, 0.03),
       main = "Normal
Q-Q Plot of Inverse
Square Root of Weight")
qqline(invsqrt.weight,
       col = "blue",
       datax = TRUE)


## Adding an index field
set.seed(1234)
x <- rnorm(1000, 5, .55)
y <- rnorm(1000, 4.5, .66)

data <- data.frame (x, y)

ID <- 1:nrow(data)

### Newdata with index field
data <- data.frame (ID, x, y)

data1<- data[order (x),]
View(data1)

********************************
shapiro.test(cars$weightlbs)
shapiro.test (ln.weight)


data <- rnorm (100,50,5.99)
View(data)
shapiro.test (data)


library(car)
qqPlot(data)
