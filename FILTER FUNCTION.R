getwd()


install.packages("dplyr", dependencies = TRUE)
library(dplyr)
?dplyr

flights <- read.csv (file.choose())

flights <- read.csv ("flights.csv")# this requires working directory activation

flights <- read.csv (file.choose())# this will help you to choose your data from any folder



data <- data.frame (gender = c("male","female", "male","female","male"), status = c("won",
                              "lost", "won", "lost", "lost"))

class (data$gender)

str(data)
summary(data)
data$gender <- factor(data$gender)
data$gender <- as.factor(data$gender)

data$gender <- as.character(data$gender)
table (data$gender)
prop.table(table (data$gender))*100
prop.table (table (data$status))*100
attach(data)
prop.table (table (gender))

levels (data$gender)# this fucntion will work for identifying the levels of a factor vect
unique(data$gender)# this function will work for identifying the unique elements of a character vec

data$gender1<-factor(data$gender, levels = c("male", "female"), labels = c(1,0))

data$gender <- factor(data$gender1, levels = c("1","0"), labels = c(1, 2))







##finds all flights that departed in January in day one

#HERE I AM USING dplyr package
#filter function
#function(data, logical operator)
library(dplyr)
#function(data, condition)
str(flights$day)

data<- filter(flights, month == 1 & day == 1)
View (data)

##finds all flights that departed either in November or December
data2 <- filter(flights, month == 11 | month == 12)
data_2 <- filter(flights, month == 11 | month == 9)

##if you wanted to find flights that weren't delayed (on arrival or departure) by more than two hours

data3 <- filter(flights, arr_delay <= 120|dep_delay <= 120)
summary(data3)
data4 <-filter(flights, !arr_delay > 120 | !dep_delay > 120)


data5 <- filter(flights, arr_delay <= 120 & dep_delay <= 120)



summary(data5$arr_delay)
summary(data5$dep_delay)

data_5 <- filter(flights, arr_delay <= 0 & dep_delay <= 0)

data_5_1 <- filter(flights, arr_delay == 0 & dep_delay == 0)

##Had an arrival delay of two or more hours
data6<-filter(flights, arr_delay > 120)
View(data2)

##Flew to Houston (IAH or HOU)
str(data$dest)

data6<- filter (flights, dest == "IAH" |dest == "HOU")
View (data3)
##Were operated by United, American, or Delta
data7<- filter (flights, carrier == "UA" |carrier == "US"|carrier == "DL")


View(data4)

##Departed in summer (July, or August, or September)
str(data$month)
data8<- filter (flights, month == 7|month == 8|month ==9)
View (data5)

#Arrived more than two hours late, but didn't leave late
data6 <- filter (flights, (arr_delay > 120), !(dep_delay>= 0))
data_6 <- filter (flights, (arr_delay > 120) & !(dep_delay>= 0))
View(data6)
summary(flights$sched_dep_time)
##Schedule departure between midnight and 6 a.m. (inclusive)
data7 <- filter (flights, (sched_dep_time >= 00 & sched_dep_time <= 600))
data7
View(data7)
