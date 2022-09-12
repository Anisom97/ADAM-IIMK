rm(data5, data_5_1)

data <- read.csv ("flights.csv")
data <- flights
# arrange function. we need dplyr package


library(dplyr)

#function(e.g., filter, or arrange, or select, or mutate)(data, logic)

# for example, filter(data, DEST == "TTT")



?arrange

data1 <- arrange(data, year, month, day)


data2 <- arrange(data, desc(arr_delay))  
head(data2, 10)
View (mydata)

Exercises

## Sort flights to find the most delayed flights.
data2<- arrange(data, desc(dep_delay))
View(data2)

##Sort flights to find the fastest flights.

data9 <- arrange (data, desc(distance), air_time, arr_delay)

head(data9, 5)

## SELECT IS A FUCNTION IS PART OF DPLYR PACKAGE
attach(data)
detach(data)
arr_delay
data10 <- data.frame (FLIGHT = data$flight, DISTANCE = data$distance, 
                      airtime = data$air_time, arrdelay = data$arr_delay)

mydata <- select(data, flight, distance, air_time, arr_delay)

data22 <- select (data, flight, distance, air_time)
data10 <- select(data9, flight,distance, air_time, arr_delay)
View (data9)
##Which flights traveled the longest? Which traveled the shortest?


mydata <- data %>% filter(distance >= 1000) %>% select (flight, distance, air_time) %>%
  arrange(desc(distance))

mydata1 <-flights %>% select (flight, distance, air_time) %>% subset (air_time >= 100) %>%
  arrange(desc(distance))

  
