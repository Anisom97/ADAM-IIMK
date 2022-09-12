

data <- read.csv (file.choose())

library(dplyr)
## simple summary

sum (is.na(data$dep_delay))
summary(data$dep_delay)

summarize(data, delay = mean(dep_delay, na.rm = TRUE))# remove missing values if any

##summarize() is not terribly useful unless we pair it with group_by().
#This changes the unit of analysis from the complete dataset to individual
#groups.

summary(data$dep_delay)
summary(data$arr_delay)

?group_by
by_day <- group_by(data, year, month, day)
group_data(by_day)
?group_data
summary(data)

mysum <- summarize(by_day, count = n(), delay = mean(dep_delay, na.rm = TRUE), ardelay = mean(arr_delay, na.rm = TRUE))

##For example, if we applied exactly the same code to a data frame grouped by date,
we get the average delay per date

class ()

by_dest <- group_by(data, dest)# grouping based destinations

delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

data%>% group_by(dest)%>% summarize(dest, COUNT = n(), dist = mean(distance, na.rm = TRUE),
                                             delay = mean(arr_delay, na.rm = TRUE)) 
  

## this is called as piping function

data %>% group_by(carrier)%>%summarize(carrier,
                                            count = n(),
                                            dist = mean(distance, na.rm = TRUE),
                                            delay = mean(arr_delay, na.rm = TRUE))

View (delay)

delay1 <- filter(delay, dist > 1000)
delay1

mydata <-  data %>%
   filter(carrier >= 120) %>%
   select (dep_delay, arr_delay, carrier)%>%
   group_by(carrier)%>%
   summarise(delay = mean(dep_delay, na.rm = TRUE), arr_delay = mean(arr_delay,na.rm = TRUE))%>%
            arrange(desc(delay))

mydata2 <- data %>%
            filter (carrier == "DL")%>%
            group_by(origin)%>% 
            summarize (COUNT = n(), DELAY = mean(dep_delay, na.rm = TRUE))

sum(mydata2$COUNT)

summary(mydata2)

mydata2<- data %>% filter(carrier=="DL") %>% group_by(origin) %>% 
   summarize(COUNT=n(), DELAY = mean(dep_delay, na.rm=TRUE))

 
   
sum(mydata2$DELAY)
   

   


