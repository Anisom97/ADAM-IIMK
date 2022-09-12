

data <- read.csv (file.choose())

data <- data[, c(1, 2, 3, )]
library(dplyr)

flights_sml <- select(data,
                      year:day,
                      arr_delay,
                      dep_delay,
                      distance,
                      air_time)

flights_sml1 <- mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60, CUM = cumsum(speed), STD_dep = scale (dep_delay))

data %>% select(year:day,
               arr_delay,
               dep_delay,
               distance,
               air_time) %>%
              mutate(GAIN = arr_delay - dep_delay)

newflights <- mutate(flights_sml,
                     gain = arr_delay - dep_delay,
                     hours = air_time / 60,
                     gain_per_hour = gain / hours)



View (newflights)


## If you only want to keep the new variables, use transmute():
newflights1 <- transmute(flights_sml,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

newflights2 <- transmute(flights_sml, ARRI_DELAY = scale (arr_delay))

?scale

write.csv(newflgihts2, "mydata.csv")

