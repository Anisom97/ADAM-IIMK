
data <- read.csv("flights.csv")

select(data, year, month, day)
# Select all columns between year and day (inclusive)
select(data, year:day)

# Select all columns except those from year to day (inclusive)
select(data, -(year:day))

rename(data, tail_num = tailnum)

##Another option is to use select() in conjunction with the every
thing() helper. This is useful if you have a handful of variables
you'd like to move to the start of the data frame:
  
  select(data, time_hour, air_time, everything())
