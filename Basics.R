install.packages ("nycflights13")
install.packages ("DataExplorer")
data_list <- list(airlines, airports, flights, planes, weather)
library(DataExplorer)
library (nycflights13)

View (airlines)

dim(flights)




data_list <- list(airlines, airports, flights, planes, weather)

?airlines
View(airlines)
?airports
airports <- airports
View (airports)
?airports
flights <- flights
?flights
planes  <- planes
?planes
View (flights)
View (airlines)
View (planes)

merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)
?merge


merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, 
                      suffixes = c("_flight", "_manuf"))

names (merge_planes)[3] <- "year_flights"
names (merge_planes)[21] <- "year_manuf"

?airports
?merge
View (airports)
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin",
                               by.y = "faa", all.x = TRUE, suffixes = c("_airlinename", "_airportname"))

final_data <- merge(merge_airports_origin, airports, by.x = "dest", 
                  by.y = "faa", all.x = TRUE, suffixes = c("_originname", "_destname"))

library (DataExplorer)
? DataExplorer
plot_str(data_list)
plot_str(data_list, type = "r") 

introduce(final_data)


data <- data.frame (income = c (200, 300, 400, 450, 500),
                    age = c(22, 23, 24, 27, 30), gender = c("m", "f","m", "f", "m"))
str(data)
introduce (data)
plot_intro(data)

plot_missing (final_data)


final_data1 <- drop_columns(final_data, "speed")
?drop_columns


plot_bar(final_data1)

create_report(final_data1,  output_file = "MY_REPORT.html",
              output_dir = getwd(),config = configure_report())
?create_report


plot_correlation(data, type = "continuous")
plot_correlation(na.omit (final_data1[, c(10,13)]), type = "continuous")


?plot_correlation



data1 <- data.frame (height = c (120, 130, 140, 150, 155), weight = c(50, 60, 70, 80,90))
cor(data1)

plot_correlation(data1)


write.csv (flights, "flights.csv")
write.csv (airplines, "airplanes.csv")

