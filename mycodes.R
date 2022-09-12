
install.packages ("janitor")
library(janitor)

orders <- read.csv ("clipboard", sep = "\t", header = TRUE)
return <- read.csv ("clipboard", sep = "\t", header = TRUE)
renam
orders1 <- clean_names(orders)

?clean_names
names(orders)[2] <- "Order.ID"

mydata <- merge(orders, return, by = "Order.ID",all.x = T)

sum(is.na(mydata$Returned))


sum(mydata$Profit)

mydata$Returned[is.na(mydata$Returned)] <- "Not Returned"

unique(mydata$Returned)

sum (table (mydata$Returned))

sum(mydata$Returned)



