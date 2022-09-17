str(data)
#data$`Month-Year` <- as.data.frame(data$`Month-Year`, "%Y-%m-%d")

install.packages("janitor")
library(janitor)

data1 <- clean_names(data)


# q1 : 

# ONE SAMPLE T TEST

# H0- D6 info. was accessed less than 60 times - mu <= 60
# Ha- D6 info was accessed atleast greater than 60 times - mu > 60

data1 <- subset(data1,data1$month_year >= "2017-10-01")

t.test(data1$d6, mu=60, alternative = "greater")

sample_mean <- mean(data1$d6)
sd <- sd(data1$d6)
se <- sd/sqrt(28)  # sample so n-1
t <- (sample_mean - 60) / se
1-pnorm(t)

# calc. p value is 0.013
# P value is less than 0.05, so we reject the null hypothesis
# the claim made is true, and the access went greater than 60 

# Here we found an estimated sample value of 68.41, which was compared against the population value of 60
# using one sample t-test. The test resutls reported that t = 2.341, df = 28, p-value = 0.01329.
# Since, the p value associated the test is less than that of alpha (0.05), we will reject the null (Mu < = 60)
# and support the alternative hypothesis (Mu >60). Hence, we will infer that the claim made by
# Mr. Anand is true, that is, from October 2017 onwads the average weekly access is greater than 60 with
# respec to Leafcurl.


# q2 

# TWO TAILED T TEST

data1 <- clean_names(data)

data1$group <- factor(ifelse(data1$month_year >= "2017-01-01","2017-2018","2015-2016" ))

data1$group <- relevel(data1$group, ref = "2017-2018")  #q3

t_test <- t.test(data1$no_of_users~data1$group, alternative = "greater")
t_test


#inorder to examine the stated hypothesis, the study applied an indpendent sample t-test
#In this test, we compared 2017-2018 average weekly estimates against 2015-2016 average weekly users.
#The test resutls supported t = 9.2567, df = 121, p-value = 4.753e-16. This indicates
#that the associated p value of the test is less than that of alpha (0.05), hence we reject the null
#hypothesis, and infer that average weekly users during 2017-2018 is higher than that of 2015-2016.

#q3 & q4
install.packages("sos")

library(sos)
library(dplyr)

data3 <- clean_names(data)
data4 <- filter(data3, data3$month_year >= "2016-01-01")
data5 <- select(data4, week, usage)
data5$week <- factor(data5$week)
levels(data5$week)
anova <- aov(usage~week, data=data5)
summary(anova)
model.tables(anova, type = "means")


data6 <- filter(data1, month_year < "2016-01-01")
data7 <- select(data6, week, usage)
data7$week <- factor(data7$week)
levels (data7$week)# how many levels are there in this factor vector
anova1 <- aov(usage~week, data = data7)
summary(anova1)
model.tables(anova, type = "means")


#---------------------------------------------------------------------------------------------
# ASSIGNMENT

#q5
# Check for usage trend year-month wise 

library(dplyr)

Data <- clean_names(data)
data_q4<-Data
data_q4_f <- filter(data_q4, data_q4$month_year >= "2016-08-01")
data_q4_monthwise <- group_by(data_q4_f,month_year)
data_q4_summary <- summarise(data_q4_monthwise, usage = mean(usage))
data_q4_summary

plot(data_q4_summary$month_year, data_q4_summary$usage, type='o',
      xlab = "Year Month", ylab = "Mean_Usage",
      main = "Mean Use Month-wise"
      )
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))

#Immediately after launch in August 2016, within 2 months, there was a spike in average usage, 
#but that did not last long. 
#But from September 2017, there was significant raise in the average usage which lasted for a 
#longer time frame.


#q6
# Two sample T-test

#H0: Mean disease access when conditions are favorable <= Mean disease access when conditions 
#are not favorable 
#Ha: Mean disease access when conditions are favorable > Mean disease access when conditions 
#are not favorable 

#---- For DISEASE 1 ----

library(janitor)

data_bela_w <- clean_names(data_bel_w)

data_bela_w$flag<-factor(ifelse((data_bela_w$temperature>=20 
                                  & data_bela_w$temperature<=24 
                                  & data_bela_w$humidity>80),"Disease access","No Disease access"))

data_bela_w$flag<-relevel(data_bela_w$flag,ref = "Disease access")

t.test(data_bela_w$d1~data_bela_w$flag,data=data_bela_w,alternative="greater",var.eq=TRUE)

# P value (0.0057) is less than 0.05(significance) - so we reject the NULL HYPOTHESIS

data_dharw_w <- clean_names(data_dhar_w)

data_dharw_w$flag<-factor(ifelse((data_dharw_w$temperature>=20 
                                 & data_dharw_w$temperature<=24 
                                 & data_dharw_w$relative_humidity>80),"Disease access"
                                ,"No Disease access"))

data_dharw_w$flag<-relevel(data_dharw_w$flag,ref = "Disease access")

t.test(data_dharw_w$d1~data_dharw_w$flag,data=data_dharw_w,alternative="greater",var.eq=TRUE)

# P value (0.000088) is less than 0.05(significance) - so we reject the NULL HYPOTHESIS

#--------

