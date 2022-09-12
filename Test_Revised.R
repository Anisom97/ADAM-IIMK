
######################################
## IMPORTING THE DATA INTO R
str(data)

data <- read.csv ("clipboard", sep = "\t", header = TRUE, stringsAsFactors = TRUE)
str(data)

## EXAMINING THE NAME
names(data)

#RE-NAMING IT, SO THAT THERE WILL BE SHORT NAMES/TITLES

names(data) <- c("Sl.No", "RS","SO", "PDT", "INT","RG","RS1","PM","SVM", "PP","JB","LCC")

# EXAMINING THE STRUCTURE OF THE DATA
str(data$RS)

data$RS <- as.numeric(data$RS)-1
data$RS <- factor (data$RS)

#COVERTING THE CATEGORICAL VARIABLES AS FACTORS AND GIVING SUITABLE REPRESENTATIONS
#data$RS <- factor(data$RS, levels = c("Lost", "Won"), labels = c("0","1"))


data$SO <- factor(data$SO)

data$PDT <- factor(data$PDT)# simple factor coversion 
levels (data$PDT)

data$PDT <- factor(data$PDT, order = TRUE, levels = c("GTMSys", "Procsys", "LearnSys", "Finsys", "Lifesys","Logissys","ContactSys"))
summary(data$PDT)


data$INT <- factor(data$INT)
levels (data$INT)

summary(data$INT)
data$INT <- factor(data$INT, order = TRUE, levels = c("Capital Markets", "Banks", "Defense", "Consumer goods", "Others","Security","Energy",
                                                      "Insurance", "Airline", "Finance", "Infrastructure","Mobility","Other Govt.",
                                                      "Govt.", "Telecom equipments","Health", "Clinical research","Agriculture"))

data$RG <- factor (data$RG) 
levels (data$RG)
table <- table(data$RG)
boxplot (table(data$RG))

barplot(table (data$RG))

prop.table (table (data$INT))*100

data$RG <- factor (data$RG, order = TRUE, levels = c("UK", "Other Europe","Americas","Africa",
    "India", "Japan", "Singapore","Spain","Canada")) 
data$LCC <- factor (data$LCC)
levels (data$LCC)

data$LCC <- factor(data$LCC, levels = c("E", "V","F", "L"), labels = c(1,2,3,4))

str( data)
data$RS1 <- as.numeric (data$RS1)
data$PP <- as.numeric (data$PP)
data$JB <- as.numeric (data$JB)

library(Rcmdr)
#a <- 2L
#class(a)
#m <- "male"
#class (m)
#a
#b <-22.45
#class (b)
# while importing data whole number will be treated as integers...integer values and decimal 
##values will be trated as numeric
###################################################

str(data)


##################################
#CASE ANALYSIS

#1.	The marketing team of WSES believes that the average sales value of the leads that they receive is at least 8 million dollars.
#The marketing team believes that the standard deviation of sale value is about 2 million dollars.
#Use an appropriate hypothesis test to check whether the average sales value in the population is at least 8 million dollars.
##################################
#Answer
mean(data$SVM)

SE <- 2 / sqrt(1000)
  
  
# calculating Z statistics & p value
z_stat <- (mean(data$SVM) - 8) /
(2 / sqrt(1000))
mean(data$SVM)

z_stat
pnorm(z_stat)# this shows probability to the left of the curve
1- (pnorm(z_stat))
##############################################################
#To examine the claim, the analyst applied Z test (since the population SD is given)
#In this case, sample sales value generated is 8.004, and the SE = 0.06324555.
#The test results supported z value of 0.6988 and corresponding p value of .2423.
# Here the analyst presumed alpha value of 0.05.
#Conclusion: The test results supported that the p value associated with the test is more than
#that of alpha (0.05), we are not able to reject the null hypothesis (that is sales value is less than or equal to 8 Million), 
#Hence, we concluded that, there is no statistical evidence to support the claim (that is sales value is at least 8 Million)
###############################################################
### assumption that, the sample value received is 10 million (H0 MU > 8)
zstat1 <- (10-8)/(2 / sqrt(1000))
zstat1
1-pnorm(zstat1)

1-pnorm(31)

pnorm(31)



z_stat1 <- (5 - 8) / (2 / sqrt(1000))
z_stat1
pnorm(-47.43)

pvalue <- pnorm(.698863)
pvalue
1-pvalue
# Right tailed 
p <- 1- (pnorm(.698))
p <- (pnorm(.698))
p
#####
# z value is 20
pnorm(20)
1-pnorm(20)
1- pnorm(3.11)

1-pnorm(4.11)


## since the p value of the test reported as .24, which is greater than that of alpha (0.05),
## we are not able reject the null hypothesis. Therefore, we made a conclusion that, there is no
#evidence to support their claim that sales is greater than 8 Million. Thus, we concluded that,
# the test is insignificant. 


## do the calculation at 8.2 million as the sample value

zstat1 <- (8.2-8)/(2 / sqrt(1000))
zstat1
1-pnorm(zstat1)

#here in this case, we considered a fictitious sample value (8.2 million), and conducted the test
# the test results using z test indicated a z statistics of 3.1622, and reported a p value of 0.000,
# thus we concluded that, since the p value is less than that of alpha (0.05), we rejected the null
#and find support for alternative hypothesis. This shows that, the test is statistically significant.
# Thus, we can say that, their claim is true (that is sales value is greater than 8 million)




#QUESTION 2
#Jason McCullagh, senior marketing manager at WSES doubted the value of standard deviation provided by the marketing team. Jason argued that there is no way the marketing team could have known the population standard deviation for the sales value, since the population itself is unknown. Do you agree with Jason McCullagh? If yes, perform the test again using an appropriate hypothesis test.
data$SVM1 <- scale(data$SVM)
?t.test

t.test(data$SVM, mu = 8, alternative = "greater")

sd(data$SVM)

SE <- 1.9829/sqrt(999)
SE

# In this case, the company is doubtful about the SD given (from population)
#So, decided to use sample SD as a proxy for population SD.
# Since population SD is not known and use sample SD as a proxy to test the sample
#mean against the population mean, the appropriate test is one-sample t-test.
#The test (t-test) results supported a t value (test value) of 0.70488, and df = 999
#and p value of .24. In this case, the p value associated with the test value is
#greater than that of alpha (0.05), we are not able to reject the null hypothesis
#(that is sales value is less than or equal to 8 million). Hence, we concluded
#that the test is insignificant. Thus, the conclusion is that we are not able to confirm
#claim (alternative hypothesis), that is not able to get sufficient evidence to support
#the claim made by the company.
#t value  is .7048, and the associated p value = .2405 (it means that the probably of
#of getting this t value in this t distribution is .2405 or 24.05%)


sales value is greater than 8 million)


###########################################
#Question 3:	Prudy Perkins, the Chief Marketing Office (CMO) informed the board that they win at least 50% of the sales leads that they receive. 
#Use an appropriate hypothesis testing procedure to check whether the proportion of leads won by WSES is more than 50%.
#H0: Mu <= 50
#Ha: Mu > 50

table <- table (data$SO)
table
prop.table(table)*100



se <- sqrt ((.5*.5/1000))
se
sqrt (p*q/n)
#p = .50
#q(1-p) = .50
#n = 1000

z_stat1 <- (.481-.50) / se
z_stat1
p <- 1-(pnorm(-1.2016))
p
# while estiamting the z value of the proportion test,and the associated p value
#we found that the p value is greater than that of 0.05(ie. .88), thus, we not able reject the null
#which direct us to infer that the claim is not true. That means, we are not able to reject the null hypothesis


# Question 4;	Hendry Jackson, who works in the product line "learnsys", 
#claims that the probability of winning a sales lead for the product "learnsys"
#is more than that of "Finsys". Is there statistically significant evidence in favor
#of Hendry's claim?

data2 <- data.frame (PDT = data$PDT, SO = data$SO, SVM = data$SVM)

library(dplyr)
data3 <- filter (data2, PDT == "LearnSys"|PDT == "Finsys")

data3 <-  subset(data2,PDT == "LearnSys"|PDT == "Finsys")
View (data3)

data4 <- subset (data2, PDT == "LearnSys")
data5 <- subset (data2, PDT == "Finsys")
hist(data4$SVM)
hist(data5$SVM)
summary(data5)
summary(data4)

table <- table (data3$PDT,data3$SO)
table


prop.table (table,margin = 1)

res <- prop.test(x = c(71, 34), n = c(126, 117), alternative = "greater")
res
#HERE IN THIS CASE, WE EXAMINED TWO SAMPLE PROPORTION TEST TO ANLAYSE/TEST THE HYPOTHESIS.
#THE TEST RESULTS REPORTED THAT, THE SAMPLE WINNING PROPORTION IN LEARN SYSTEM IS .56 AND FIN SYSTEM IS .29,
# THE TEST RESUTLS SUPPORTED A TEST VALUE OF 17.316 (CHI-SQUARE DISTRIBUTION), WITH A P VALUE OF .00000. IN THIS CASE SINCE THE P VALUE OF THE 
#TEST IS LOWER THAN THAT OF ALPHA PRESUMED (O.05), WE REJECT THE NULLL HYPOTHESIS, AND SUPPORT THE CLAIM THAT LEARN SYSTEM
#WINNING CHANCE IS HIGHER THAN THAT OF FIN SYSTEM. THAT MEANS THE TEST IS STATISTICALLY SIGNIFIANT.




# number of learnsys reported as won (1) = 71, (n = total  = 126)
#number of finsys reported as won = 34, (n = total  = 117)

# THE TEST OF TWO GROUP PROPORTION REPORTED THAT THE P VALUE ASSOCIATED WITH THE TEST
#IS LESS THAN THAT OF ALPHA (0.05), WE REJECT THE NULL HYPOTHEIS, AND INFERRED THAT THE CLAIM IS TRUE, 
#THAT MEANS LEANSYSTM'S SALES COVERSION IS BETTER THAN THAT OF FINSYSTMS



# Question 5:	Hendry Jackson also claims that the average sales value of 
#"learnsys" projects is higher than that of "Finsys" projects. 
#Check whether John Crocker is correct at 5% significance.

# H0: SVM[learnsys] <= SVM [finsys]
#Ha: SVM[learnsys] > SVM [finsys]

library(dplyr)
data3 <- filter(data, PDT == "LearnSys"|PDT == "Finsys")
str(data3)
?t.test
res1 <- t.test(SVM ~ PDT, data3, alternative = "greater", var.equal = TRUE)
res1
# To test the hypo, we used two sample t-test. The test reasuls reported a p value, which is greater
#than that of 0.05 (alpha), which led us to infer that his claim is not ture. That means, we are not able 
#to reject null hypotheis. Hence, we are not able to find support for the claim, that leansystm is greater than
#than of finsystem



#Question 6: Liz was of the opinion that there is difference in the average profit across
#geographical locations, such as United Kingdom, India and the America.
#Use an appropriate test to verify the same.

data4 <-  subset(data,RG == "India"|RG == "Americas"|RG == "UK")
data4 <- filter (data,RG == "India"|RG == "Americas"|RG == "UK" )

library(dplyr)
data4 <- select(data4, RG, PP)
OR

#data4 <- data.frame(RG = data4$RG, PP = data4$PP)
#data4$RG <- factor (data4$RG)


#null: no differnce in profit across countries
#alternative: there is a difference in profit across countries (atleast two groups)
str(data4$RG)
anova <- aov(PP~RG, data = data4)
model.tables (anova, type = "means")# it will produce  anova table with mean values 
summary(anova)
#SINCE THE P VALUE IS GREATER THAN ALPHA (0.05), WE ARE NOT ABLE TO REJCT NULL, AND CONFIRM THAT, THE ASSUMPTION IS TURE.

################################
#Extending ANOVA to all countries (profit%)
anova1 <- aov (PP~RG, data  = data)
model.tables (anova1, type = "means")
summary(anova1)
#Extending ANOVA to all countries (RS1)
anova2 <- aov (RS1~RG, data  = data)
model.tables (anova2, type = "means")
summary(anova2)
#POST-HOC TEST (TukeyHSD is a post-hoc test)
TukeyHSD(anova2, which = "RG")

table (data$RG)

# Extending ANOVA  (TWO-Way ANOVA for RS1) to all countries (RG) and product (PDT)
anova3 <- aov (RS1~RG + PDT + RG*PDT, data  = data)
model.tables (anova3, type = "means")
summary(anova3)

?aov

TukeyHSD(anova3, which = "RG")
TukeyHSD(anova3, which = "PDT")
########################
#Question 6: Jack Williams, the CEO of the company believed that the sales conversions are different
#for different for different geographical locations. 
#Check the validity of Jack's belief using an appropriate hypothesis test.
test <- chisq.test(data$RG, data$SO)
test

table <- table(data$RG, data$SO)
table
prop.table (table, margin = 1)*100
test <- chisq.test(table)
test

table2 <- table (data$PDT, data$SO)
prop.table (table2, margin = 1)*100
test2 <- chisq.test(table2)
test2

# another way of doing chi-square test (make sure that two columns are factor vector)
chisq.test(data$RG, data$SO)
# Question 7:	Joe Danby, the chief financial officer believes that 
#the sales conversions depend on the sales value. Use an appropriate hypothesis test to check the validity
# of this claim by making the following 3 groups

# Sales value less than 6 million dollars.
# Sales value between 6 and 8 million (both inclusive) dollars.
# More than 8 million dollars.
library(dplyr)

data <- data %>% mutate(RSVM1 = case_when (SVM < 6 ~ "1", SVM >= 6 & SVM <= 8 ~ "2", SVM > 8 ~ "3"))

## alternative way of getting the groups
                
data$RSVM[data$SVM<6]= 1
data$RSVM[data$SVM>=6 & data$SVM<=8]= 2
data$RSVM[data$SVM> 8]= 3
data$SO

table1 <- table(data$RSVM1, data$SO)
table1
prop.table (table1, margin = 1)
table1

test <- chisq.test(table1)
test
H0: no association
Ha: there is an association 
Since the p value of the test stastics is greater than 0.05, we are not able reject the null
and hence we infer that there exists no association between SVM and SO. 
********************************************************

# Paired t-test
  
before <- c(80,85,86,87,88)
after <- c(91,92,93,94,95)

t.test(before, after, paired = TRUE, alernative = two.sided)\
Here in this case, since the p value associated with the test is less than that of alpha (0.05)
we reject the null, and support the alternative hypothesis
###########################################
