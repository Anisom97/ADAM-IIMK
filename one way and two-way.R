
data <- read.csv ("clipboard", sep = "\t", header =T)

data$medicines <- as.factor(data$medicines)
str(data)

?aov

anova1 <- aov(effectiveness~medicines, data = data)
summary(anova1)

model.tables(anova1, type = "means")

## HERE our objective is to comapre three conditions (such as medicine A, B and control)
#in terms of effectiveness.
# Here we examined the study using one factor (one group with three levels) anaysis of 
#varience.

#Ho: No difference
#Ha: At least two group (condition)means are different

#The resutsl supported an F value of 106 (df = 2, 12), and reported a p value of 0.000
#The reported p value associated is less than that of alpha (0.05)we are able to reject 
#the null hypothesis. Hence we inferred there exists a difference in means of atleast two
#groups/conditions
# Why post-hoc (in ANOVA we can't conclusviely say that all the three group means are
#different, hence to examine this difference, for example A vs B, A vs. C, B vs. C)we need
#further probing. This probing/test called as post-hoc analysis.

#Inorder to examine which groups are statistically different, we further examined post-hoc
#test name: TukeyHSD


TukeyHSD(anova1, which = "medicines")



#Tukey first test: condition B vs. condition A (54.8 vs. 94.6 = -39.8), difference is 
#negative, implies that condition A effectiveness is higher by 39.8, and it is statistically 
#signifaint(ie., p value is less than that of alpha value of 0.05)

#Second test: it test whetehr thre exists a difference in effectivenss between control condition 
#vs. condition-A(medicine A), the difference is 39.8, and found as statistically significant

# Third test: it test whetehr there exis a difference in effectiveness between control condition
#vs. medicine B, and it shows that the difference is almost zero, and not statistically signfiant. 


data1 <- read.csv ("clipboard", sep = "\t", header = T, stringsAsFactors = T)
str(data1)



model <- aov(clicks~ ads + channels + ads*channels, data = data1)
summary(model)

model.tables (model, type = "means")

coefficients(model)

#interpretation of two-way anova

#Here we have two factor variables. First is ads, seccond is channels. Hence
#we have three effects in anova,  such as:
##1. main effect of ads 
#(which measures whetherthe ads, such as A vs. B create a difference in clicks)
##2. main effect of channel
# which measures whether channel difference create a difference in average of number of
#clicks
## 3. interacction effect of ads*channel
##Which measures whether the difference in ads effect is different across channels,such as
#ndtv vs. cnn.

#INERPRETATIONN_1: MAIN EFFECT OF ADS

#The results reported an ANOVA with a main effect F value of 261.749 for add
#The p value of the F test indicated a very low p value, ie., which is less than
#that of 0.05. Thus, it indicate that reject the null (no difference), and support
#the alternative (there is a difference). Hence, we conclude that ads (A vs. B)
#difference create a difference in average number of clicks. From the mean estimates
#it reports that ad B (mean_clicks = 83.5)is more effective in terms of 
#generating clicks compared to ad A (mean_clicks = 33.37).

##INTERPRETATION_2: MAIN EFFECT OF CHANNEL

#The main effect of channel reported an F value of 0.015 with a p value of .90.
#Here the p value is greater than that of alpha (0.05), we are not able to reject
#null, and hence we support null. It says that channel as such doesn't create
#any difference in average clicks received. It is also evident from the mean estimates
#that, cnn received a average click value of 58.25, which is almost similar to average
#clicks received from ndtv of 58.62

                                                            
# INTERPREATATION_3: INTERACTION EFFECT
#The analysis of interaction effect of ads*channel reported an F vlaue of
#52.156, with a p value of 0.0000. This p value is less than that of alpha (0.05)
#we reject the null, and infer that the effect ads to create difference in number of
#clicks is different across channels. From the mean estimate table, it is clear
#that when users presented with ad A, it reported higher (vs. lower) number of 
#clicks in ndtv (mean_clicks = 44.75) compared to cnn (mean_clicks = 22.00).
#In contradiction with, when ad B is exposed, it was reported higher number of clicks
#in cnn (mean_clicks = 94.50) in comparison with ndtv (mean_clicks = 72.50). 
#See Figure 1 for more details. 


data3 <- read.csv ("clipboard", sep = "\t", header = T)
correlation  <- cor(data3)
correlation
cor.test(data3$Consumption, data3$Income)
cor.test(data3$Consumption, data3$Savings)
cor.test(data3$Savings, data3$Income)

# GENERALLY CORRELATION IS AN ESTIMATE. WE NEED TO TEST THE SAME INORDER TO 
CONFIRM IT IN POPULATION.
#We will use t test to confirm the significance of correlation
#H0: correaltion = 0 (no linear association)
#Ha: correlation is not equal to zero (linear association)

#fROM THE TEST WE FOUND A CORRELATION OF 0.85 BETWEEN INCOME AND CONSUMPTION,
#THE TEST RESUTLS CONFIRME A T VALUE 2.87, AND A P VALUE OF 0.06. AT 5% ALPHA
#THIS CORRELATION IS NOT SIGNFIIANT, HOWEVER, AT 10% ALPHA IT IS STATISTICALLY
#SIGNFIANT, HENCE WE CAN CONCLUDE THAT THE CORRELATION IS MARGINALLY SIGNIFICANT.

readxl
writexl
dplyr
mice
janitor
knitr
Rcmdr
tinytex
rmarkdown
DataExplorer
nycflights13


data4 <- read.csv ("clipboard", sep = "\t", header = T)
table <- table(data4$store, data4$gender)
prop.table (table,2)
chisq.test(data4$store, data4$gender)

# in this case, the chi-square reported is .62, and the associated p value is
#.42, which is greater than that of alpha of 0.05, hence we are not able to reject
#the null. This implies that there is no statistical depdence between store type and
#the gender visted the store.


