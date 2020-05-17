list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)

singleSourceOfTruthAppended <- read_csv("singleSourceOfTruthAppended.csv")
RQ1 <- select(ssot_filtered_sosci, LA01_01:LA02_03, R101, R501,R507,  R510, R513, E201_01:E201_20, A204_01:A204_06 )
RQ2 <- select(ssot_filtered_sosci, 'Current Country of Residence',R101, R216:R216_06, R501, R507,R510, R513, R534:R534_09, E201_01:E201_20, A204_01:A204_06, A307_01:A307_09, HP02_01:HP02_05 )
RQ3 <- select(ssot_filtered_sosci, A004, A005, A007, R101, R501, R534, R528, R507, R510, R513) #Rq3 Selection

############################### RQ_01 ##############################################################
##fresh start to analyses###

# RQ1: What is the effect of the applied regulatory framework on Smart Home device usage and adoption? 

  # H1 Strong legislative protection increases adoption of Smart Home devices.
  # H2 Strong legislative protection increases the usage of Smart Home devices. 
  # H3 The lack of appropriate legislative protection influences the perception of Smart Home device in users positively
      
      # Choosing which part of LA01 to use for analysis with corresponding variables 
      # LA01_01	Legislatory Framework: unwanted access by third parties. *** selected 
      # LA01_02	Legislatory Framework: unwanted sharing with third parties.
      # LA01_03	Legislatory Framework: unwanted processing and analysis by third parties.
      
#Rq1 - H1 LA01_01 - R101
attach(singleSourceOfTruthAppended)
kruskal.test(LA01_01,R101) #statistically significant for p-value 0.0004 -> Legislative opinion is connected with amount of devices

#testing for average device risk assessment and legislative satisfaction regarding access by third parties
kruskal.test(LA01_01,rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))) #statistically insignificant p-value = 0.64
kruskal.test(LA01_01,E201_16) # testing for Smart TV and legislative satisfaction # p = 0.007634
kruskal.test(LA01_01,E201_14) # testing for Smart Speaker and legislative satisfaction # P = 0.001681
kruskal.test(LA01_01,E201_11) # testing for Smart Lights and legislative satisfaction # P = 0.08259



##testing for rq_01 LA01 ~ E201 device risk

wilcox.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_20)))
wilcox.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20)))#both are statistically significant (WRONG) 
kruskal.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20)))#both are statistically significant (WRONG)
kruskal.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_20))) ## can we set different levels of legislative concern?

## because numeric data we check how relationship is with correlation
cor.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))) ##statistically significant
#The lack of appropriate legislative protection influences the perception of Smart Home device in users positively.

cor.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_20))) ##why is this not statistically significant
rquery.cormat(cbind(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))))


## testing for rq1 Legislative stance la01 on perceived responsibility between manufacturer and user 
wilcox.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, A204_01:A204_06)))
kruskal.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, A204_01:A204_06))) ##stat sig
kruskal.test(singleSourceOfTruthAppended$LA01_01,singleSourceOfTruthAppended$A204_03)
wilcox.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, A204_01:A204_06)))
kruskal.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, A204_01:A204_06)))

cor.test(rowMeans(select(ssot_filtered_sosci,LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_02)))
LA_Responsibility <- cbind((select(ssot_filtered_sosci, LA01_01:LA01_03)),select(ssot_filtered_sosci,A204_01:A204_06))
LA_Responsibility <- cbind((select(ssot_filtered_sosci, LA02_01:LA02_03)),select(ssot_filtered_sosci,A204_01:A204_06))

rquery.cormat(LA_Responsibility)
########################## RQ_02 ###################################################
# RQ2: How does the cultural context impact Smart Home device adoption and use? 
# H1: The purchasing trends of buying a Smart Home device differs internationally. 
# H2: The cultural background of participants affects the usage of Smart Home devices. 
# H3: The perception towards Smart Home devices differs internationally.



## H1 ##

attach(singleSourceOfTruthAppended)
singleSourceOfTruthAppended$`Current Country of Residence` <- as.factor(singleSourceOfTruthAppended$`Current Country of Residence`)

## testing dependencies between current country of residence and consulting online reviews for their smart devices 
kruskal.test(`Current Country of Residence`, R216_01) #not significant 
kruskal.test(smartTvAnswers$`Current Country of Residence`, smartTvAnswers$R216_01) #not significant
kruskal.test(smartSpeakerAnswers$`Current Country of Residence`, smartSpeakerAnswers$R216_01) # not significant
kruskal.test(smartLightAnswers$`Current Country of Residence`, smartLightAnswers$R216_01) #not significant 

#testing dependencies between current country of residence and amount of sources consulted before purchasing a smart home device

levels(smartSpeakerAnswers$`Current Country of Residence`)
kruskal.test(`Current Country of Residence`, rowMeans(select(singleSourceOfTruthAppended,R216,R218,R220))) # not statistically sign.


## using alternative H1 instead of device specifics 
# HP01_01	Pre-purchase consultation: Friends and Family
# HP01_02	Pre-purchase consultation: Internet message boards
#	HP01_03	Pre-purchase consultation: Print media (e.g., Newspapers, Magazines, etc.)
#	HP01_04	Pre-purchase consultation: Online reviews

kruskal.test(`Current Country of Residence`, HP01_02) 

## H1 ##

# testing for dependencies in country of residence in purchase was influenced by: 
kruskal.test(`Current Country of Residence`, HP02_01) # low prices
# testing for dependencies btw. country of residence and taking ad
kruskal.test(`Current Country of Residence`, HP02_02) # bundled offers
kruskal.test(`Current Country of Residence`, HP02_03) # free trials
kruskal.test(`Current Country of Residence`, HP02_04) # periodic sales # stat sig. p = 3.719 * 10^-6
kruskal.test(`Current Country of Residence`, HP02_05) # general discounts and coupon codes 







###test for changes in legislature per country 
RQ2$`Current Country of Residence` <- as.factor(RQ2$`Current Country of Residence`)
kruskal.test(rowMeans(select(ssot_filtered_sosci,LA01_01:LA01_03))~RQ2$`Current Country of Residence`)
##test for prosections per country
kruskal.test(rowMeans(select(ssot_filtered_sosci,LA02_01:LA02_03))~RQ2$`Current Country of Residence`)

#test for devices owned by country 
kruskal.test(RQ2$R101 ~ RQ2$`Current Country of Residence`) #not statistically significant

#test for perception
kruskal.test(rowMeans(select(RQ2,A204_02:A204_02))~ RQ2$`Current Country of Residence`)

#test for benefitial features
kruskal.test(rowMeans(select(RQ2,A307_01:A307_09))~ RQ2$`Current Country of Residence`) ##stat. sig.


########################## RQ_03 ###################################################
##testing wilcox test for children > 0 impact on amount of devices##
RQ3$A004 <- cut(RQ3$A004, breaks=c(0, 1, Inf)) ## adding levels to children
wilcox.test(R101~A004) ##mann whitney u test
kruskal.test(R101~A004) ## kruskal wallis test one way anova

RQ3$A007 <- as.factor(RQ3$A007)
wilcox.test(R101~A007 == "Rent"| A007 == "Own")

##using Chi squared test to check for dependence of usage amount and Children or no children
test <- read.csv("https://goo.gl/j6lRXD")
table(test$treatment, test$improvement)
view(test)
chisq.test(test$treatment, test$improvement, correct=FALSE)
chisq.test(R501,A004, correct = FALSE) ##output shows that the two variables are not dependent
table(A004,R501)

## checking for dependence of disabling features and having children
table(singleSourceOfTruthAppended$A004,singleSourceOfTruthAppended$R507)
attach(RQ3)
chisq.test(A004, R507,simulate.p.value = TRUE) ##In chisq.test(A004, R507) : Chi-squared approximation may be incorrect
chisq.test(A004, R510, simulate.p.value = TRUE) ##In chisq.test(A004, R507) : Chi-squared approximation may be incorrect
chisq.test(A004, R513, simulate.p.value = TRUE) ##In chisq.test(A004, R507) : Chi-squared approximation may be incorrect
