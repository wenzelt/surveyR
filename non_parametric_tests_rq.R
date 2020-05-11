list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

singleSourceOfTruthAppended <- read_csv("singleSourceOfTruthAppended.csv")
RQ1 <- select(ssot_filtered_sosci, LA01_01:LA02_03, R101, R501,R507,  R510, R513, E201_01:E201_20, A204_01:A204_06 )
RQ2 <- select(ssot_filtered_sosci, 'Current Country of Residence',R101, R216:R216_06, R501, R507,R510, R513, R534:R534_09, E201_01:E201_20, A204_01:A204_06, A307_01:A307_09, HP02_01:HP02_05 )
RQ3 <- select(ssot_filtered_sosci, A004, A005, A007, R101, R501, R534, R528, R507, R510, R513) #Rq3 Selection

############################### RQ_01 ##############################################################
##testing for rq_01 LA01 ~ E201 device risk
wilcox.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),ssot_filtered_sosci$R101)
wilcox.test(rowMeans(select(ssot_filtered_sosci, LA02_01:LA02_03)),ssot_filtered_sosci$R101)

wilcox.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_20)))
wilcox.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20)))#both are statistically significant (WRONG) 
kruskal.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20)))#both are statistically significant (WRONG)
kruskal.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_20))) ## can we set different levels of legislative concern?

## because numeric data we check how relationship is with correlation
cor.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))) ##statistically significant

cor.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_20))) ##why is this not statistically significant
rquery.cormat(cbind(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))))


## testing for rq1 Legislative stance la01 on perceived responsibility between manufacturer and user 
wilcox.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, A204_01:A204_06)))
kruskal.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, A204_01:A204_06))) ##stat sig
wilcox.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, A204_01:A204_06)))
kruskal.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, A204_01:A204_06)))

cor.test(rowMeans(select(ssot_filtered_sosci,LA01_01:LA01_03)),rowMeans(select(ssot_filtered_sosci, E201_01:E201_02)))
LA_Responsibility <- cbind((select(ssot_filtered_sosci, LA01_01:LA01_03)),select(ssot_filtered_sosci,A204_01:A204_06))
LA_Responsibility <- cbind((select(ssot_filtered_sosci, LA02_01:LA02_03)),select(ssot_filtered_sosci,A204_01:A204_06))

rquery.cormat(LA_Responsibility)
########################## RQ_02 ###################################################
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
table(A004,R507)
chisq.test(A004, R507,simulate.p.value = TRUE) ##In chisq.test(A004, R507) : Chi-squared approximation may be incorrect
chisq.test(A004, R510, simulate.p.value = TRUE) ##In chisq.test(A004, R507) : Chi-squared approximation may be incorrect
chisq.test(A004, R513, simulate.p.value = TRUE) ##In chisq.test(A004, R507) : Chi-squared approximation may be incorrect
