list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

singleSourceOfTruthAppended <- read_csv("singleSourceOfTruthAppended.csv")
RQ1 <- select(ssot_clean, LA01_01:LA02_03, R101, R501,R507,  R510, R513, E201_01:E201_20, A204_01:A204_06 )
RQ2 <- select(ssot_clean, 'Current Country of Residence', R216:R216_06, R501, R507,R510, R513, R534:R534_09, E201_01:E201_20, A204_01:A204_06, A307_01:A307_09, HP02_01:HP02_05 )
RQ3 <- select(ssot_clean, A004, A005, A007, R101, R501, R534, R528, R507, R510, R513) #Rq3 Selection

##testing for rq_01
wilcox.test(rowMeans(select(ssot_clean, LA01_01:LA01_03)),ssot_clean$R101)

wilcox.test(rowMeans(select(ssot_clean, LA02_01:LA02_03)),ssot_clean$R101)

wilcox.test(rowMeans(select(ssot_clean, LA01_01:LA01_03)),rowMeans(select(ssot_clean, E201_01:E201_20)))
wilcox.test(rowMeans(select(singleSourceOfTruthAppended, LA01_01:LA01_03)),rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20)))#both are statistically significant 


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
