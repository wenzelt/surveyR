list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

singleSourceOfTruthAppended <- read_csv("singleSourceOfTruthAppended.csv")
RQ2 <- select(singleSourceOfTruthAppended, )
RQ3 <- select(singleSourceOfTruthAppended, A004, A005, A007, R101, R501, R534, R528, R507) #Rq3 Selection
attach(RQ3)
##testing wilcox test for children > 0 impact on amount of devices##
RQ3$A004 <- cut(RQ3$A004, breaks=c(0, 1, Inf)) ## adding levels to children
wilcox.test(R101~A004) ##mann whitney u test
kruskal.test(R101~A004) ## kruskal wallis test one way anova
friedman.test(R101~A004)

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
chisq.test(A004, R507)
           