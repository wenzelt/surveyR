# 3-Way Anova RQ2 - Country comparison
list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
# H1: The purchasing trends of buying a Smart Home device differs internationally.

H1 <- select(singleSourceOfTruthAppended,'Current Country of Residence', R216,R218,R220,HP02_01:HP02_05)

likertScaleCheck <- select(singleSourceOfTruthAppended,participant_id, E201_01:E201_20,A305_01:A305_09,S101_01:S101_12,S102_01:S102_09)


#Changes 'Current Country of Residence' to 'country' #spacessuck
H1 <-H1 %>% rename(country = 'Current Country of Residence')
# if i may propose an alternative: 
#colnames(H1)[1] <- "country"
###########
###rrCountry Comparison on their Pre-Purchase Consultation R216-R220

## D1
H1.16<- na.omit(select(H1,country, R216))

##Step 1, Vectors of R216, which are D1

dachData <- subset(H1.16, H1.16$country == "DACH" )
dachData <- subset( dachData, select = -country)

ukData <- subset(H1.16, H1.16$country == "United Kingdom" )
ukData <- subset( ukData, select = -country)

usData <- subset(H1.16, H1.16$country == "United States" )
usData <- subset( usData, select = -country)
#Data same length?
# 117 is the size of the dach data set
ukDataPicked <- sample_n(ukData, 117)
usDataPicked <- sample_n(usData, 117)

##Step 2, Built data frame out of vectors
combined_groups <- data.frame(cbind(dachData,ukDataPicked,usDataPicked))
##Step 3, Stack groups
stacked_groups <- stack(combined_groups)
##Step 4, Anova
H1.R216.aov <- aov(values~ind,data=stacked_groups)
cricitcalF <- qf(.95, df1=2, df2=348)

## D2
H1.18<- na.omit(select(H1,country, R218))

##Step 1, Vectors of R218, which are D2

dachData18 <- subset(H1.18, H1.18$country == "DACH" )
dachData18 <- subset( dachData18, select = -country)

ukData18 <- subset(H1.18, H1.18$country == "United Kingdom" )
ukData18 <- subset( ukData18, select = -country)

usData18 <- subset(H1.18, H1.18$country == "United States" )
usData18 <- subset( usData18, select = -country)
#Data same length?
# 80 is the size of the us data set
dachData18Picked <- sample_n(dachData18, 80)
ukData18Picked <- sample_n(ukData18, 80)
#usData18Picked <- sample_n(usData19, 117)

##Step 2, Built data frame out of vectors
combined_groups <- data.frame(cbind(dachData18Picked,ukData18Picked,usData18))
##Step 3, Stack groups
stacked_groups <- stack(combined_groups)
##Step 4, Anova
H1.R218.aov <- aov(values~ind,data=stacked_groups)
cricitcalF18 <- qf(.95, df1=2, df2=237)

## D3
H1.20<- na.omit(select(H1,country, R220))

##Step 1, Vectors of R220, which are D3

dachData20 <- subset(H1.20, H1.20$country == "DACH" )
dachData20 <- subset( dachData20, select = -country)

ukData20 <- subset(H1.20, H1.20$country == "United Kingdom" )
ukData20 <- subset( ukData20, select = -country)

usData20 <- subset(H1.20, H1.20$country == "United States" )
usData20 <- subset( usData20, select = -country)
#Data same length?
# 56 is the size of the us data set
dachData20Picked <- sample_n(dachData20, 56)
ukData20Picked <- sample_n(ukData20, 56)
#usData20Picked <- sample_n(usData19, 117)

##Step 2, Built data frame out of vectors
combined_groups <- data.frame(cbind(dachData20Picked,ukData20Picked,usData20))
##Step 3, Stack groups
stacked_groups <- stack(combined_groups)
##Step 4, Anova
H1.R220.aov <- aov(values~ind,data=stacked_groups)
cricitcalF20 <- qf(.95, df1=2, df2=165)

summary(H1.R216.aov)
summary(H1.R218.aov)
summary(H1.R220.aov)


###########

###########
### Country of Residence; Market Tools; HP02

H1.HP<- na.omit(select(H1,country, HP02_01:HP02_05))

##Step 1, Vectors of HP02_01

dachDataHP01 <- subset(H1.HP, H1.HP$country == "DACH" )
dachDataHP01 <- subset( dachDataHP01, select = -country)
dachDataHP01 <- select( dachDataHP01, 'HP02_01')

ukDataHP01 <- subset(H1.HP, H1.HP$country == "United Kingdom" )
ukDataHP01 <- subset( ukDataHP01, select = -country)
ukDataHP01 <- select( ukDataHP01, 'HP02_01')

usDataHP01 <- subset(H1.HP, H1.HP$country == "United States" )
usDataHP01 <- subset( usDataHP01, select = -country)
usDataHP01 <- select( usDataHP01, 'HP02_01')


#Data same length?
dataLength <- c(count(dachDataHP01),count(ukDataHP01),count(usDataHP01))
shortestDataHP01 <- min(unlist(dataLength))
# shortesDataHP01 is the size of the us data set
dachDataHP01Picked <- sample_n(dachDataHP01, shortestDataHP01)
ukDataHP01Picked <- sample_n(ukDataHP01, shortestDataHP01)
usDataHP01Picked <- sample_n(ukDataHP01, shortestDataHP01)

##Step 2, Built data frame out of vectors
combined_groups <- data.frame(cbind(dachDataHP01Picked,ukDataHP01Picked,usDataHP01Picked))
##Step 3, Stack groups
stacked_groups <- stack(combined_groups)
##Step 4, Anova
H1.HP01_01.aov <- aov(values~ind,data=stacked_groups)
summary(H1.HP01_01.aov)
cricitcalHP01_01 <- qf(.95, df1=2, df2=348)
###########
#!#!#!#! READ YaRrr today for only 9,99$ - EXLUSIVE TIPPS!#!#!#  https://bookdown.org/ndphillips/YaRrr/ 
#!#!#!#! EXLUSIVE TIPPS - NASTY HACKS - COOL TRICKS - YaRrr has it all!#!#!# 
#!#!#!#! ORDER NOW +4917647321889!!! ONLY 3 LEFT IN STOCK !!!!!!!!!!!#!#!# 