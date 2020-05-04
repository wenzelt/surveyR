# 3-Way Anova RQ2 - Country comparison

library(tidyverse)
library(dplyr)
library(dbplyr)
library(sqldf)

# H1: The purchasing trends of buying a Smart Home device differs internationally.
H1 <- select(singleSourceOfTruthAppended,'Current Country of Residence', R216,R218,R220,HP02_01:HP02_05)
dachData <- subset(H1.16, H1.16$`Current Country of Residence` == "DACH" )

H1.16<- na.omit(select(H1,'Current Country of Residence', R216))
dachData <- subset(H1.16, 'Current Country of Residence' == "DACH" )

dachData %>% filter(H1.16,'Current Country of Residence'== 'United Kingdom')

test <- c(2,3,4,5)

##Schritt 1, Vectoren der R216 Werte pro Landesgruppe
dachData <-c(RQ2_R216_dach)
ukData <- c(RQ2_R216_uk)
usData <- c(RQ2_R216_us)
#Data same length

##Schritt 2, Data Frame aus Vectoren bauen
combined_groups <- data.frame(cbind(dachData,ukData,usData))
##Schritt 3, Gruppen Stacken
stacked_groups <- stack(combined_groups)
##Schritt 4, Anova
H1.R216.aov <- aov(R216~country,data=stacked_groups)

###Country of Residence; Pre-Purchase Consultation; R216-18
summary(aov(R216~country,data=H1.16))

####Country of Residence; Market Tools; HP02