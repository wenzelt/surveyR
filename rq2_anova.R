# 3-Way Anova RQ2 - Country comparison

library(tidyverse)
library(dplyr)
library(dbplyr)
library(sqldf)

# H1: The purchasing trends of buying a Smart Home device differs internationally.
H1 <- select(singleSourceOfTruthAppended,'Current Country of Residence', R216,R218,R220,HP02_01:HP02_05)
H1 <-H1 %>% rename(country = 'Current Country of Residence')
#Changes 'Current Country of Residence' to 'country' #spacessuck
H1.16<- na.omit(select(H1,country, R216))

##Schritt 1, Vectoren der R216 Werte pro Landesgruppe

dachData <- subset(H1.16, H1.16$country == "DACH" )
dachData <- subset( dachData, select = -country)

ukData <- subset(H1.16, H1.16$country == "United Kingdom" )
ukData <- subset( ukData, select = -country)

usData <- subset(H1.16, H1.16$country == "United States" )
usData <- subset( usData, select = -country)
#Data same length?
# 117 is the size of the data data set
ukDataPicked <- sample_n(ukData, 117)
usDataPicked <- sample_n(usData, 117)

##Schritt 2, Data Frame aus Vectoren bauen
combined_groups <- data.frame(cbind(dachData,ukDataPicked,usDataPicked))
##Schritt 3, Gruppen Stacken
stacked_groups <- stack(combined_groups)
##Schritt 4, Anova
H1.R216.aov <- aov(values~ind,data=stacked_groups)

###Country of Residence; Pre-Purchase Consultation; R216-18
summary(aov(R216~country,data=H1.16))

####Country of Residence; Market Tools; HP02