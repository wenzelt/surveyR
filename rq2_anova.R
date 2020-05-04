# 3-Way Anova RQ2 - Country comparison

library(tidyverse)
library(dplyr)
library(dbplyr)

# H1: The purchasing trends of buying a Smart Home device differs internationally.
H1 <- select(singleSourceOfTruthAppended,'Current Country of Residence', R216,R218,R220,HP02_01:HP02_05)
#Changes 'Current Country of Residence' to 'country' #spacessuck
H1 <-H1 %>% rename(country = 'Current Country of Residence')

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

##Schritt 2, Data Frame aus Vectoren bauen
combined_groups <- data.frame(cbind(dachData,ukDataPicked,usDataPicked))
##Schritt 3, Gruppen Stacken
stacked_groups <- stack(combined_groups)
##Schritt 4, Anova
H1.R216.aov <- aov(values~ind,data=stacked_groups)
summary(H1.R216.aov)
cricitcalF <- qf(.95, df1=2, df2=237)

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

##Schritt 2, Data Frame aus Vectoren bauen
combined_groups <- data.frame(cbind(dachData18Picked,ukData18Picked,usData18))
##Schritt 3, Gruppen Stacken
stacked_groups <- stack(combined_groups)
##Schritt 4, Anova
H1.R218.aov <- aov(values~ind,data=stacked_groups)
summary(H1.R218.aov)
cricitcalF18 <- qf(.95, df1=2, df2=258)

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

##Schritt 2, Data Frame aus Vectoren bauen
combined_groups <- data.frame(cbind(dachData20Picked,ukData20Picked,usData20))
##Schritt 3, Gruppen Stacken
stacked_groups <- stack(combined_groups)
##Schritt 4, Anova
H1.R220.aov <- aov(values~ind,data=stacked_groups)
summary(H1.R220.aov)
cricitcalF20 <- qf(.95, df1=2, df2=165)

####Country of Residence; Market Tools; HP02