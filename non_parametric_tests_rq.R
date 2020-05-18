list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)

singleSourceOfTruthAppended <-
  read_csv("singleSourceOfTruthAppended.csv")
RQ1 <-
  select(
    ssot_filtered_sosci,
    LA01_01:LA02_03,
    R101,
    R501,
    R507,
    R510,
    R513,
    E201_01:E201_20,
    A204_01:A204_06
  )
RQ2 <-
  select(
    ssot_filtered_sosci,
    'Current Country of Residence',
    R101,
    R216:R216_06,
    R501,
    R507,
    R510,
    R513,
    R534:R534_09,
    E201_01:E201_20,
    A204_01:A204_06,
    A307_01:A307_09,
    HP02_01:HP02_05
  )
RQ3 <-
  select(ssot_filtered_sosci,
         A004,
         A005,
         A007,
         R101,
         R501,
         R534,
         R528,
         R507,
         R510,
         R513) #Rq3 Selection

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

####H1####
#Rq1 - H1 LA01_01 - R101
attach(singleSourceOfTruthAppended)
kruskal.test(LA01_01, R101) #statistically significant for p-value 0.0004 -> Legislative opinion is connected with amount of devices

#testing for average device risk assessment and legislative satisfaction regarding access by third parties
kruskal.test(LA01_01, rowMeans(select(
  singleSourceOfTruthAppended, E201_01:E201_20
))) #statistically insignificant p-value = 0.64
kruskal.test(LA01_01, E201_16) # testing for Smart TV and legislative satisfaction # p = 0.007634
kruskal.test(LA01_01, E201_14) # testing for Smart Speaker and legislative satisfaction # P = 0.001681
kruskal.test(LA01_01, E201_11) # testing for Smart Lights and legislative satisfaction # P = 0.08259




########################## RQ_02 ###################################################
# RQ2: How does the cultural context impact Smart Home device adoption and use?
# H1: The purchasing trends of buying a Smart Home device differs internationally.
# H2: The cultural background of participants affects the usage of Smart Home devices.
# H3: The perception towards Smart Home devices differs internationally.



## H1 ##

attach(singleSourceOfTruthAppended)
singleSourceOfTruthAppended$`Current Country of Residence` <-
  as.factor(singleSourceOfTruthAppended$`Current Country of Residence`)

## testing dependencies between current country of residence and consulting online reviews for their smart devices
kruskal.test(`Current Country of Residence`, R216_01) #not significant
kruskal.test(smartTvAnswers$`Current Country of Residence`,
             smartTvAnswers$R216_01) #not significant
kruskal.test(smartSpeakerAnswers$`Current Country of Residence`,
             smartSpeakerAnswers$R216_01) # not significant
kruskal.test(smartLightAnswers$`Current Country of Residence`,
             smartLightAnswers$R216_01) #not significant

#testing dependencies between current country of residence and amount of sources consulted before purchasing a smart home device

levels(smartSpeakerAnswers$`Current Country of Residence`)
kruskal.test(`Current Country of Residence`, rowMeans(select(
  singleSourceOfTruthAppended, R216, R218, R220
))) # not statistically sign.


## H1 ####


# 1	HP02_01	Market tools: Low prices
# 2	HP02_02	Market tools: Bundled offers (e.g., including other devices with purchase of one or more devices)
# 3	HP02_03	Market tools: Trials (e.g., 30-day free use of a service)
# 4	HP02_04	Market tools: Periodic sales
# 5	HP02_05	Market tools: Discounts (e.g., coupons)

# testing for dependencies in country of residence in purchase was influenced by:
kruskal.test(`Current Country of Residence`, HP02_01) # low prices
kruskal.test(`Current Country of Residence`, HP02_02) # bundled offers
kruskal.test(`Current Country of Residence`, HP02_03) # free trials
kruskal.test(`Current Country of Residence`, HP02_04) # periodic sales # stat sig. p = 3.719 * 10^-6
kruskal.test(`Current Country of Residence`, HP02_05) # general discounts and coupon codes


##H2####



##H3####
# 1 A204_01	Manufacturer responsibilitiy: Keeping the Smart Home device software up-to-date
# 2	A204_02	Manufacturer responsibilitiy: Ensuring my privacy
# 3	A204_03	Manufacturer responsibilitiy: Protecting my Smart Home ecosystem as a whole
# 4	A204_04	Manufacturer responsibilitiy: Keeping the Smart Home device secure
# 5	A204_05	Manufacturer responsibilitiy: Fixing a hardware failure
# 6	A204_06	Manufacturer responsibilitiy: Fixing a software failure



kruskal_test(singleSourceOfTruthAppended,
             formula = A204_01 ~ `Current Country of Residence`)
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_02 ~ `Current Country of Residence`)
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_03 ~ `Current Country of Residence`) #0.0436
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_04 ~ `Current Country of Residence`) #0.00227
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_05 ~ `Current Country of Residence`)
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_06 ~ `Current Country of Residence`)



# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value


kruskal_test(singleSourceOfTruthAppended,
             formula = A307_01 ~ `Current Country of Residence`) # 0.0272
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_02 ~ `Current Country of Residence`)
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_03 ~ `Current Country of Residence`) #0.0508
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_04 ~ `Current Country of Residence`) #0.00268
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_05 ~ `Current Country of Residence`) #0.0125
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_06 ~ `Current Country of Residence`) #0.000141
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_07 ~ `Current Country of Residence`) #0.00867
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_08 ~ `Current Country of Residence`) #0.000615
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_09 ~ `Current Country of Residence`)
kruskal_test(singleSourceOfTruthAppended,
             formula = A307_10 ~ `Current Country of Residence`) #0.0000274




###test for changes in legislature per country
RQ2$`Current Country of Residence` <-
  as.factor(RQ2$`Current Country of Residence`)
kruskal.test(rowMeans(select(ssot_filtered_sosci, LA01_01:LA01_03)) ~ RQ2$`Current Country of Residence`)
##test for prosections per country
kruskal.test(rowMeans(select(ssot_filtered_sosci, LA02_01:LA02_03)) ~ RQ2$`Current Country of Residence`)

#test for devices owned by country
kruskal.test(RQ2$R101 ~ RQ2$`Current Country of Residence`) #not statistically significant

#test for perception
kruskal.test(rowMeans(select(RQ2, A204_02:A204_02)) ~ RQ2$`Current Country of Residence`)

#test for benefitial features
kruskal.test(rowMeans(select(RQ2, A307_01:A307_09)) ~ RQ2$`Current Country of Residence`) ##stat. sig.






########################## RQ_03 ###################################################

library("ggpubr")
ggboxplot(singleSourceOfTruthAppended, x = "A004", y = "R101", 
          color = "A004", palette = c("#00AFBB", "#E7B800"),
          ylab = "Amount of devices", xlab = "Children or no children")


ggboxplot(singleSourceOfTruthAppended, x = "Sex", y = "R101", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Amount of devices", xlab = "Sex")


##testing wilcox test for children > 0 impact on amount of devices##
singleSourceOfTruthAppended$A004 <-
  cut(singleSourceOfTruthAppended$A004, breaks = c(0, 1, Inf)) ## adding levels to children
wilcox.test(R101 ~ A004) ##mann whitney u test
kruskal.test(R101 ~ A004) ## kruskal wallis test one way anova

RQ3$A007 <- as.factor(RQ3$A007)
wilcox.test(R101 ~ A007 == "Rent" | A007 == "Own")

##H1####
# testing for amount of devices per property ownership / renting a property 


