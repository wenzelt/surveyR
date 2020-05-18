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



## H1 ####

# 1 R216_01	Education about Device D1: Online reviews
# 2	R216_02	Education about Device D1: Online forums
# 3	R216_03	Education about Device D1: Print media (e.g., Newspapers, Magazines, etc.)
# 4	R216_04	Education about Device D1: Friends and Family
# 5	R216_05	Education about Device D1: Online news sites

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



# 1	HP02_01	Market tools: Low prices
# 2	HP02_02	Market tools: Bundled offers (e.g., including other devices with purchase of one or more devices)
# 3	HP02_03	Market tools: Trials (e.g., 30-day free use of a service)
# 4	HP02_04	Market tools: Periodic sales
# 5	HP02_05	Market tools: Discounts (e.g., coupons)

# testing for dependencies in country of residence in purchase was influenced by:

kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_01 ~ `Current Country of Residence`)#n.s
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_02 ~ `Current Country of Residence`)#s p = 0.0127
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_03 ~ `Current Country of Residence`)#s P = 0.0103
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_04 ~ `Current Country of Residence`)#s p = 0.0000000901
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_05 ~ `Current Country of Residence`)#n.s


##H2####



##H3####
#The perception towards Smart Home devices differs internationally.

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

#testing correlation between keeping smart home device secure and letting the manufacturer update my device remotely
cor_test(select(singleSourceOfTruthAppended, A204_04, A206_03)) # no correlation cor = 0.14

#testing for correlation between keeping smart home
cor_test(select(singleSourceOfTruthAppended, A204_04, A206_04)) # no correlation cor = 0.091

# connection between country of residence and smart home device preferences

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
             formula = A307_03 ~ `Current Country of Residence`) #0.0508 # not stat sig
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






########################## RQ_03 ###################################################

##H1####
#plotting sex against amount of devices (purely out of interest)
ggboxplot(
  singleSourceOfTruthAppended,
  x = "Sex",
  y = "R101",
  color = "Sex",
  palette = c("#00AFBB", "#E7B800"),
  ylab = "Amount of devices",
  xlab = "Sex"
)


# testing for amount of devices per property ownership / renting a property
ggboxplot(
  singleSourceOfTruthAppended,
  x = "A007",
  y = "R101",
  color = "A007",
  palette = c("#00AFBB", "#E7B800", "#E7F800"),
  ylab = "Amount of devices",
  xlab = "Renting or owning"
)

wilcox.test(R101 ~ A007 == "Rent" |
              A007 == "Own") # no statistical significance found
kruskal_test(singleSourceOfTruthAppended, formula = R101 ~ A007) # 0.0701

# testing for amount of children in household

ggboxplot(
  singleSourceOfTruthAppended,
  x = "A004",
  y = "R101",
  color = "A004",
  palette = c("#00AFBB", "#E7B800"),
  ylab = "Amount of devices",
  xlab = "Children or no children"
)



##H3####
##testing wilcox test for children > 0 impact on amount of devices##
singleSourceOfTruthAppended$A004 <-
  cut(singleSourceOfTruthAppended$A004, breaks = c(0, 1, Inf)) ## adding levels to children
singleSourceOfTruthAppended$A004 <-
  as.factor(singleSourceOfTruthAppended$A004)


#plotting having children and not having children against eachother
ggboxplot(
  singleSourceOfTruthAppended,
  x = "A004",
  y = "R101",
  color = "A004",
  palette = c("#00AFBB", "#E7B800"),
  ylab = "Amount of devices",
  xlab = "Children or no children"
)

wilcox.test(R101, na.omit(A004), alternative = "greater")##mann whitney u test #not significant
kruskal.test(R101 ~ (na.omit(A004))) ## for some reason significant TODO: WHY?
