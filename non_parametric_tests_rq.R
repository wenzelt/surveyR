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
library(readxl)


singleSourceOfTruthAppended <-
  read_csv("singleSourceOfTruthAppended.csv")
singleSourceOfTruthAppended <- read_xlsx("singleSourceOfTruthAppended_P.xlsx")

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

#testing for unwanted access to data with amount of different devices owned.
kruskal_test(singleSourceOfTruthAppended, LA01_01 ~ R101) # statistically significant for value p = 0.000395
kruskal_test(subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United States"), LA01_01 ~ R101) 
kruskal_test(subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United Kingdom"), LA01_01 ~ R101) 
kruskal_test(subset(singleSourceOfTruthAppended, `Current Country of Residence` == "DACH"), LA01_01 ~ R101) # statistically significant for value p = 0.00687


cor_test(select(singleSourceOfTruthAppended, LA01_01,E201_11, E201_14,E201_16))
#   var1    var2       cor statistic        p conf.low conf.high method 
# 2 LA01_01 E201_11 -0.085     -1.79 7.34e- 2   -0.177   0.00809 Pearson #  n.s
# 3 LA01_01 E201_14 -0.17      -3.63 3.20e- 4   -0.259  -0.0783  Pearson #  s
# 4 LA01_01 E201_16 -0.17      -3.62 3.25e- 4   -0.259  -0.0781  Pearson #  s


# check correlation for these
cor_test(select(singleSourceOfTruthAppended, E201_11, E201_14,E201_16))
#   var1    var2      cor statistic        p conf.low conf.high method 
# 2 E201_11 E201_14  0.31      6.84 2.61e-11    0.223     0.392 Pearson
# 3 E201_11 E201_16  0.39      8.76 4.08e-17    0.303     0.462 Pearson
# 4 E201_14 E201_11  0.31      6.84 2.61e-11    0.223     0.392 Pearson
# 6 E201_14 E201_16  0.51     12.3  4.51e-30    0.432     0.571 Pearson
# 7 E201_16 E201_11  0.39      8.76 4.08e-17    0.303     0.462 Pearson
# 8 E201_16 E201_14  0.51     12.3  4.51e-30    0.432     0.571 Pearson

#testing for  device risk assessment for our most popular devices and legislative satisfaction regarding access by third parties
kruskal_test(singleSourceOfTruthAppended, E201_14 ~ E201_16) # testing for Smart TV and legislative satisfaction # p = 0.007634
kruskal_test(singleSourceOfTruthAppended, LA01_01 ~ E201_14) # testing for Smart speaker and legislative satisfaction # p = 0.00168
kruskal_test(singleSourceOfTruthAppended, LA01_01 ~ E201_11) # testing for Smart Lights and legislative satisfaction # p = 0.0826

cor_test(select(singleSourceOfTruthAppended, LA01_01, A204_03))
plot(select(singleSourceOfTruthAppended, LA01_01, A204_02))

kruskal_test(singleSourceOfTruthAppended, LA01_01 ~ A204_03) #Protecting my Smart Home ecosystem as a whole
kruskal_test(singleSourceOfTruthAppended, LA01_01 ~ A204_04) #Keeping the Smart Home device secure

# creating table usage device ownership 
u <- select(singleSourceOfTruthAppended,participant_id, R232_01,R232_02,R232_03, R233_01, R233_02 ,R233_03 ,R501, R503, R505)
d1 <- select(subset(u,R233_01 == 1), participant_id, R232_01, R501)
d2 <- select(subset(u,R233_02 == 1), participant_id, R232_02, R503)
d3 <- select(subset(u,R233_03 == 1), participant_id, R232_03, R505)
colnames(d1) <- c("participant_id","Device_Owned", "Usage")
colnames(d2) <- c("participant_id","Device_Owned", "Usage")
colnames(d3) <- c("participant_id","Device_Owned", "Usage")
d <- rbind(d1,d2,d3)
d <- subset(d, Usage != "Don't know")
d$Usage <- factor(d$Usage, levels = c("0 times","1-5 times","6-10 times","11-20 times","21-30 times","30+ times"))

###merge on participant_id their legislative opinion

d <- merge(select(singleSourceOfTruthAppended,LA01_01,participant_id), d, by = "participant_id" )
dSmartTV <- subset(d,Device_Owned == "Smart TV")
dSmartSpeaker <- subset(d,Device_Owned == "Smart Speaker")
dSmartLights <- subset(d,Device_Owned == "Smart Lightbulb")
dOther <- subset(d,Device_Owned != "Smart TV" & Device_Owned != "Smart Lightbulb" & Device_Owned != "Smart Speaker")

#testing correlation of legislative opinion with usage 
cor.test(d$LA01_01,as.numeric(d$Usage))


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
kruskal.test(`Current Country of Residence`, R216_05) # significant
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

p<- c(
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_01 ~ `Current Country of Residence`)[5],
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_02 ~ `Current Country of Residence`)[5],
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_03 ~ `Current Country of Residence`)[5],
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_04 ~ `Current Country of Residence`)[5],#p-adj: 0.01362 #dach - us / us - uk  / dach - uk correct p values for pairwise testing
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_05 ~ `Current Country of Residence`)[5],
kruskal_test(singleSourceOfTruthAppended,
             formula = A204_06 ~ `Current Country of Residence`)[5]
)
p.adjust(p, method = "bonferroni", n = length(p))
#1.00000 1.00000 0.26160 0.01362 0.38100 1.00000

#pairwise testing for A204_04
#starting pairwise testing per country
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United Kingdom"
  ),
  formula = A204_04 ~ `Current Country of Residence`
)#0.00582
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  formula = A204_04 ~ `Current Country of Residence`
)#0.00125
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  formula = A204_04 ~ `Current Country of Residence`
)#0.448 n-s

countryIncreaseProperty = select(singleSourceOfTruthAppended,`Current Country of Residence`, A204_04)
aggregate(countryIncreaseProperty[, 2], list(countryIncreaseProperty$`Current Country of Residence`), mean)
# Group.1  A204_04
# 1           DACH 3.940741
# 2 United Kingdom 4.535484
# 3  United States 4.675862








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


p <- c(
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_01 ~ `Current Country of Residence`
  )[5],
  # 0.0272
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_02 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_03 ~ `Current Country of Residence`
  )[5],
  #0.0508 # not stat sig
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_04 ~ `Current Country of Residence`
  )[5],
  #0.00268
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_05 ~ `Current Country of Residence`
  )[5],
  #0.0125
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_06 ~ `Current Country of Residence`
  )[5],
  #0.000141 -- adding pairwise testing -- p.adjust(p, method = "bonferroni", n = length(p))
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_07 ~ `Current Country of Residence`
  )[5],
  #0.00867
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_08 ~ `Current Country of Residence`
  )[5],
  #0.000615
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_09 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_10 ~ `Current Country of Residence`
  )[5]
) #0.0000274
p <- unlist(p, use.names = FALSE)
p.adjust(p, method = "bonferroni", n = length(p))
#0.272000 1.000000 0.508000 0.026800 0.125000 0.001410 0.086700 0.006150 1.000000 0.000274

#starting pairwise testing per country
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United Kingdom"
  ),
  formula = A307_10 ~ `Current Country of Residence`
)#0.0367
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  formula = A307_10 ~ `Current Country of Residence`
)#0.00000354
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  formula = A307_10 ~ `Current Country of Residence`
)#0.0126

countryIncreaseProperty = select(singleSourceOfTruthAppended,`Current Country of Residence`, A307_10)
aggregate(countryIncreaseProperty[, 2], list(countryIncreaseProperty$`Current Country of Residence`), mean)


# testing for country by perceived device risk

# 1	E201_01	Device risk: Smart Coffee Maker
# 2	E201_02	Device risk: Smart Dishwasher
# 3	E201_03	Device risk: Smart Door Lock
# 4	E201_04	Device risk: Smart Doorbell
# 5	E201_05	Device risk: Smart Electricity Meter
# 6	E201_06	Device risk: Smart Electrical Outlet
# 7	E201_07	Device risk: Smart Fridge
# 8	E201_08	Device risk: Smart Gardening Equipment
# 9	E201_09	Device risk: Smart Heating/Cooling System
# 10	E201_10	Device risk: Smart Home Monitoring System
# 11	E201_11	Device risk: Smart Lightbulb
# 12	E201_12	Device risk: Smart Oven
# 13	E201_13	Device risk: Smart Robot
# 14	E201_14	Device risk: Smart Speaker
# 15	E201_15	Device risk: Smart Stove
# 16	E201_16	Device risk: Smart TV
# 17	E201_17	Device risk: Smart Thermostat
# 18	E201_18	Device risk: Smart Toy
# 19	E201_19	Device risk: Smart Vacuum Cleaner
# 20	E201_20	Device risk: Smart Washing Machine

p <- c((
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = E201_11 ~ `Current Country of Residence`
  )[5]
),
# smart lights
kruskal_test(
  singleSourceOfTruthAppended,
  formula = E201_14 ~ `Current Country of Residence`
)[5],
# smart speaker
kruskal_test(
  singleSourceOfTruthAppended,
  formula = E201_16 ~ `Current Country of Residence`
)[5]
)# smart TV - significantly different for countries p = 0.0000555
# --- plot means by country to find out which is different and higher / lower
p.adjust(p, "bonferroni") #1.0000000 0.2760000 0.0001665

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

wilcox.test(R101 ~ A007 == "Rent" | A007 == "Own") # no statistical significance found
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

# check after removing the outlier
wilcox.test(R101, na.omit(A004), alternative = "greater")##mann whitney u test #not significant
kruskal.test(R101 ~ (na.omit(A004))) ## for some reason significant TODO: WHY?

wilcox_test(singleSourceOfTruthAppended, R101 ~ A004) #p = 0.888

#testing for children affecting the type of usage the user is comfortable with

# 1 E205_01	Usage type: Voice commands via a Smart Speaker
# 2	E205_02	Usage type: Voice commands via a Smartphone Voice Assistant
# 3	E205_03	Usage type: Smartphone App for the Device
# 4	E205_04	Usage type: Smartphone Widgets or Shortcuts
# 5	E205_05	Usage type: Sensors inside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 6	E205_06	Usage type: Sensors outside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 7	E205_07	Usage type: Automatic Operation based on Device Programming

wilcox_test(singleSourceOfTruthAppended, E205_01 ~ A004) #ns
wilcox_test(singleSourceOfTruthAppended, E205_02 ~ A004) #ns
wilcox_test(singleSourceOfTruthAppended, E205_05 ~ A004) #ns
wilcox_test(singleSourceOfTruthAppended, E205_06 ~ A004) #ns
