
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
  read_xlsx("singleSourceOfTruthAppended_P.xlsx")
singleSourceOfTruthAppended <-
  subset(singleSourceOfTruthAppended,
         `Current Country of Residence` != "NA")

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
kruskal_test(singleSourceOfTruthAppended, LA01_01 ~ R101) # statistically significant for value p = 0.00082

###UK US
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  LA01_01 ~ R101
)#ns
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  LA01_02 ~ R101
)#s
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  LA01_03 ~ R101
)#s

###DACH US
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  LA01_01 ~ R101
)#s
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  LA01_02 ~ R101
)#s
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  LA01_03 ~ R101
)#ns

###UK DACH
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "DACH"
  ),
  LA01_01 ~ R101
) # s
cor.test(singleSourceOfTruthAppended$LA01_01, singleSourceOfTruthAppended$R101)

cor.test(LA01_01, A204_01) #Protecting my Smart Home ecosystem as a whole #ns
cor.test(LA01_01, A204_02) #Protecting my Smart Home ecosystem as a whole #ns
cor.test(LA01_01, A204_03) #Protecting my Smart Home ecosystem as a whole #ns
cor.test(LA01_01, A204_04) #keeping my device secure #ns
cor.test(LA01_01, A204_05) #Protecting my Smart Home ecosystem as a whole #ns
cor.test(LA01_01, A204_06) #Protecting my Smart Home ecosystem as a whole #ns

kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "DACH"
  ),
  LA01_02 ~ R101
) #s
kruskal_test(
  subset(
    singleSourceOfTruthAppended,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "DACH"
  ),
  LA01_03 ~ R101
) #s


cor.test(singleSourceOfTruthAppended$LA01_01, singleSourceOfTruthAppended$R101)

# creating table usage device ownership
u <-
  select(
    singleSourceOfTruthAppended,
    participant_id,
    R232_01,
    R232_02,
    R232_03,
    R233_01,
    R233_02 ,
    R233_03 ,
    R501,
    R503,
    R505
  )
d1 <- select(subset(u, R233_01 == 1), participant_id, R232_01, R501)
d2 <- select(subset(u, R233_02 == 1), participant_id, R232_02, R503)
d3 <- select(subset(u, R233_03 == 1), participant_id, R232_03, R505)
colnames(d1) <- c("participant_id", "Device_Owned", "Usage")
colnames(d2) <- c("participant_id", "Device_Owned", "Usage")
colnames(d3) <- c("participant_id", "Device_Owned", "Usage")
d <- rbind(d1, d2, d3)
d <- subset(d, Usage != "Don't know")
d$Usage <-
  factor(
    d$Usage,
    levels = c(
      "0 times",
      "1-5 times",
      "6-10 times",
      "11-20 times",
      "21-30 times",
      "30+ times"
    )
  )

###merge on participant_id their legislative opinion

####H2####
#testing for LA on disabled features:

disabled_features <-
  select(singleSourceOfTruthAppended,
         participant_id,
         R507,
         R510,
         R513,
         LA01_01)
disabled_features$choice <-
  ifelse(disabled_features$R507 == "Yes" |
           R510 == "Yes" | R513 == "Yes", 1, 0)

wilcox.test(disabled_features$LA01_01, disabled_features$choice) #p-value < 2.2e-16
aggregate(LA01_01 ~ choice, data = disabled_features, mean)
# choice  LA01_01
# 1      0 3.331658
# 2      1 3.216216

ggboxplot(
  disabled_features,
  x = "choice",
  y = "LA01_01",
  color = "choice",
  palette = c("#00AFBB", "#E7B800"),
  order = c(0, 1),
  ylab = "LA01_01",
  xlab = "choice"
)


######
#LA01_01 unwanted access by third parties.
d <-
  merge(select(singleSourceOfTruthAppended, LA01_01, participant_id),
        d,
        by = "participant_id")
dSmartTV <- subset(d, Device_Owned == "Smart TV")
dSmartSpeaker <- subset(d, Device_Owned == "Smart Speaker")
dSmartLights <- subset(d, Device_Owned == "Smart Lightbulb")
dOther <-
  subset(
    d,
    Device_Owned != "Smart TV" &
      Device_Owned != "Smart Lightbulb" &
      Device_Owned != "Smart Speaker"
  )
######
#LA01_02 unwanted sharing with third parties.
d2 <-
  merge(select(singleSourceOfTruthAppended, LA01_02, participant_id),
        d2,
        by = "participant_id")
d2SmartTV <- subset(d2, Device_Owned == "Smart TV")
d2SmartSpeaker <- subset(d2, Device_Owned == "Smart Speaker")
d2SmartLights <- subset(d2, Device_Owned == "Smart Lightbulb")
d2Other <-
  subset(
    d2,
    Device_Owned != "Smart TV" &
      Device_Owned != "Smart Lightbulb" &
      Device_Owned != "Smart Speaker"
  )

######
#LA01_03 unwanted processing and analysis by third parties.
d3 <-
  merge(select(singleSourceOfTruthAppended, LA01_03, participant_id),
        d3,
        by = "participant_id")
d3SmartTV <- subset(d3, Device_Owned == "Smart TV")
d3SmartSpeaker <- subset(d3, Device_Owned == "Smart Speaker")
d3SmartLights <- subset(d3, Device_Owned == "Smart Lightbulb")
d3Other <-
  subset(
    d3,
    Device_Owned != "Smart TV" &
      Device_Owned != "Smart Lightbulb" &
      Device_Owned != "Smart Speaker"
  )
######

#testing correlation of legislative opinion with usage
cor.test(d$LA01_01, as.numeric(d$Usage)) #p-value = 0.003954 cor =0.1019092 
cor.test(d2$LA01_02, as.numeric(d2$Usage)) #p-value = 0.0009184 cor =0.1171079 
cor.test(d3$LA01_03, as.numeric(d3$Usage)) #p-value = 0.0001166 cor =0.1359802 

cor.test(dSmartTV$LA01_01, as.numeric(dSmartTV$Usage))#NS p-value = 0.07521 cor = 0.1170394 
  cor.test(d2SmartTV$LA01_02, as.numeric(d2SmartTV$Usage))#p-value = 0.001254 cor = 0.2105709
cor.test(d3SmartTV$LA01_03, as.numeric(d3SmartTV$Usage))#p-value = 0.00351 cor = 0.1909176

cor.test(dSmartSpeaker$LA01_01, as.numeric(dSmartSpeaker$Usage))#p-value = 0.00184 cor = 0.2628
cor.test(d2SmartSpeaker$LA01_02, as.numeric(d2SmartSpeaker$Usage))#p-value = 0.00974 cor = 0.2193522 
cor.test(d3SmartSpeaker$LA01_03, as.numeric(d3SmartSpeaker$Usage))#p-value = 0.0009839 cor = 0.2774907

cor.test(dSmartLights$LA01_01, as.numeric(dSmartLights$Usage))#NS p-value = 0.9616 cor = -0.005112956 
cor.test(d2SmartLights$LA01_02, as.numeric(d2SmartLights$Usage))#NS p-value = 0.8468 cor = -0.02052836 
cor.test(d3SmartLights$LA01_03, as.numeric(d3SmartLights$Usage))#NS p-value = 0.6602 cor = -0.04670735

####H3####

#testing correlation between access to privacy and device risk assessment for popular Devices
cor_test(select(
  singleSourceOfTruthAppended,
  LA01_01,
  E201_11,
  E201_14,
  E201_16
))

#   var1    var2       cor statistic        p conf.low conf.high method
# 2 LA01_01 E201_11 -0.092     -1.93 5.46e- 2   -0.185   0.00181 Pearson
# 3 LA01_01 E201_14 -0.17      -3.59 3.64e- 4   -0.260  -0.0774  Pearson
# 4 LA01_01 E201_16 -0.17      -3.58 3.79e- 4   -0.260  -0.0769  Pearson

v <- rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))
cor.test(singleSourceOfTruthAppended$LA01_01, v) # p-value = 0.0005891 cor = -0.1641262

#Correlation between LA01 and Manufacturer responsibility
cor.test(LA01_01, A204_01) #ns
cor.test(LA01_01, A204_02) #ns
cor.test(LA01_01, A204_03) #Protecting my Smart Home ecosystem as a whole #ns
cor.test(LA01_01, A204_04) #keeping my device secure #ns
cor.test(LA01_01, A204_05) #ns
cor.test(LA01_01, A204_06) #ns