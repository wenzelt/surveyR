



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

####H1_Purchase Behaviour####

#how does the purchase behaviour / adoption of smart home devices change in regards to the participant's feeling legistlative protection

#Rq1 - H1 LA01_01 - R101
# R101 equals the amount of owned devices of the user

#testing for unwanted access to data with amount of different devices owned.

#variance of devices correlates with the Legislative satisfaction lightly
cor.test(singleSourceOfTruthAppended$LA01_01,
         singleSourceOfTruthAppended$R101) #*
cor.test(singleSourceOfTruthAppended$LA01_02,
         singleSourceOfTruthAppended$R101) #-
cor.test(singleSourceOfTruthAppended$LA01_03,
         singleSourceOfTruthAppended$R101) #*
cor.test(singleSourceOfTruthAppended$LA_Mean,
         singleSourceOfTruthAppended$R101) #*
# [EXPLANATION] Overall shows that the more the participants feel protected from evil entities by legislation the more different devices they own



# [EXPLANATION] Is there a difference in the amount of devices by region investigated?
kruskal_test(rbind(Participants_UK, Participants_US), LA_Mean ~ R101) #*
kruskal_test(rbind(Participants_DACH, Participants_US), LA_Mean ~ R101) #*
kruskal_test(rbind(Participants_DACH, Participants_UK), LA_Mean ~ R101) #*



#[EXPLANATION] Check if there is a significant difference in amount of devices over regions
kruskal_test(singleSourceOfTruthAppended,
             R101 ~ `Current Country of Residence`)
# there is no reason to believe there are different amount of unique devices over different regions


#There is significant difference in the means of the different countries regarding the legislative protection and the amount of devices unique owned

####H1_Pairwise comparison, only used for discussion####

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
cor.test(singleSourceOfTruthAppended$LA01_01,
         singleSourceOfTruthAppended$R101)

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
###


#####H2 Usage of smart home devices influenced by Legislation (LA)



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

#filtering out devices owned for our analysis on the recurring devices
d1 <-
  select(
    subset(singleSourceOfTruthAppended, R233_01 == 1),
    participant_id,
    R232_01,
    R501,
    LA_Mean
  )
d2 <-
  select(
    subset(singleSourceOfTruthAppended, R233_02 == 1),
    participant_id,
    R232_02,
    R503,
    LA_Mean
  )
d3 <-
  select(
    subset(singleSourceOfTruthAppended, R233_03 == 1),
    participant_id,
    R232_03,
    R505,
    LA_Mean
  )
colnames(d1) <-
  c("participant_id", "Device_Owned", "Usage", "LA_Mean")
colnames(d2) <-
  c("participant_id", "Device_Owned", "Usage", "LA_Mean")
colnames(d3) <-
  c("participant_id", "Device_Owned", "Usage", "LA_Mean")
d <- rbind(d1, d2, d3)
d <-
  subset(d, Usage != "Don't know") #filtering out the options of I do not know due to them not holding additional data
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
cor.test(d$LA_Mean, as.numeric(d$Usage))

# [Explanation] we find that the usage of devices correlates positively when the participants felt protected by legislation


####H2_Disabled Features#### -- too little people to use in analysis



disabled_features <-
  select(
    singleSourceOfTruthAppended,
    participant_id,
    R507,
    R510,
    R513,
    LA01_01,
    LA01_02,
    LA01_03,
  )
disabled_features$choice <-
  ifelse(
    disabled_features$R507 == "Yes" |
      disabled_features$R510 == "Yes" |
      disabled_features$R513 == "Yes",
    1,
    0
  )
table(disabled_features$choice)

wilcox.test(disabled_features$LA01_Mean, disabled_features$choice) #p-value < 2.2e-16
wilcox.test(disabled_features$LA01_01, disabled_features$choice) #p-value < 2.2e-16
wilcox.test(disabled_features$LA01_02, disabled_features$choice) #p-value < 2.2e-16
wilcox.test(disabled_features$LA01_03, disabled_features$choice) #p-value < 2.2e-16

aggregate(LA01_01 ~ choice, data = disabled_features, mean)
aggregate(LA01_02 ~ choice, data = disabled_features, mean)
aggregate(LA01_03 ~ choice, data = disabled_features, mean)


#plotting results
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
ggboxplot(
  disabled_features,
  x = "choice",
  y = "LA01_02",
  color = "choice",
  palette = c("#00AFBB", "#E7B800"),
  order = c(0, 1),
  ylab = "LA01_01",
  xlab = "choice"
)
ggboxplot(
  disabled_features,
  x = "choice",
  y = "LA01_03",
  color = "choice",
  palette = c("#00AFBB", "#E7B800"),
  order = c(0, 1),
  ylab = "Agreement to unwanted processing",
  xlab = "Disabled features"
)


####H2 Device specific usage in popular smart home devices - how does usage relate to legislative protection?

#Device Usage x LA_Mean
dtInteresting <-
  subset(
    d,
    Device_Owned == "Smart TV" |
      Device_Owned == "Smart Lightbulb" |
      Device_Owned == "Smart Speaker"
  )
dt = group_by(dtInteresting, Device_Owned)
LA_MEAN_USAGE_DEVICE_INTERESTING <-
  dplyr::summarize(dt, cor(LA_Mean, as.numeric(Usage)))
LA_MEAN_USAGE_DEVICE_INTERESTING[3] <- "Pearson"
LA_MEAN_USAGE_DEVICE_INTERESTING[4] <-
  c(
    cor.test(
      subset(dtInteresting, Device_Owned == "Smart Lightbulb")$LA_Mean,
      as.numeric(subset(
        dtInteresting, Device_Owned == "Smart Lightbulb"
      )$Usage),
      method = "pearson"
    )$p.value,
    cor.test(
      subset(dtInteresting, Device_Owned == "Smart Speaker")$LA_Mean,
      as.numeric(subset(
        dtInteresting, Device_Owned == "Smart Speaker"
      )$Usage),
      method = "pearson"
    )$p.value,
    cor.test(
      subset(dtInteresting, Device_Owned == "Smart TV")$LA_Mean,
      as.numeric(subset(dtInteresting, Device_Owned == "Smart TV")$Usage),
      method = "pearson"
    )$p.value
  )
colnames(LA_MEAN_USAGE_DEVICE_INTERESTING) <-
  c("Device", "Cor", "Method", "P-Value")

#overall correlation over all 3 devices
cor.test(dtInteresting$LA_Mean,
         as.numeric(dtInteresting$Usage),
         method = "pearson")

#All devices in one table
ddply(dt,
      "Device_Owned",
      summarise,
      corr = cor(LA_Mean, as.numeric(Usage), method = "pearson"))

#individual correlations
cor.test(
  subset(dtInteresting, Device_Owned == "Smart TV")$LA_Mean,
  as.numeric(subset(dtInteresting, Device_Owned == "Smart TV")$Usage),
  method = "pearson"
)
cor.test(
  subset(dtInteresting, Device_Owned == "Smart Speaker")$LA_Mean,
  as.numeric(subset(
    dtInteresting, Device_Owned == "Smart Speaker"
  )$Usage),
  method = "pearson"
)
cor.test(
  subset(dtInteresting, Device_Owned == "Smart Lightbulb")$LA_Mean,
  as.numeric(subset(
    dtInteresting, Device_Owned == "Smart Lightbulb"
  )$Usage),
  method = "pearson"
)

####H3_Perception#### How is perception affected by the feeling of legislative protection ?
#E201_01-20 correspond to the perceived risk of a certain device
#11 Smart Lightbuld; 14 Smart Speaker; 16 Smart TV

# over all devices. Different devices and use cases generate noise
v <- rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))
cor.test(singleSourceOfTruthAppended$LA01_01, v)
cor.test(singleSourceOfTruthAppended$LA01_02, v)
cor.test(singleSourceOfTruthAppended$LA01_03, v)
cor.test(singleSourceOfTruthAppended$LA_Mean, v)
#even though it generates noise, we find that with a statistically significant value
# device risk assessment goes down when the participants feel protected by legislation


####H3_Perception_popular devices####

#correlation of interesting devices (3)
LA_E201_Latex_Interesting <- subset(select(cor_test(
  select(
    singleSourceOfTruthAppended,
    LA_Mean,
    E201_11,
    E201_14,
    E201_16
  )
), var1, var2, cor, p),
var1 == "LA_Mean" & var2 != "LA_Mean")
# [explanation] we find that the smart speaker benefits the most from strong legislation due to a significant drop in risk


#correlation of all devices we have asked
LA_E201_Latex <- subset(select(cor_test(
  select(singleSourceOfTruthAppended,
         LA_Mean,
         E201_01:E201_20)
), var1, var2, cor, p),
var1 == "LA_Mean" & var2 != "LA_Mean")
# we find that the smart home monitoring system receives a huge boost and has 49 people owning it

cor_test(select(singleSourceOfTruthAppended, LA_Mean, E201_10))
#shows large negative impact on device risk when high legislative satisfaction

cor_test(select(
  singleSourceOfTruthAppended,
  LA_Mean,
  E201_11,
  E201_14,
  E201_16
))

#testing correlation between perceived risk for popular Devices
cor_test(select(
  singleSourceOfTruthAppended,
  LA01_01,
  E201_11,
  E201_14,
  E201_16
))
#result:
#   var1    var2       cor statistic        p conf.low conf.high method
# 2 LA01_01 E201_11 -0.092     -1.93 5.46e- 2   -0.185   0.00181 Pearson
# 3 LA01_01 E201_14 -0.17      -3.59 3.64e- 4   -0.260  -0.0774  Pearson
# 4 LA01_01 E201_16 -0.17      -3.58 3.79e- 4   -0.260  -0.0769  Pearson
cor_test(select(
  singleSourceOfTruthAppended,
  LA01_02,
  E201_11,
  E201_14,
  E201_16
))
#result:
#   var1    var2       cor statistic        p conf.low conf.high method
#2 LA01_02 E201_11 -0.076     -1.59 1.13e- 1   -0.169    0.0181 Pearson
#3 LA01_02 E201_14 -0.15      -3.14 1.79e- 3   -0.240   -0.0561 Pearson
#4 LA01_02 E201_16 -0.18      -3.85 1.34e- 4   -0.271   -0.0896 Pearson

cor_test(select(
  singleSourceOfTruthAppended,
  LA01_03,
  E201_11,
  E201_14,
  E201_16
))
#result:
#  var1    var2       cor statistic        p conf.low conf.high method
#2 LA01_03 E201_11 -0.074     -1.54 1.25e- 1   -0.166    0.0205 Pearson
#3 LA01_03 E201_14 -0.13      -2.69 7.51e- 3   -0.219   -0.0344 Pearson
#4 LA01_03 E201_16 -0.17      -3.54 4.42e- 4   -0.258   -0.0749 Pearson

####H3_Perceived Responsibility####
#Correlation between LA01 and Manufacturer responsibility
#Likert-scale from 0-me to 7-manufacturer
#Keeping the Smart Home device software up-to-date
#Ensuring my privacy
#Protecting my Smart Home ecosystem as a whole #ns
#keeping my device secure #ns
#Fixing a hardware failure
#Fixing a software failure

cor_test(select(singleSourceOfTruthAppended, LA_Mean, A204_01:A204_06))
LA_A204_Latex <- subset(select(cor_test(
  select(singleSourceOfTruthAppended,
         LA_Mean,
         A204_01:A204_06)
), var1, var2, cor, p),
var1 == "LA_Mean" & var2 != "LA_Mean")
# [EXPLANATION] we cannot find a large correlation between manufacturer responsibility and perceived legilsative protection



# perception - how does perceived legislative protection influence perceived surveillance
cor_test(select(singleSourceOfTruthAppended, LA_Mean, muipc_PerceivedSur_avg))
#strong negative correlation for non device users.
cor_test(select(
  subset(singleSourceOfTruthAppended, R101 < 1),
  LA_Mean,
  muipc_PerceivedSur_avg
))
cor_test(select(
  subset(singleSourceOfTruthAppended, R101 > 0),
  LA_Mean,
  muipc_PerceivedSur_avg
))
# higher legislative protection negatively impacts perceived surveillance greatly
