

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

#Rq1 - H1 LA01_01 - R101
# R101 equals the amount of owned devices of the user

#testing for unwanted access to data with amount of different devices owned.

#variance of devices correlates with the Legislative satisfaction lightly 
cor.test(singleSourceOfTruthAppended$LA01_01,
         singleSourceOfTruthAppended$R101)
cor.test(singleSourceOfTruthAppended$LA01_02,
         singleSourceOfTruthAppended$R101)
cor.test(singleSourceOfTruthAppended$LA01_03,
         singleSourceOfTruthAppended$R101)
cor.test(singleSourceOfTruthAppended$LA_Mean,singleSourceOfTruthAppended$R101)

####H2_Pairwise comparison, only used for discussion####

kruskal_test(rbind(Participants_UK,Participants_US), LA_Mean ~ R101)
kruskal_test(rbind(Participants_DACH,Participants_US), LA_Mean ~ R101)
kruskal_test(rbind(Participants_DACH,Participants_UK), LA_Mean ~ R101)

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
d1 <- select(subset(singleSourceOfTruthAppended, R233_01 == 1), participant_id, R232_01, R501, LA_Mean)
d2 <- select(subset(singleSourceOfTruthAppended, R233_02 == 1), participant_id, R232_02, R503, LA_Mean)
d3 <- select(subset(singleSourceOfTruthAppended, R233_03 == 1), participant_id, R232_03, R505, LA_Mean)
colnames(d1) <- c("participant_id", "Device_Owned", "Usage", "LA_Mean")
colnames(d2) <- c("participant_id", "Device_Owned", "Usage", "LA_Mean")
colnames(d3) <- c("participant_id", "Device_Owned", "Usage", "LA_Mean")
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
cor.test(d$LA_Mean, as.numeric(d$Usage))
dt <- data.table(d)
dtCor <- dt[, .(mCor = cor(as.numeric(LA_Mean),as.numeric(Usage))), by=Device_Owned]

####H2_Disabled Features####

disabled_features <-
  select(singleSourceOfTruthAppended,
         participant_id,
         R507,
         R510,
         R513,
         LA01_01,LA01_02,LA01_03,)
disabled_features$choice <-ifelse(disabled_features$R507 == "Yes" | disabled_features$R510 == "Yes" | disabled_features$R513 == "Yes", 1, 0)

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


####H2_Device_usage_TV,Speaker,Light####

#Device Usage x LA_Mean 
dtInteresting <- filter(dt, Device_Owned == "Smart TV" | Device_Owned == "Smart Lightbulb" | Device_Owned == "Smart Speaker")
dt = group_by(dtInteresting, Device_Owned)
LA_MEAN_USAGE_DEVICE_INTERESTING <- dplyr::summarize(dt, cor(LA_Mean, as.numeric(Usage)))
LA_MEAN_USAGE_DEVICE_INTERESTING[3] <- "Pearson"
LA_MEAN_USAGE_DEVICE_INTERESTING[4] <- c(cor.test(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$LA_Mean, as.numeric(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$Usage),method = "pearson")$p.value,
                                         cor.test(subset(dtInteresting,Device_Owned == "Smart Speaker")$LA_Mean, as.numeric(subset(dtInteresting,Device_Owned == "Smart Speaker")$Usage),method = "pearson")$p.value,
                                         cor.test(subset(dtInteresting,Device_Owned == "Smart TV")$LA_Mean, as.numeric(subset(dtInteresting,Device_Owned == "Smart TV")$Usage),method = "pearson")$p.value)
colnames(LA_MEAN_USAGE_DEVICE_INTERESTING) <- c("Device","Cor", "Method", "P-Value")

ddply(dt, "Device_Owned", summarise, corr=cor(LA_Mean, as.numeric(Usage), method = "pearson"))

cor.test(dtInteresting$LA_Mean, as.numeric(dtInteresting$Usage),method = "pearson")
cor.test(subset(dtInteresting,Device_Owned == "Smart TV")$LA_Mean, as.numeric(subset(dtInteresting,Device_Owned == "Smart TV")$Usage),method = "pearson")
cor.test(subset(dtInteresting,Device_Owned == "Smart Speaker")$LA_Mean, as.numeric(subset(dtInteresting,Device_Owned == "Smart Speaker")$Usage),method = "pearson")
cor.test(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$LA_Mean, as.numeric(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$Usage),method = "pearson")

# #Creating sub-sets for tests
# #LA01_01 unwanted access by third parties.
# d <-
#   merge(select(singleSourceOfTruthAppended, LA01_01, participant_id),
#         d,
#         by = "participant_id")
# dSmartTV <- subset(d, Device_Owned == "Smart TV")
# dSmartSpeaker <- subset(d, Device_Owned == "Smart Speaker")
# dSmartLights <- subset(d, Device_Owned == "Smart Lightbulb")
# dOther <-
#   subset(
#     d,
#     Device_Owned != "Smart TV" &
#       Device_Owned != "Smart Lightbulb" &
#       Device_Owned != "Smart Speaker"
#   )
# #LA01_02 unwanted sharing with third parties.
# d2 <-
#   merge(select(singleSourceOfTruthAppended, LA01_02, participant_id),
#         d2,
#         by = "participant_id")
# d2SmartTV <- subset(d2, Device_Owned == "Smart TV")
# d2SmartSpeaker <- subset(d2, Device_Owned == "Smart Speaker")
# d2SmartLights <- subset(d2, Device_Owned == "Smart Lightbulb")
# d2Other <-
#   subset(
#     d2,
#     Device_Owned != "Smart TV" &
#       Device_Owned != "Smart Lightbulb" &
#       Device_Owned != "Smart Speaker"
#   )
# #LA01_03 unwanted processing and analysis by third parties.
# d3 <-
#   merge(select(singleSourceOfTruthAppended, LA01_03, participant_id),
#         d3,
#         by = "participant_id")
# d3SmartTV <- subset(d3, Device_Owned == "Smart TV")
# d3SmartSpeaker <- subset(d3, Device_Owned == "Smart Speaker")
# d3SmartLights <- subset(d3, Device_Owned == "Smart Lightbulb")
# d3Other <-
#   subset(
#     d3,
#     Device_Owned != "Smart TV" &
#       Device_Owned != "Smart Lightbulb" &
#       Device_Owned != "Smart Speaker"
#   )
# 
# #testing correlation of legislative opinion with usage
# cor.test(d$LA01_01, as.numeric(d$Usage)) #p-value = 0.003954 cor =0.1019092
# cor.test(d2$LA01_02, as.numeric(d2$Usage)) #p-value = 0.0009184 cor =0.1171079
# cor.test(d3$LA01_03, as.numeric(d3$Usage)) #p-value = 0.0001166 cor =0.1359802
# 
# cor.test(dSmartTV$LA01_01, as.numeric(dSmartTV$Usage))#NS p-value = 0.07521 cor = 0.1170394
# cor.test(d2SmartTV$LA01_02, as.numeric(d2SmartTV$Usage))#p-value = 0.001254 cor = 0.2105709
# cor.test(d3SmartTV$LA01_03, as.numeric(d3SmartTV$Usage))#p-value = 0.00351 cor = 0.1909176
# 
# cor.test(dSmartSpeaker$LA01_01, as.numeric(dSmartSpeaker$Usage))#p-value = 0.00184 cor = 0.2628
# cor.test(d2SmartSpeaker$LA01_02, as.numeric(d2SmartSpeaker$Usage))#p-value = 0.00974 cor = 0.2193522
# cor.test(d3SmartSpeaker$LA01_03, as.numeric(d3SmartSpeaker$Usage))#p-value = 0.0009839 cor = 0.2774907
# 
# cor.test(dSmartLights$LA01_01, as.numeric(dSmartLights$Usage))#NS p-value = 0.9616 cor = -0.005112956
# cor.test(d2SmartLights$LA01_02, as.numeric(d2SmartLights$Usage))#NS p-value = 0.8468 cor = -0.02052836
# cor.test(d3SmartLights$LA01_03, as.numeric(d3SmartLights$Usage))#NS p-value = 0.6602 cor = -0.04670735

####H3_Perception####
#E201_01-20 correspond to the perceived risk of a certain device
#11 Smart Lightbuld; 14 Smart Speaker; 16 Smart TV

v <- rowMeans(select(singleSourceOfTruthAppended, E201_01:E201_20))
cor.test(singleSourceOfTruthAppended$LA01_01, v) # p-value = 0.0005891 cor = -0.1641262
cor.test(singleSourceOfTruthAppended$LA01_02, v)
cor.test(singleSourceOfTruthAppended$LA01_03, v)
cor.test(singleSourceOfTruthAppended$LA_Mean, v)

####H3_Perception_popular devices####

LA_E201_Latex_Interesting <- subset(select(cor_test(select(
  singleSourceOfTruthAppended,
  LA_Mean,
  E201_11,
  E201_14,
  E201_16
)),var1,var2,cor,p,method),var1 == "LA_Mean" & var2 != "LA_Mean")

LA_E201_Latex <- subset(select(cor_test(select(
  singleSourceOfTruthAppended,
  LA_Mean,
  E201_01:E201_20
)),var1,var2,cor,p,method),var1 == "LA_Mean" & var2 != "LA_Mean")

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
LA_A204_Latex <- subset(select(cor_test(select(
  singleSourceOfTruthAppended,
  LA_Mean,
  A204_01:A204_06
)),var1,var2,cor,p,method),var1 == "LA_Mean" & var2 != "LA_Mean")
