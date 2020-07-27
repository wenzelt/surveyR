

########################## RQ_03 ###################################################
##PREP####
attach(singleSourceOfTruthAppended)

##Discussion Data####
#p Discussion
# the below test can only be done before splitting into >1 
cor.test(singleSourceOfTruthAppended$R101, as.numeric(singleSourceOfTruthAppended$A005)) # greater hh size higher variance of devices
cor.test(singleSourceOfTruthAppended$R101, as.numeric(singleSourceOfTruthAppended$A004)) # more kids does not correlate 

#Discussion
wilcox_test(singleSourceOfTruthAppended, formula = E201_11 ~ A005) # smart home smart speaker smart tv risk assessment
wilcox_test(singleSourceOfTruthAppended, formula = E201_14 ~ A005) # household size
wilcox_test(singleSourceOfTruthAppended, formula = E201_16 ~ A005)

wilcox_test(singleSourceOfTruthAppended, R101 ~ A004) #p = 0.888

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

##H2####


#H2 - Household Size ~ Daily Usage of devices - R501, R503, R505
# creating table usage device ownership

d1 <- select(subset(singleSourceOfTruthAppended, R233_01 == 1), participant_id, R232_01, R501, A005)
d2 <- select(subset(singleSourceOfTruthAppended, R233_02 == 1), participant_id, R232_02, R503, A005)
d3 <- select(subset(singleSourceOfTruthAppended, R233_03 == 1), participant_id, R232_03, R505, A005)
colnames(d1) <- c("participant_id", "Device_Owned", "Usage", "A005")
colnames(d2) <- c("participant_id", "Device_Owned", "Usage", "A005")
colnames(d3) <- c("participant_id", "Device_Owned", "Usage", "A005")
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
d <- subset(d,A005 != "More than 6")

cor.test(as.numeric(d$A005), as.numeric(d$Usage))

#Device Usage x A005
dtInteresting <- filter(dt, Device_Owned == "Smart TV" | Device_Owned == "Smart Lightbulb" | Device_Owned == "Smart Speaker")
dt = group_by(dtInteresting, Device_Owned)
dplyr::summarize(dt, cor(as.numeric(d$A005), as.numeric(Usage)))
ddply(dt, "Device_Owned", summarise, corr=cor(as.numeric(d$A005), as.numeric(Usage), method = "spearman"))

cor.test(dtInteresting$as.numeric(d$A005), as.numeric(dtInteresting$Usage),method = "pearson")
cor.test(subset(dtInteresting,Device_Owned == "Smart TV")$as.numeric(d$A005), as.numeric(subset(dtInteresting,Device_Owned == "Smart TV")$Usage),method = "pearson")
cor.test(subset(dtInteresting,Device_Owned == "Smart Speaker")$as.numeric(d$A005), as.numeric(subset(dtInteresting,Device_Owned == "Smart Speaker")$Usage),method = "pearson")
cor.test(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$as.numeric(d$A005), as.numeric(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$Usage),method = "pearson")


#H2 - Household size ~ Device Interaction - R534, R536, R538

#H2 - Household size ~ Device Location - R528, R530, R532 //low priority, no interesting findings expected

deviceLocation <-
  select(
    singleSourceOfTruthAppended,
    R528_01:R528_12,
    R529_01:R529_12,
    R530_01:R530_12,
    participant_id,
    A004
  )
#deviceLocationSmartSpeaker <- merge(deviceLocation,dSmartSpeaker, by="participant_id")

#H2 - Household size ~ Disabled features - R507, 510, R513 //no little disabled features (n=37)-> low priority


#H3####
##testing wilcox test for children > 0 impact on amount of devices##

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

wilcox.test(R101, na.omit(A004))##mann whitney u test #not significant

wilcox_test(singleSourceOfTruthAppended, formula = E201_11 ~ A004) 
wilcox_test(singleSourceOfTruthAppended, formula = E201_14 ~ A004) 
wilcox_test(singleSourceOfTruthAppended, formula = E201_16 ~ A004) 

#p what is this?
wilcox_test(singleSourceOfTruthAppended, formula = A204_01 ~ A004) #manufacturer responsbility with child
wilcox_test(singleSourceOfTruthAppended, formula = A204_02 ~ A004) # A004: Children
wilcox_test(singleSourceOfTruthAppended, formula = A204_03 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_04 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_05 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_06 ~ A004)

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
