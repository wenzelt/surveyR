

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

wilcox_test(singleSourceOfTruthAppended, R101 ~ A004) 


device_interaction <- select(singleSourceOfTruthAppended, R534_01:R534_06,R536_01:R536_06,R538_01:R538_06)
i1 <- select(singleSourceOfTruthAppended, R534_01:R534_06)
i2 <- select(singleSourceOfTruthAppended, R536_01:R536_06)
i3 <- select(singleSourceOfTruthAppended, R538_01:R538_06)

i1 <- unname(select(singleSourceOfTruthAppended, R534_01:R534_06))
i2 <- unname(select(singleSourceOfTruthAppended, R536_01:R536_06))
i3 <- unname(select(singleSourceOfTruthAppended, R538_01:R538_06))

colnames(i1) = c("Voice Assistant", "App on my phone", "Physical buttons on the device", "Screen on the device", "Internet based service connected to the device", "Home Internet router")
colnames(i2) = c("Voice Assistant", "App on my phone", "Physical buttons on the device", "Screen on the device", "Internet based service connected to the device", "Home Internet router")
colnames(i3) = c("Voice Assistant", "App on my phone", "Physical buttons on the device", "Screen on the device", "Internet based service connected to the device", "Home Internet router")


device_interaction <- rbind(i1,i2,i3)
#table(device_interaction,exclude = c("False"),useNA = "no")
#prop.table(table(device_interaction))

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

####H1####

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

wilcox.test(singleSourceOfTruthAppended$R101 ~ singleSourceOfTruthAppended$A007 == "Rent" |
              singleSourceOfTruthAppended$A007 == "Own") # no statistical significance found

kruskal_test(singleSourceOfTruthAppended, formula = R101 ~ A007) 

# Renting and owning distribution in the US DACH UK 

boxplot <- select(singleSourceOfTruthAppended, A007, `Current Country of Residence`)
ggplot(boxplot, aes(y = `Current Country of Residence`)) +
  geom_bar(aes(fill = A007), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + labs(fill = "Living Situation") + labs(y = "Current Region of Residence")


boxplot <- select(singleSourceOfTruthAppended, A007, `Current Country of Residence`)

piechart <- boxplot %>%
  group_by(`Current Country of Residence`, A007) %>%
  dplyr::summarise(count = n()) %>%
  group_by(`Current Country of Residence`) %>%
  mutate(per=count/sum(count)) %>% 
  ungroup()

ggplot(piechart, aes(x= "", y = per, fill=A007)) + 
  geom_col( width=1, position = "fill") +
  facet_wrap(~`Current Country of Residence`)+
  ggtitle("Living Situation in Regions surveyed in percent") +
  coord_polar("y", start=0) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(name = "Living situation", palette="Accent") + 
  theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 0, hjust =0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 0, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 0, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 0, angle = 90, hjust = .5, vjust = .5, face = "plain")
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
dtInteresting <- filter(d, Device_Owned == "Smart TV" | Device_Owned == "Smart Lightbulb" | Device_Owned == "Smart Speaker")
dt = group_by(dtInteresting, Device_Owned)
USAGE_A005_LATEX <- dplyr::summarize(dt, cor(as.numeric(A005), as.numeric(Usage)))
USAGE_A005_LATEX[3] <- "Pearson"
USAGE_A005_LATEX[4] <- c(cor.test(as.numeric(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$A005), as.numeric(subset(dtInteresting,Device_Owned == "Smart Lightbulb")$Usage),method = "pearson")$p.value,
                                         cor.test(as.numeric(subset(dtInteresting,Device_Owned == "Smart Speaker")$A005), as.numeric(subset(dtInteresting,Device_Owned == "Smart Speaker")$Usage),method = "pearson")$p.value,
                                         cor.test(as.numeric(subset(dtInteresting,Device_Owned == "Smart TV")$A005), as.numeric(subset(dtInteresting,Device_Owned == "Smart TV")$Usage),method = "pearson")$p.value)

colnames(USAGE_A005_LATEX) <- c("Device","Cor", "Method", "P-Value")
USAGE_A005_LATEX$`P-Value` <- paste(as.numeric(USAGE_A005_LATEX$`P-Value`),stars.pval(as.numeric(USAGE_A005_LATEX$`P-Value`)))

#H2 - Household size ~ Device Location - R528, R530, R532 
#//low priority, no interesting findings expected


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

#H1 - Household size ~ Disabled features - R507, 510, R513
##/no little disabled features (n=37)-> low priority


#H2####
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

#dunnTest(singleSourceOfTruthAppended$A004, as.factor(select(singleSourceOfTruthAppended,E201_01:E201_20)), method = "bonferroni")

riskChildren_LATEX <- data.frame(
  "Usage_type" = c("Smart Lightbulb",
                   "Smart Speaker",
                   "Smart TV"
  ), 
  "p_value"= c(wilcox_test(singleSourceOfTruthAppended, E201_11 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E201_14 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E201_16 ~ A004)$p
  ), 
  "effect_size" = c(wilcox_effsize(singleSourceOfTruthAppended, formula = E201_11 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E201_14 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E201_16 ~ A004)$effsize
  ))
riskChildren_LATEX$p_value <- paste(as.numeric(riskChildren_LATEX$p_value),stars.pval(as.numeric(riskChildren_LATEX$p_value)))



#Perception of responsibility
#1 Keeping the Smart Home device software up-to-date
#2 Ensuring my privacy
#3 Protecting my Smart Home ecosystem as a whole
#4 Keeping the Smart Home device secure
#5 Fixing a hardware failure
#6 Fixing a software failure

responsibilityChildren_LATEX <- data.frame(
  "Usage_type" = c("Keeping the Smart Home device software up-to-date",
                   "Ensuring my privacy",
                   "Protecting my Smart Home ecosystem as a whole",
                   "Keeping the Smart Home device secure",
                   "Fixing a hardware failure",
                   "Fixing a software failure"
  ), 
  "p_value"= c(wilcox_test(singleSourceOfTruthAppended, A204_01 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, A204_02 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, A204_03 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, A204_04 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, A204_05 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, A204_06 ~ A004)$p
  ), 
  "effect_size" = c(wilcox_effsize(singleSourceOfTruthAppended, formula = A204_01 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = A204_02 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = A204_03 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = A204_04 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = A204_05 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = A204_06 ~ A004)$effsize
  ))
responsibilityChildren_LATEX$p_value <- paste(as.numeric(responsibilityChildren_LATEX$p_value),stars.pval(as.numeric(responsibilityChildren_LATEX$p_value)))

#testing for children affecting the type of usage the user is comfortable with

# 1 E205_01	Usage type: Voice commands via a Smart Speaker
# 2	E205_02	Usage type: Voice commands via a Smartphone Voice Assistant
# 3	E205_03	Usage type: Smartphone App for the Device
# 4	E205_04	Usage type: Smartphone Widgets or Shortcuts
# 5	E205_05	Usage type: Sensors inside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 6	E205_06	Usage type: Sensors outside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 7	E205_07	Usage type: Automatic Operation based on Device Programming

aggregate(singleSourceOfTruthAppended$E205_01,
          list(singleSourceOfTruthAppended$A004),
          mean)


USAGETYPE_Children_LATEX <- data.frame(
  "Usage_type" = c("Voice commands via a Smart Speaker",
                   "Voice commands via a Smartphone Voice Assistant",
                   "Smartphone App for the Device",
                   "Smartphone Widgets or Shortcuts",
                   "Sensors inside the Home",
                   "Sensors outside the Home",
                   "Automatic Operation based on Device Programming"
  ), 
  "p_value"= c(wilcox_test(singleSourceOfTruthAppended, E205_01 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E205_02 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E205_03 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E205_04 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E205_05 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E205_06 ~ A004)$p,
               wilcox_test(singleSourceOfTruthAppended, E205_07 ~ A004)$p
    ), 
  "effect_size" = c(wilcox_effsize(singleSourceOfTruthAppended, formula = E205_01 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E205_02 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E205_03 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E205_04 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E205_05 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E205_06 ~ A004)$effsize,
                    wilcox_effsize(singleSourceOfTruthAppended, formula = E205_07 ~ A004)$effsize
  ))
USAGETYPE_Children_LATEX$p_value <- paste(as.numeric(USAGETYPE_Children_LATEX$p_value),stars.pval(as.numeric(USAGETYPE_Children_LATEX$p_value)))
