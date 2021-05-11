attach(singleSourceOfTruthAppended)
####RQ1####

#Checking for diff of first time user and others

oneDevice <- subset(singleSourceOfTruthAppended, as.numeric(R101) == 1)
moreDevices <- subset(singleSourceOfTruthAppended, as.numeric(R101) > 1)
moreDevices$R101 = 2

overallDevices <- rbind(oneDevice, moreDevices)

mean(oneDevice$LA_Mean)
mean(moreDevices$LA_Mean)
wilcox_test(overallDevices, formula = LA_Mean ~ R101)
chisq_test(overallDevices, formula = LA_Mean ~ R101)

#perceived prosecution of evil-doers.
mean(oneDevice$LA02_Mean)
mean(moreDevices$LA02_Mean)
wilcox_test(overallDevices, formula = LA02_Mean ~ R101)
chisq_test(overallDevices, formula = LA02_Mean ~ R101)

#non users more privacy aware?
no_devices <- subset(select(singleSourceOfTruthAppended,LA_Mean, LA02_Mean, sebis_avg, R101, A204_01:A204_06),R101==0) 
devices <- subset(select(singleSourceOfTruthAppended,LA_Mean, LA02_Mean, sebis_avg, R101,  A204_01:A204_06),R101>0) 
devices$R101 = 1

devices_test <- rbind(no_devices,devices)
wilcox.test(devices_test$sebis_avg,devices_test$R101)
mean(no_devices$sebis_avg)
mean(devices$sebis_avg)

mean(rowMeans(select(no_devices,A204_01:A204_06)))
mean(rowMeans(select(devices,A204_01:A204_06)))


#Comparison LA01 /LA02
mean(singleSourceOfTruthAppended$LA_Mean)
mean(singleSourceOfTruthAppended$LA02_Mean)

#Country comparison
mean(Participants_DACH$LA_Mean)
mean(Participants_UK$LA_Mean)
mean(Participants_US$LA_Mean)


location <- select(singleSourceOfTruthAppended, R528_01:R528_12, R529_01:R529_12,R530_01:R530_12, `Current Country of Residence`)

location1 <- select(singleSourceOfTruthAppended,R528_01:R528_12, `Current Country of Residence`)
location2 <- select(singleSourceOfTruthAppended,R529_01:R529_12, `Current Country of Residence`)
location3 <- select(singleSourceOfTruthAppended,R530_01:R530_12, `Current Country of Residence`)

colnames(location1) <- c("Balcony","Basement", "Children's Room", "Dining Room", "Garage", "Guest Bedroom", "Hallway","Kitchen","Living Room" , "Master Bedroom","Patio", "Yard","Current Country of Residence")
colnames(location2) <- c("Balcony","Basement", "Children's Room", "Dining Room", "Garage", "Guest Bedroom", "Hallway","Kitchen","Living Room" , "Master Bedroom","Patio", "Yard","Current Country of Residence")
colnames(location3) <- c("Balcony","Basement", "Children's Room", "Dining Room", "Garage", "Guest Bedroom", "Hallway","Kitchen","Living Room" , "Master Bedroom","Patio", "Yard","Current Country of Residence")

location <- rbind(location1,location2,location3)

location_stacked <- subset(cbind(A=location[,13],stack(location[1:12])),values==TRUE,-2)
summary(location_stacked)
table(location_stacked$ind,location_stacked$`Current Country of Residence`)
prop.table(table(location_stacked$ind,location_stacked$`Current Country of Residence`),2)*100


####RQ3 preserving privacy of certain people in the household E203####


E203 <- select(singleSourceOfTruthAppended,E203_01:E203_07)
colnames(E203) = c("I wish to preserve the privacy of the adults in my household.","I wish to preserve the privacy of my children.","I wish to preserve the privacy of my guests.","I wish to preserve the privacy of my pets.
","I perceive no benefit from using Smart Home devices.","I find Smart Home devices too expensive.","I do not have a domestic Internet connection suitable for the use of Smart Home devices.")

count(E203[1]) #327 T / 108 F 75.2%
count(E203[2]) #215 T / 220 F 49.4%
count(E203[3]) #182 T / 253 F 41.8%
count(E203[4]) 
count(E203[5])
count(E203[6])
count(E203[7])




df <- as.data.frame(E203)
nn <- names(df)
for (i in seq_along(df)) {
  df[i] <- ifelse(df[i] == TRUE, nn[i], df[i])
}
df %>% 
  transmute_all(funs(ifelse(. == TRUE, deparse(substitute(.)), NA)))


test <- cbind(colMeans(select(Participants_DACH, E201_01:E201_20)),colMeans(select(Participants_UK, E201_01:E201_20)),colMeans(select(Participants_US, E201_01:E201_20)))
dunnTest(x=as.numeric(singleSourceOfTruthAppended$E201_16),g=as.factor(singleSourceOfTruthAppended$`Current Country of Residence`))
dunnTest(x=as.numeric(singleSourceOfTruthAppended$LA_Mean),g=as.factor(singleSourceOfTruthAppended$`Current Country of Residence`))
aggregate(singleSourceOfTruthAppended$LA_Mean ~ singleSourceOfTruthAppended$`Current Country of Residence`, data = singleSourceOfTruthAppended, mean)

#MUIPC Sebis experiments
cor.test(singleSourceOfTruthAppended$muipc_PerceivedSur_avg,singleSourceOfTruthAppended$A204_04)
cor.test(singleSourceOfTruthAppended$muipc_PerceivedSur_avg,rowMeans(select(singleSourceOfTruthAppended,A204_01:A204_06)))

plot(singleSourceOfTruthAppended$muipc_PerceivedSur_avg,rowMeans(select(singleSourceOfTruthAppended,A204_01:A204_06)))
abline(lm(singleSourceOfTruthAppended$muipc_PerceivedSur_avg~rowMeans(select(singleSourceOfTruthAppended,A204_01:A204_06))), col="red") # regression line (y~x)
lines(lowess(rowMeans(select(singleSourceOfTruthAppended,A204_01:A204_06))), col="blue") # lowess line (x,y)
#discuss in meeting


#sebis experiments 
cor.test(singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg, rowMeans(select(singleSourceOfTruthAppended, A204_01:A204_06)))
cor.test(singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg, rowMeans(select(singleSourceOfTruthAppended, A204_02)))

cor.test(singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg, rowMeans(select(singleSourceOfTruthAppended, E201_11)))#device risk
cor.test(singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg, rowMeans(select(singleSourceOfTruthAppended, E203)))
chisq_test(singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg,singleSourceOfTruthAppended$E203_04)

#Getting the qualitative from disabled devices: 
library(tidyr)
disabled_features <- select(singleSourceOfTruthAppended,R509_07a,R542_07a,R512_07a)
disabled_features <- data.frame(a=unlist(a, use.names = FALSE))
disabled_features <- disabled_features %>% filter(a!= "NA")
write_csv(disabled_features, path = "./Excels/disabled_features.csv")

# 20 people per region in non-users, make mini section with smarthome independent questions. 
# differences in legislative protection over the different users 
# adjustments to the paper user/non-user part regarding legislation --> discussion 
#---------------------------------------
sebis_by_region <- select(singleSourceOfTruthAppended, `Current Country of Residence`,sebis_avg, muipc_avg, LA_Mean)
ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=sebis_avg)) + 
  geom_boxplot()

ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=sebis_DeviceSecurement_avg)) + 
  geom_boxplot()
ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=sebis_ProactiveAwareness_avg)) + 
  geom_boxplot()
ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=sebis_UpdatingBehaviour_avg)) + 
  geom_boxplot()
#---------------------------------------
#boxplots sebis categories
ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=muipc_avg)) + 
  geom_boxplot()

ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=muipc_PerceivedIntrusion_avg)) + 
  geom_boxplot()
ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=muipc_PerceivedSur_avg)) + 
  geom_boxplot()
ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=muipc_PersonalInfo_avg)) + 
  geom_boxplot()
#---------------------------------------
#violin plot for LA_Mean
p <- ggplot(sebis_by_region, aes(x=`Current Country of Residence`, y=LA_Mean)) + 
  geom_violin(trim=FALSE)
p+ geom_jitter(shape=16, position=position_jitter(0.2))

p



detach(singleSourceOfTruthAppended)
