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
prop.table(table(location_stacked$ind,location_stacked$`Current Country of Residence`))


#RQ3 preserving privacy of certain people in the household E203

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
rcorr(as.matrix(select(singleSourceOfTruthAppended,muipc_PerceivedSur_avg,muipc_PerceivedIntrusion_avg,muipc_PersonalInformation_avg,A204_04)))
cor.test(singleSourceOfTruthAppended$muipc_PerceivedSur_avg,singleSourceOfTruthAppended$A204_04)


#Getting the qualitative from disabled devices: 
library(tidyr)
disabled_features <- select(singleSourceOfTruthAppended,R509_07a,R542_07a,R512_07a)
disabled_features <- data.frame(a=unlist(a, use.names = FALSE))
disabled_features <- disabled_features %>% filter(a!= "NA")
write_csv(disabled_features, path = "./Excels/disabled_features.csv")
