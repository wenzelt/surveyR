##### Load libraries ----

library(Hmisc)
library(summarytools)
library(rcompanion)
library(FSA)
library(dunn.test)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sjPlot)
library(ggpubr)
library(rstatix)
library(readxl)
library(psych)
library(QCA)
library(xtable)
library(data.table)
library(plyr)
library(doBy)
library(janitor)
library(gplots)
library(hash)
library(knitr)
library(pagedown)
library(rmarkdown)
library(formattable)



shd_responses <- read_csv("/Users/sameer/Desktop/PoPETS/code/shd_responses.csv")

# Calculate measures needed for analysis ----

# Create Boolean varialbe to indicate if the participant has children

shd_responses$parent <- ifelse(shd_responses$numberofchildren > 0, 1, 0) # having children


# Calculate regulatory protection

shd_responses$rpthirdparty <-
  rowMeans(
    select(
      shd_responses,
      regulation_thirdpartyaccess,
      regulation_thirdpartysharing,
      regulation_thirdpartyprocessing
    )
  )

shd_responses$rppenalty <-
  rowMeans(
    select(
      shd_responses,
      regulation_accesspenalty,
      regulation_accesspenalty,
      regulation_storepenalty
    )
  )


# Reverse four items in SeBIS Proactive Awareness subscale

#shd_responses <- mutate(shd_responses, comparison = 7 - sebis_awareness_openlink)
#shd_responses <- mutate(shd_responses, comparison = 7 - sebis_awareness_sitelookfeel)
#shd_responses <- mutate(shd_responses, comparison = 7 - sebis_awareness_urlverification)
#shd_responses <- mutate(shd_responses, comparison = 7 - sebis_awareness_someoneelse)


#this needs to be 8 
shd_responses <- mutate(shd_responses, sebis_awareness_openlink = 8 - sebis_awareness_openlink)
shd_responses <- mutate(shd_responses, sebis_awareness_sitelookfeel = 8 - sebis_awareness_sitelookfeel)
shd_responses <- mutate(shd_responses, sebis_awareness_urlverification = 8 - sebis_awareness_urlverification)
shd_responses <- mutate(shd_responses, sebis_awareness_someoneelse = 8 - sebis_awareness_someoneelse)



# Calculate scores for SeBIS and MUIPC, including subscales

shd_responses$sebis <-
  rowMeans(
    select(
      shd_responses,
      sebis_securement_screenlockauto,
      sebis_securement_computerpassword,
      sebis_securement_screenlockmanual,
      sebis_securement_mobilepassword,
      sebis_awareness_openlink,
      sebis_awareness_sitelookfeel,
      sebis_awareness_urlverification,
      sebis_awareness_mouseover,
      sebis_awareness_someoneelse,
      sebis_updating_immediate,
      sebis_updating_ensure,
      sebis_updating_antivirus
    )
  )

shd_responses$sebis_devicesecurement <-
  rowMeans(
    select(
      shd_responses,
      sebis_securement_screenlockauto,
      sebis_securement_computerpassword,
      sebis_securement_screenlockmanual,
      sebis_securement_mobilepassword
    )
  )

shd_responses$sebis_proactiveawareness <-
  rowMeans(
    select(
      shd_responses,
      sebis_awareness_openlink,
      sebis_awareness_sitelookfeel,
      sebis_awareness_urlverification,
      sebis_awareness_mouseover,
      sebis_awareness_someoneelse
    )
  )

shd_responses$sebis_updatingbehavior <-
  rowMeans(
    select(
      shd_responses,
      sebis_updating_immediate,
      sebis_updating_ensure,
      sebis_updating_antivirus
    )
  )


shd_responses$muipc <-
  rowMeans(
    select(
      shd_responses,
      muipc_surveillance_location,
      muipc_surveillance_toomuch,
      muipc_surveillance_activities,
      muipc_intrusion_othersknow,
      muipc_intrusion_readilyavailable,
      muipc_intrusion_privacyinvasion,
      muipc_secondaryuse_unauthorized,
      muipc_secondaryuse_otherpurposes,
      muipc_secondaryuse_shareunauthorized
    )
  )

shd_responses$muipc_perceivedsurveillance <-
  rowMeans(
    select(
      shd_responses,
      muipc_surveillance_location,
      muipc_surveillance_toomuch,
      muipc_surveillance_activities
    )
  )

shd_responses$muipc_perceivedintrusion <-
  rowMeans(
    select(
      shd_responses,
      muipc_intrusion_othersknow,
      muipc_intrusion_readilyavailable,
      muipc_intrusion_privacyinvasion
    )
  )

shd_responses$muipc_secondaryuse <-
  rowMeans(
    select(
      shd_responses,
      muipc_secondaryuse_unauthorized,
      muipc_secondaryuse_otherpurposes,
      muipc_secondaryuse_shareunauthorized
    )
  )



# Convert household size to numeric

shd_responses$householdsize <- as.numeric(shd_responses$householdsize)


# Create dataset subsets for each region  for later analyses

gs <- subset(shd_responses, region == "GS")

us <- subset(shd_responses, region == "US")

uk <- subset(shd_responses, region == "UK")



# Define the functions needed for analyses ====



# Define ANOVA function for regions -----

region_anova = select(
  shd_responses,
  region,
  sebis,
  numberofshds,
  softwareupdate,
  ensureprivacy,
  shdecosystemprotection,
  devicesecurity,
  hardwarefix,
  softwarefix,
  devicerisk_coffeemaker,
  devicerisk_dishwasher,
  devicerisk_doorlock,
  devicerisk_doorbell,
  devicerisk_electricitymeter,
  devicerisk_outlet,
  devicerisk_fridge,
  devicerisk_gardening,
  devicerisk_hvac,
  devicerisk_homemonitor,
  devicerisk_lightbulb,
  devicerisk_oven,
  devicerisk_robot,
  devicerisk_speaker,
  devicerisk_stove,
  devicerisk_tv,
  devicerisk_thermostat,
  devicerisk_toy,
  devicerisk_vacuum,
  devicerisk_washer,
  benefit_savemoney,
  benefit_saveenergy,
  benefit_convenience,
  benefit_leisure,
  benefit_peaceofmind,
  benefit_comfort,
  benefit_safety,
  benefit_care,
  benefit_lifequality,
  benefit_propertyvalue,
)

titles <- hash()


calc_anova <- function(data, column) {
  if (!is.null(data[[column]])) {
    data <- data[data[[column]] >= 0, ]
  } else{
    print("Column does not Exist")
  }
  
  means <- round(tapply(as.numeric(data[[column]]), data$region, mean), digits = 2)
  
  # F statistics = Variation among sample means / Variation within groups
  
  aov_content =  aov(data[[column]] ~ data$region)
  
  if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
      tukeyhsd = TukeyHSD(aov_content)
      print(summary(aov_content))
      print(tukeyhsd)
      print(means)
    }
 
   print(means)
}





# Define ANOVA function for household characteristics ----

calc_anova_household <- function(d) {
  print(sprintf("Running ANOVA on %s and '%s'", names(d[1]), titles[[names(d[2])]]))
  d[, 2][d[, 2] < 0] = NA #remove negative values
  means <- round(tapply(as.numeric(d[[2]]), d[[1]], mean), digits = 2)
  
  aov_content =  aov(d[[2]] ~ as.factor(d[[1]]))
  
  if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
    tukeyhsd = TukeyHSD(aov_content)
    print(summary(aov_content))
    print(tukeyhsd)
    plot(tukeyhsd)
    print(means)
  }
  
}

# Create dataset subsets needed for the analyses ----

# Create subset for ownership and use for the three specific devices

specificshds <-
  select(
    shd_responses,
    id,
    pickeddevice1,
    pickeddevice2,
    pickeddevice3,
    pickeddevice1_owned,
    pickeddevice2_owned,
    pickeddevice3_owned,
    device1_usefrequency,
    device2_usefrequency,
    device3_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage
  )



# Create subsets for each device based on whether the participant owned the device ----

device1 <-
  select(
    subset(specificshds, pickeddevice1_owned == 1),
    id,
    pickeddevice1,
    device1_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage
  )

device2 <-
  select(
    subset(specificshds, pickeddevice2_owned == 1),
    id,
    pickeddevice2,
    device2_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage
  )

device3 <-
  select(
    subset(specificshds, pickeddevice3_owned == 1),
    id,
    pickeddevice3,
    device3_usefrequency,
    region,
    rpthirdparty,
    shddata_remotestorage
  )

colnames(device1) <- c("id", "device", "usefrequency", "region", "rpthirdparty", "shddata_remotestorage")
colnames(device2) <- c("id", "device", "usefrequency", "region", "rpthirdparty", "shddata_remotestorage")
colnames(device3) <- c("id", "device", "usefrequency", "region", "rpthirdparty", "shddata_remotestorage")

owneddevices_combined <- rbind(device1, device2, device3)
owneddevices_combined <- subset(owneddevices_combined, usefrequency != 7) #filtering out the options of 'I do not know'


# Create dataset subset for ANOVA for household characteristics ----

household_anova = select(shd_responses, homeowner, numberofshds, parent)



# Create dataset subset for ANOVA for region ----

region_anova = select(
  shd_responses,
  region,
  sebis,
  numberofshds,
  softwareupdate,
  ensureprivacy,
  shdecosystemprotection,
  devicesecurity,
  hardwarefix,
  softwarefix,
  devicerisk_coffeemaker,
  devicerisk_dishwasher,
  devicerisk_doorlock,
  devicerisk_doorbell,
  devicerisk_electricitymeter,
  devicerisk_outlet,
  devicerisk_fridge,
  devicerisk_gardening,
  devicerisk_hvac,
  devicerisk_homemonitor,
  devicerisk_lightbulb,
  devicerisk_oven,
  devicerisk_robot,
  devicerisk_speaker,
  devicerisk_stove,
  devicerisk_tv,
  devicerisk_thermostat,
  devicerisk_toy,
  devicerisk_vacuum,
  devicerisk_washer,
  benefit_savemoney,
  benefit_saveenergy,
  benefit_convenience,
  benefit_leisure,
  benefit_peaceofmind,
  benefit_comfort,
  benefit_safety,
  benefit_care,
  benefit_lifequality,
  benefit_propertyvalue,
)





# Create dataset subset of use and household characteristics ----

# Create subset for ownership, use, and household characteristics for the three specific devices

specificshds_household <-
  select(
    shd_responses,
    id,
    region,
    pickeddevice1,
    pickeddevice2,
    pickeddevice3,
    pickeddevice1_owned,
    pickeddevice2_owned,
    pickeddevice3_owned,
    device1_usefrequency,
    device2_usefrequency,
    device3_usefrequency,
    parent,
    householdsize,
  )

# Create subsets for each device based on whether the participant owned the device ----

device1_household <-
  select(
    subset(specificshds_household, pickeddevice1_owned == 1),
    id,
    region,
    pickeddevice1,
    device1_usefrequency,
    parent,
    householdsize,
  )

device2_household <-
  select(
    subset(specificshds_household, pickeddevice2_owned == 1),
    id,
    region,
    pickeddevice1,
    device2_usefrequency,
    parent,
    householdsize,
  )

device3_household <-
  select(
    subset(specificshds_household, pickeddevice3_owned == 1),
    id,
    region,
    pickeddevice1,
    device3_usefrequency,
    parent,
    householdsize,
  )

colnames(device1_household) <- c("id", "region", "device", "usefrequency", "parent", "householdsize")
colnames(device2_household) <- c("id", "region", "device", "usefrequency", "parent", "householdsize")
colnames(device3_household) <- c("id", "region", "device", "usefrequency", "parent", "householdsize")


owneddevices_combined_household <- rbind(device1_household, device2_household, device3_household)
owneddevices_combined_household <- subset(owneddevices_combined_household, owneddevices_combined_household$usefrequency != 7) #filtering out the options of 'I do not know'



##### Analyses ----



# Demographics of the participants who reported owning the three most owned devices.

# demographics <- select(shd_responses, gender, age_prolific, id)
# device_owners <- owneddevices_combined %>% distinct(id, .keep_all = TRUE)
# demographics <- merge(x = demographics, y = device_owners, by = "id")
# demographics <- subset(demographics, device == "Smart Speaker" | device == "Smart TV" | device == "Smart Light Bulb")
# 
# table(demographics$gender)/nrow(demographics)*100
# median(demographics$age_prolific)

## correction: 
mostowneddevice_owners <- mostowneddevices %>% distinct(id, .keep_all = TRUE)
demographics <- merge(x = demographics, y = mostowneddevice_owners, by = "id")

table(demographics$gender)/nrow(demographics)*100
median(demographics$age_prolific)


#### F1 H3_Perception - Perceived Intrusion MUIPC ----



# perception - how does perceived legislative protection influence Perceived Intrusion

cor.test(shd_responses$rpthirdparty, shd_responses$muipc_perceivedintrusion)

# strong negative correlation for non device users.

#### F2 H3_Perception - Perceived Intrusion MUIPC ----

cor.test(subset(shd_responses, numberofshds < 1)$rpthirdparty, subset(shd_responses, numberofshds < 1)$muipc_perceivedintrusion)



#### F3 H3_Perception - Perceived Intrusion MUIPC----

cor.test(subset(shd_responses, numberofshds > 0)$rpthirdparty, subset(shd_responses, numberofshds > 0)$muipc_perceivedintrusion)



# F4 manufacturer responsibility for protecting privacy and security ----

cor.test(shd_responses$devicesecurity, shd_responses$muipc_perceivedintrusion)



# F5 - perceived regulatory protection} and the \textsl{number of unique types of devices} ====

cor.test(shd_responses$rpthirdparty, shd_responses$numberofshds) #* overall correlation construct average

cor.test(shd_responses$regulation_thirdpartyaccess, shd_responses$numberofshds) #* unwanted access by third parties.

cor.test(shd_responses$regulation_thirdpartyprocessing, shd_responses$numberofshds) #* unwanted processing and analysis by third parties.




# F6 - #Device Usage x Regulatory Protection ====



mostowneddevices <- subset(owneddevices_combined, device == "Smart TV" | device == "Smart Light Bulb" | device == "Smart Speaker")
devices_grouped = group_by(mostowneddevices, device)

cor.test(subset(mostowneddevices, device == "Smart Speaker")$rpthirdparty,
         as.numeric(subset(mostowneddevices, device == "Smart Speaker")$usefrequency),
         method = "pearson"
)

cor.test(subset(mostowneddevices, device == "Smart TV")$rpthirdparty,
         as.numeric(subset(mostowneddevices, device == "Smart TV")$usefrequency), method = "pearson"
)

cor.test(subset(mostowneddevices, device == "Smart Light Bulb")$rpthirdparty,
  as.numeric(subset(mostowneddevices, device == "Smart Light Bulb")$usefrequency),
  method = "pearson"
)


####



# F 7 - Perception Devices Country ANOVA ----



titles$devicesecurity = "Keeping the Smart Home device secure"
titles$hardwarefix = "Fixing a hardware failure"


calc_anova(region_anova, 'devicesecurity')

#calc_anova(region_anova, 'hardwarefix')



nonusers = subset(region_anova, numberofshds == 0)

calc_anova(nonusers, 'devicesecurity')





# F 8 - Perception Devices Adoption by country and legislative protection ----

cor.test(us$rpthirdparty, us$numberofshds)
cor.test(uk$rpthirdparty, uk$numberofshds)
cor.test(gs$rpthirdparty, gs$numberofshds)




# F 9 - Usage of smart TVs across regions ====

dunnTest(x = subset(owneddevices_combined, device == "Smart TV")$usefrequency,
  g = as.factor(subset(owneddevices_combined, device == "Smart TV")$region),
  method = "bonferroni"
)



# F 10 - Usage of smart TVs across regions ====

calc_anova_household(select(household_anova, homeowner, numberofshds))



# F 11 - Amount of types of smart home devices and household size  ====

cor.test(shd_responses$numberofshds, as.numeric(shd_responses$householdsize)) # greater hh size higher variance of devices



# F12 - Children no children t test

t.test(shd_responses$numberofshds ~ shd_responses$parent, var.equal = TRUE, alternative = "two.sided")



# F13 - children impact usage ----

wilcox.test(as.numeric(owneddevices_combined_household$usefrequency) ~ owneddevices_combined_household$parent)

mean(as.numeric(subset(owneddevices_combined_household, parent == 1)$usefrequency))
mean(as.numeric(subset(owneddevices_combined_household, parent == 0)$usefrequency))



# F14 - smart tv owners with kids higher usage ----



parents_smarttv = subset(owneddevices_combined_household, device == "Smart TV")
wilcox.test(as.numeric(parents_smarttv$usefrequency) ~ parents_smarttv$parent)
t.test(as.numeric(parents_smarttv$usefrequency) ~ parents_smarttv$parent)

parents_smartspeaker = subset(owneddevices_combined_household, device == "Smart Speaker")
wilcox.test(as.numeric(parents_smartspeaker$usefrequency) ~ parents_smartspeaker$parent)
t.test(as.numeric(parents_smartspeaker$usefrequency) ~ parents_smartspeaker$parent)

parents_smartlightbulb = subset(owneddevices_combined_household, device == "Smart Light Bulb")
wilcox.test(as.numeric(parents_smartlightbulb$usefrequency) ~ parents_smartlightbulb$parent)
t.test(as.numeric(parents_smartlightbulb$usefrequency) ~ parents_smartlightbulb$parent)



mean(as.numeric(subset(parents_smarttv, parent == 1)$usefrequency))
mean(as.numeric(subset(parents_smarttv, parent == 0)$usefrequency))



# F15 - preference for voice based interaction ----

mean(subset(select(shd_responses, interactcomfort_speaker, parent), parent == 0)$interactcomfort_speaker)
mean(subset(select(shd_responses, interactcomfort_speaker, parent), parent == 1)$interactcomfort_speaker)
wilcox_test(shd_responses, formula = interactcomfort_speaker ~ parent)

mean(subset(select(shd_responses, interactcomfort_assistant, parent), parent == 0)$interactcomfort_assistant)
mean(subset(select(shd_responses, interactcomfort_assistant, parent), parent == 1)$interactcomfort_assistant)
wilcox_test(shd_responses, formula = interactcomfort_assistant ~ parent)




# F16 - mean users non users LA_MEAN ----

mean(subset(shd_responses, numberofshds == 0)$rpthirdparty)
mean(subset(shd_responses, numberofshds > 0)$rpthirdparty)



# F17 - wilcox test MUIPC ----

shdnonowners <-
  subset(
    select(
      shd_responses,
      rpthirdparty,
      rppenalty,
      sebis,
      numberofshds,
      softwareupdate,
      ensureprivacy,
      shdecosystemprotection,
      devicesecurity,
      hardwarefix,
      softwarefix,
      muipc,
      muipc_perceivedintrusion
    ),
    numberofshds == 0
  )

shdowners <-
  subset(
    select(
      shd_responses,
      rpthirdparty,
      rppenalty,
      sebis,
      numberofshds,
      softwareupdate,
      ensureprivacy,
      shdecosystemprotection,
      devicesecurity,
      hardwarefix,
      softwarefix,
      muipc,
      muipc_perceivedintrusion
    ),
    numberofshds > 0
  )


wilcox.test(shdnonowners$muipc, shdowners$muipc)

mean(shdnonowners$muipc)
mean(shdowners$muipc)


# F17.2 -  -----

smartspeaker_owners = subset(owneddevices_combined, device == "Smart Speaker")
mean(as.numeric(smartspeaker_owners$shddata_remotestorage))


# F18 - No integration selections -----


notownreasons = select(
  shd_responses,
  notown_adultprivacy,
  notown_childrenprivacy,
  notown_guestprivacy,
  notown_petprivacy,
  notown_nobenefit,
  notown_cost,
  notown_nointernet
)


prop.table(table(notownreasons$notown_adultprivacy))
prop.table(table(notownreasons$notown_childrenprivacy))
prop.table(table(notownreasons$notown_guestprivacy))
prop.table(table(notownreasons$notown_cost))


# F19 - No integration selections -----


devicerisk_mostowneddevices = select(
  shd_responses,
  devicerisk_tv,
  devicerisk_lightbulb,
  devicerisk_speaker
)

tv = as.data.frame(shd_responses$devicerisk_tv)
tv$id = 'tv'

speaker = as.data.frame(shd_responses$devicerisk_speaker)
speaker$id = 'speaker'

lightbulb= as.data.frame(shd_responses$devicerisk_lightbulb)
lightbulb$id = 'lightbulb'


colnames(tv)=colnames(speaker)=colnames(lightbulb)

devicerisk_mostowneddevices_combined = rbind(tv,speaker,lightbulb)
colnames(devicerisk_mostowneddevices_combined) = c('risk','device')
aov_content =  aov(devicerisk_mostowneddevices_combined$risk ~ devicerisk_mostowneddevices_combined$device)

tukeyhsd = TukeyHSD(aov_content)
print(summary(aov_content))
print(tukeyhsd)



##### 9.0 -  Tables ----



# TABLE: Summary of participant demographics split by the three regions covered in the study ----



demographicstable <-
  select(shd_responses,
         region,
         gender,
         age_prolific)
us_demographics = subset(demographicstable, region == "US")
uk_demographics = subset(demographicstable, region == "UK")
gs_demographics = subset(demographicstable, region == "GS")


#3 people preferred not to enter their age

agedata <- summaryBy(
  age_prolific ~ region,
  data = demographicstable,
  FUN = c(median, mean, sd, min, max),
  na.rm = TRUE
)

agedata <- formattable(agedata,format="f",digits=4)
agedata

table(shd_responses$region, shd_responses$gender)
table(us_demographics$gender)/nrow(us_demographics)*100
table(uk_demographics$gender)/nrow(uk_demographics)*100
table(gs_demographics$gender)/nrow(gs_demographics)*100











# TABLE: The distribution of participants based on the number of types of Smart Home Devices they reported owning, split by the three regions covered in the study. ====

numberofdevices = select(shd_responses, numberofshds, region)

counts <-
  ddply(
    numberofdevices,
    .(
      numberofdevices$numberofshds,
      numberofdevices$region
    ),
    nrow
  )

table(numberofdevices$region, numberofdevices$numberofshds)
table(numberofdevices$numberofshds)

#table2 <- data.table(numberofdevices) # transpose to data.table
#table2 = table2[, list(Freq = .N), by = list(numberofshds, region)] # use list to name var directly





# T3 -  H3_Perception - How is perception affected by the feeling of legislative protection ? (table 3 ) ----



#devicerisk_coffeemaker-20 correspond to the perceived risk of a certain device

#11 Smart Light bulb; 14 Smart Speaker; 16 Smart TV



# table in bold.

# over all devices. Different devices and use cases generate noise

risk_all_devices <- rowMeans(
  select(
    shd_responses,
    devicerisk_coffeemaker,
    devicerisk_dishwasher,
    devicerisk_doorlock,
    devicerisk_doorbell,
    devicerisk_electricitymeter,
    devicerisk_outlet,
    devicerisk_fridge,
    devicerisk_gardening,
    devicerisk_hvac,
    devicerisk_homemonitor,
    devicerisk_lightbulb,
    devicerisk_oven,
    devicerisk_robot,
    devicerisk_speaker,
    devicerisk_stove,
    devicerisk_tv,
    devicerisk_thermostat,
    devicerisk_toy,
    devicerisk_vacuum,
    devicerisk_washer
  )
)





#correlation of interesting devices (3)

# table non bold - first section

cor.test(shd_responses$rpthirdparty, risk_all_devices, adjust.method = "Bonferroni")

subset(select(cor_test(
  select(
    shd_responses,
    rpthirdparty,
    devicerisk_lightbulb,
    devicerisk_speaker,
    devicerisk_tv,
  )
), var1, var2, cor, p),

var1 == "rpthirdparty" & var2 != "rpthirdparty")



cor.test(shd_responses$regulation_thirdpartyaccess, risk_all_devices, adjust.method = "Bonferroni")

subset(
  select(cor_test(
    select(
      shd_responses,
      regulation_thirdpartyaccess,
      devicerisk_lightbulb,
      devicerisk_speaker,
      devicerisk_tv,
    )
  ), var1, var2, cor, p),
  var1 == "regulation_thirdpartyaccess" & var2 != "regulation_thirdpartyaccess"
)

cor.test(shd_responses$regulation_thirdpartysharing, risk_all_devices, adjust.method = "Bonferroni")


subset(
  select(cor_test(
    select(
      shd_responses,
      regulation_thirdpartysharing,
      devicerisk_lightbulb,
      devicerisk_speaker,
      devicerisk_tv,
    )
  ), var1, var2, cor, p),
  var1 == "regulation_thirdpartysharing" & var2 != "regulation_thirdpartysharing"
)

cor.test(shd_responses$regulation_thirdpartyprocessing, risk_all_devices, adjust.method = "Bonferroni")

subset(
  select(cor_test(
    select(
      shd_responses,
      regulation_thirdpartyprocessing,
      devicerisk_lightbulb,
      devicerisk_speaker,
      devicerisk_tv,
    )
  ), var1, var2, cor, p),
  var1 == "regulation_thirdpartyprocessing" & var2 != "regulation_thirdpartyprocessing"
)



# T4 - perceived benefits of smart home devices across countries ANOVA ====

#A307 Perceived Feature benefit -

titles$benefit_savemoney = "Saving money"
titles$benefit_saveenergy = "Saving energy"
titles$benefit_convenience = "Increasing convenience"
titles$benefit_leisure = "Enhancing leisure activities"
titles$benefit_peaceofmind = "Providing peace of mind"
titles$benefit_comfort = "Providing comfort"
titles$benefit_safety = "Increasing safety"
titles$benefit_care = "Providing care"
titles$benefit_lifequality = "Improving quality of life"
titles$benefit_propertyvalue = "Increasing property value"



calc_anova(region_anova, 'benefit_leisure')
calc_anova(region_anova, 'benefit_peaceofmind')
calc_anova(region_anova, 'benefit_comfort')
calc_anova(region_anova, 'benefit_safety')
calc_anova(region_anova, 'benefit_care')
calc_anova(region_anova, 'benefit_propertyvalue')


# number of devices by region 
calc_anova(region_anova, 'numberofshds')

