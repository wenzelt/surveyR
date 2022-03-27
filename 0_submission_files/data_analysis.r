##### Installation of dependencies ----
list.of.packages <- c(
  "Hmisc",
  
  "tidyverse",
  
  "summarytools",
  
  "stringi",
  
  "sjPlot",
  
  "doBy",
  
  "janitor" ,
  
  "data.table",
  
  "plyr",
  
  "psych",
  
  "lattice",
  
  "multcompView",
  
  "dplyr",
  
  "ggpubr",
  
  "QCA",
  
  "FSA",
  
  "dunn.test",
  
  "rcompanion",
  
  "gplots",
  
  "hash",
  
  'knitr',
  
  'pagedown',
  
  'rmarkdown',
  
  "gtools"
  
)



new.packages <-
  
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages))
  
  install.packages(new.packages)



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




ssot_new <- read_csv("0_submission_files/0_submission_dataset.csv")

# 0.3 appending measures to dataset ----

# create bool if participant has children or not from numeric var

ssot_new$parents <-
  
  ifelse(ssot_new$numberofchildren > 0, 1, 0) # having children


# appending avg of our legislative construct to the dataset

ssot_new$LA_Mean <-
  
  rowMeans(
    select(
      ssot_new,
      regulation_thirdpartyaccess,
      regulation_thirdpartysharing,
      regulation_thirdpartyprocessing
    )
  )

ssot_new$LA02_Mean <-
  
  rowMeans(
    select(
      ssot_new,
      regulation_accesspenalty,
      regulation_accesspenalty,
      regulation_storepenalty
    )
  )


# reverse one item in Sebis proactive awareness

ssot_new <- mutate(ssot_new, comparison = 7 - sebis_awareness_openlink)

ssot_new <- mutate(ssot_new, comparison = 7 - sebis_awareness_sitelookfeel)

ssot_new <-
  mutate(ssot_new, comparison = 7 - sebis_awareness_urlverification)

ssot_new <- mutate(ssot_new, comparison = 7 - sebis_awareness_someoneelse)



# appending Sebis/MUIPC Averages to dataset

ssot_new$sebis_avg <-
  
  rowMeans(
    select(
      ssot_new,
      
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

ssot_new$sebis_DeviceSecurement_avg <-
  
  rowMeans(
    select(
      ssot_new,
      sebis_securement_screenlockauto,
      sebis_securement_computerpassword,
      sebis_securement_screenlockmanual,
      sebis_securement_mobilepassword
    )
  )

ssot_new$sebis_ProactiveAwareness_avg <-
  
  rowMeans(
    select(
      ssot_new,
      sebis_awareness_openlink,
      sebis_awareness_sitelookfeel,
      sebis_awareness_urlverification,
      sebis_awareness_mouseover,
      sebis_awareness_someoneelse
    )
  )

ssot_new$sebis_UpdatingBehaviour_avg <-
  
  rowMeans(
    select(
      ssot_new,
      sebis_updating_immediate,
      sebis_updating_ensure,
      sebis_updating_antivirus
    )
  )



ssot_new$muipc_avg <-
  
  rowMeans(
    select(
      ssot_new,
      
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

ssot_new$muipc_PerceivedSurveillance_avg <-
  
  rowMeans(
    select(
      ssot_new,
      muipc_surveillance_location,
      muipc_surveillance_toomuch,
      muipc_surveillance_activities
    )
  )

ssot_new$muipc_PerceivedIntrusion_avg <-
  
  rowMeans(
    select(
      ssot_new,
      muipc_intrusion_othersknow,
      muipc_intrusion_readilyavailable,
      muipc_intrusion_privacyinvasion
    )
  )

ssot_new$muipc_SecondaryUse_avg <-
  
  rowMeans(
    select(
      ssot_new,
      muipc_secondaryuse_unauthorized,
      muipc_secondaryuse_otherpurposes,
      muipc_secondaryuse_shareunauthorized
    )
  )



# converting household size to numeric variable

ssot_new$householdsize <-
  
  as.numeric(ssot_new$householdsize)


# Dataset into different countries for later analyses

Participants_GS <-
  
  subset(ssot_new,
         
         region == "GS")

Participants_US <-
  
  subset(ssot_new,
         
         region == "US")

Participants_UK <-
  
  subset(ssot_new,
         
         region == "UK")



# 0.4 function definitions ====



# 0.4.1 Definition ANOVA function Countries -----

region_anova = select(
  ssot_new,
  
  region,
  
  sebis_avg,
  
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



calc_anova <- function(data, column_to_use) {
  if (!is.null(data[[column_to_use]])) {
    data <- data[data[[column_to_use]] >= 0, ]
    
  } else{
    print("Column does not Exist")
    
  }
  
  
  
  means <-
    
    round(tapply(as.numeric(data[[column_to_use]]),
                 
                 data$region,
                 
                 mean),
          
          digits = 2)
  
  
  
  
  
  # plotmeans(
  #   data[[column_to_use]] ~ data$region,
  #   
  #   digits = 2,
  #   
  #   ccol = 'red',
  #   
  #   mean.labels = F,
  #   
  #   ylab = 'mean',
  #   
  #   main = sprintf("Plot of '%s' means by region", titles[[column_to_use]]),
  #   
  # )
  
  
  
  # boxplot(
  #   data[[column_to_use]] ~ data$region,
  #   
  #   main = sprintf("Plot of '%s' means by region", titles[[column_to_use]]),
  #   
  #   xlab = "'region'",
  #   
  #   ylab = titles[[column_to_use]],
  #   
  #   col = rainbow(7)
  #   
  # )
  
  
  
  
  
  
  
  # F statistics = Variation among sample means / Variation within groups
  
  
  
  aov_content =  aov(data[[column_to_use]] ~ data$region)
  
  if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
    tuk = TukeyHSD(aov_content)
    
    
    
    print(summary(aov_content))
    
    print(tuk)
    
    # plot(tuk)
    
    print(means)
    
  }
  
  print(means)
  
  
  
}





# 0.4.2 Definition ANOVA household ----

calc_anova_house <- function(d) {
  # pdf(sprintf("Plot_LA_MEAN_%s.pdf", titles[[names(d[2])]]))
  
  
  
  print(sprintf("Running ANOVA on %s and '%s'", names(d[1]), titles[[names(d[2])]]))
  
  d[, 2][d[, 2] < 0] = NA #remove negative values
  
  
  
  
  
  means <-
    
    round(tapply(as.numeric(d[[2]]),
                 
                 d[[1]],
                 
                 mean),
          
          digits = 2)
  
  
  
  # plotmeans(
  #   d[[2]] ~ d[[1]],
  #   
  #   digits = 2,
  #   
  #   ccol = 'red',
  #   
  #   mean.labels = F,
  #   
  #   ylab = 'mean',
  #   
  # )
  
  
  
  # boxplot(d[[2]] ~ d[[1]],
  #         
  #         xlab = d[[1]],
  #         
  #         ylab = names(d[[2]]),
  #         
  #         col = rainbow(7))
  
  
  
  aov_content =  aov(d[[2]] ~ as.factor(d[[1]]))
  
  if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
    tuk = TukeyHSD(aov_content)
    
    
    
    print(summary(aov_content))
    
    print(tuk)
    
    plot(tuk)
    
    print(means)
    
    
    
    # dev.off()
    
  }
  
}

# 0.5 creating intermediary datasets ----

# creating table usage device ownership

users_table <-
  
  select(
    ssot_new,
    
    id,
    
    device_picked_1_text,
    
    device_picked_2_text,
    
    device_picked_3_text,
    
    device_picked_1_owned,
    
    device_picked_2_owned ,
    
    device_picked_3_owned ,
    
    device1_usefrequency,
    
    device2_usefrequency,
    
    device3_usefrequency,
    
    region,
    
    LA_Mean,
    
    shddata_remotestorage
    
  )



# 0.5.1 - creating dataset of personally owned devices by participants ----

device1 <-
  
  select(
    subset(users_table, device_picked_1_owned == 1),
    
    id,
    
    device_picked_1_text,
    
    device1_usefrequency,
    
    region,
    
    LA_Mean,
    
    shddata_remotestorage
    
  )

device2 <-
  
  select(
    subset(users_table, device_picked_2_owned == 1),
    
    id,
    
    device_picked_2_text,
    
    device2_usefrequency,
    
    region,
    
    LA_Mean,
    
    shddata_remotestorage
    
  )



device3 <-
  
  select(
    subset(users_table, device_picked_3_owned == 1),
    
    id,
    
    device_picked_3_text,
    
    device3_usefrequency,
    
    region,
    
    LA_Mean,
    
    shddata_remotestorage
    
  )

colnames(device1) <-
  
  c(
    "id",
    
    "Device_Owned",
    
    "Usage",
    
    "region",
    
    "LA_Mean",
    
    "Data_Stored"
    
  )

colnames(device2) <-
  
  c(
    "id",
    
    "Device_Owned",
    
    "Usage",
    
    "region",
    
    "LA_Mean",
    
    "Data_Stored"
    
  )

colnames(device3) <-
  
  c(
    "id",
    
    "Device_Owned",
    
    "Usage",
    
    "region",
    
    "LA_Mean",
    
    "Data_Stored"
    
  )

devices_combined_2 <- rbind(device1, device2, device3)

devices_combined_2 <-
  
  subset(devices_combined
         
         , Usage != 7) #filtering out the options of 'I do not know' due to them not holding additional data



# 0.5.2 - creating dataset of ANOVA household ----



house_anova = select(ssot_new, homeowner, numberofshds, parents)



# 0.5.3 - creating dataset of ANOVA region ----

region_anova = select(
  ssot_new,
  
  region,
  
  sebis_avg,
  
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





# 0.5.4 - creating dataset of usage Household ----



# Adding all device owners to the same data frame and stacking them for analysis

usage_household <-
  
  select(
    ssot_new,
    
    id,
    
    region,
    
    device_picked_1_text,
    
    device_picked_2_text,
    
    device_picked_3_text,
    
    device_picked_1_owned,
    
    device_picked_2_owned ,
    
    device_picked_3_owned ,
    
    device1_usefrequency,
    
    device2_usefrequency,
    
    device3_usefrequency,
    
    parents,
    
    householdsize,
    
  )



device1_answers <-
  
  select(
    subset(usage_household, device_picked_1_owned == 1),
    
    id,
    
    region,
    
    device_picked_1_text,
    
    device1_usefrequency,
    
    parents,
    
    householdsize,
    
  )

device2_answers <-
  
  select(
    subset(usage_household, device_picked_2_owned == 1),
    
    id,
    
    region,
    
    device_picked_2_text,
    
    device2_usefrequency,
    
    parents,
    
    householdsize,
    
  )

device3_answers <-
  
  select(
    subset(usage_household, device_picked_3_owned == 1),
    
    id,
    
    region,
    
    device_picked_3_text,
    
    device3_usefrequency,
    
    parents,
    
    householdsize,
    
  )

colnames(device1_answers) <-
  
  c(
    "id",
    
    "region",
    
    "Device",
    
    "Usage",
    
    "Children",
    
    "Household"
    
  )

colnames(device2_answers) <-
  
  c(
    "id",
    
    "region",
    
    "Device",
    
    "Usage",
    
    "Children",
    
    "Household"
    
  )

colnames(device3_answers) <-
  
  c(
    "id",
    
    "region",
    
    "Device",
    
    "Usage",
    
    "Children",
    
    "Household"
    
  )

device_answers <- rbind(device1_answers, device2_answers, device3_answers)

device_usage <- subset(device_answers, Usage != 7)

##### 1.0 Findings ----



# - D3 - Findings - Interesting device users demographics.



demographic_data <-
  
  select(ssot_new,
         
         gender,
         
         age, id)



device_owners <-
  
  devices_combined %>% distinct(id, .keep_all = TRUE)

demographic_data <-
  
  merge(x = demographic_data, y = device_owners, by = "id")





names(demographic_data)[names(demographic_data) == "region"] <-
  
  "country"

names(demographic_data)[names(demographic_data) == "gender"] <-
  
  "gender"



attach(demographic_data)



demographic_data <-
  
  subset(
    demographic_data,
    
    Device_Owned == "Smart Speaker" |
      
      Device_Owned == "Smart TV" | Device_Owned == "Smart Lightbulb"
    
  )

anon_device <-
  
  data.frame(lapply(demographic_data, function(x) {
    gsub("Smart Speaker", "Device", x)
    
  }))

anon_device <-
  
  data.frame(lapply(anon_device, function(x) {
    gsub("Smart TV", "Device", x)
    
  }))

anon_device <-
  
  data.frame(lapply(anon_device, function(x) {
    gsub("Smart Lightbulb", "Device", x)
    
  }))



anon_mean <- mean(as.numeric(anon_device$age))

anon_median <- median(as.numeric(anon_device$age))





tabyl(anon_device, Device_Owned, gender) %>%
  
  adorn_percentages("row") %>%
  
  adorn_pct_formatting(digits = 1)



detach(demographic_data)





#### F1 H3_Perception - Perceived Intrusion MUIPC----



# perception - how does perceived legislative protection influence Perceived Intrusion

cor.test(ssot_new$LA_Mean, ssot_new$muipc_PerceivedIntrusion_avg)

# strong negative correlation for non device users.

#### F2 H3_Perception - Perceived Intrusion MUIPC----



cor.test(
  subset(ssot_new, numberofshds < 1)$LA_Mean,
  
  subset(ssot_new, numberofshds < 1)$muipc_PerceivedIntrusion_avg
)



#### F3 H3_Perception - Perceived Intrusion MUIPC----



cor.test(
  subset(ssot_new, numberofshds > 0)$LA_Mean,
  
  subset(ssot_new, numberofshds > 0)$muipc_PerceivedIntrusion_avg
)



# F4 manufacturer responsibility for protecting privacy and security ----

cor.test(ssot_new$devicesecurity,
         ssot_new$muipc_PerceivedIntrusion_avg)



# F5 - perceived regulatory protection} and the \textsl{number of unique types of devices} ====

cor.test(ssot_new$regulation_thirdpartyaccess,
         
         ssot_new$numberofshds) #* unwanted access by third parties.

cor.test(ssot_new$regulation_thirdpartyprocessing,
         
         ssot_new$numberofshds) #* unwanted processing and analysis by third parties.

cor.test(ssot_new$LA_Mean,
         
         ssot_new$numberofshds) #* overall correlation construct average



# F6 - #Device Usage x LA_Mean ====



devices_interesting <-
  
  subset(
    devices_combined
    ,
    Device_Owned == "Smart TV" |
      
      Device_Owned == "Smart Lightbulb" |
      
      Device_Owned == "Smart Speaker"
    
  )

devices_grouped = group_by(devices_interesting, Device_Owned)

cor.test(
  subset(devices_interesting, Device_Owned == "Smart Lightbulb")$LA_Mean,
  
  as.numeric(
    subset(devices_interesting, Device_Owned == "Smart Lightbulb")$Usage
    
  ),
  
  method = "pearson"
  
)

cor.test(
  subset(devices_interesting, Device_Owned == "Smart Speaker")$LA_Mean,
  
  as.numeric(
    subset(devices_interesting, Device_Owned == "Smart Speaker")$Usage
    
  ),
  
  method = "pearson"
  
)

cor.test(
  subset(devices_interesting, Device_Owned == "Smart TV")$LA_Mean,
  
  as.numeric(subset(
    devices_interesting, Device_Owned == "Smart TV"
    
  )$Usage),
  
  method = "pearson"
  
)

####



# F 7 - Perception Devices Country ANOVA ----



titles$devicesecurity = "Keeping the Smart Home device secure"

titles$hardwarefix = "Fixing a hardware failure"





calc_anova(region_anova, 'devicesecurity')

calc_anova(region_anova, 'hardwarefix')



no_users = subset(region_anova, numberofshds == 0)

calc_anova(no_users, 'devicesecurity')





# F 8 - Perception Devices Adoption by country and legislative protection ----

cor.test(Participants_GS$LA_Mean, Participants_GS$numberofshds)

cor.test(Participants_UK$LA_Mean, Participants_UK$numberofshds)

cor.test(Participants_US$LA_Mean, Participants_US$numberofshds)



# F 9 - Usage of smart TVs across regions ====

dunnTest(
  x = subset(devices_combined, Device_Owned == "Smart TV")$Usage,
  
  g = as.factor(subset(
    devices_combined, Device_Owned == "Smart TV"
  )$region),
  
  method = "bonferroni"
  
)



# F 10 - Usage of smart TVs across regions ====

calc_anova_house(select(house_anova, homeowner, numberofshds))



# F 11 - Amount of types of smart home devices and household size  ====

cor.test(ssot_new$numberofshds, as.numeric(ssot_new$householdsize)) # greater hh size higher variance of devices



# F12 - Children no children t test

t.test(
  ssot_new$numberofshds ~ ssot_new$parents,
  
  var.equal = TRUE,
  
  alternative = "two.sided"
)





# F13 - children impact usage ----

wilcox.test(as.numeric(device_usage$Usage) ~ device_usage$Children)



mean(as.numeric(subset(device_usage, Children == 1)$Usage))

mean(as.numeric(subset(device_usage, Children == 0)$Usage))



# F14 - smart tv owners with kids higher usage ----



children_smartTV = subset(device_usage, Device == "Smart TV")

wilcox.test(as.numeric(children_smartTV$Usage) ~ children_smartTV$Children)

t.test(as.numeric(children_smartTV$Usage) ~ children_smartTV$Children) # 231 people enough for t test?



mean(as.numeric(subset(children_smartTV, Children == 1)$Usage))

mean(as.numeric(subset(children_smartTV, Children == 0)$Usage))



# F15 - preference for voice based interaction ----

mean(subset(select(ssot_new, interactcomfort_speaker,parents),parents ==0)$interactcomfort_speaker)
mean(subset(select(ssot_new, interactcomfort_speaker,parents),parents ==1)$interactcomfort_speaker)

mean(subset(select(ssot_new, interactcomfort_assistant,parents),parents ==0)$interactcomfort_assistant)
mean(subset(select(ssot_new, interactcomfort_assistant,parents),parents ==1)$interactcomfort_assistant)

wilcox_test(ssot_new, formula = interactcomfort_speaker ~ parents)
wilcox_test(ssot_new, formula = interactcomfort_assistant ~ parents)




# F16 - mean users non users LA_MEAN ----

mean(subset(ssot_new, numberofshds == 0)$LA_Mean)

mean(subset(ssot_new, numberofshds > 0)$LA_Mean)



# F17 - wilcox test MUIPC ----



no_devices <-
  
  subset(
    select(
      ssot_new,
      
      LA_Mean,
      
      LA02_Mean,
      
      sebis_avg,
      
      numberofshds,
      
      softwareupdate,
      
      ensureprivacy,
      
      shdecosystemprotection,
      
      devicesecurity,
      
      hardwarefix,
      
      softwarefix,
      
      muipc_avg,
      
      muipc_PerceivedIntrusion_avg
      
    ),
    
    numberofshds == 0
    
  )

devices <-
  
  subset(
    select(
      ssot_new,
      
      LA_Mean,
      
      LA02_Mean,
      
      sebis_avg,
      
      numberofshds,
      
      softwareupdate,
      
      ensureprivacy,
      
      shdecosystemprotection,
      
      devicesecurity,
      
      hardwarefix,
      
      softwarefix,
      
      muipc_avg,
      
      muipc_PerceivedIntrusion_avg
      
    ),
    
    numberofshds > 0
    
  )

devices$numberofshds = 1



devices_test <- rbind(no_devices, devices)



wilcox.test((no_devices$muipc_avg), devices$muipc_avg)

mean(no_devices$muipc_avg)

mean(devices$muipc_avg)

# F17.2 -  -----



smart_speaker_users = subset(devices_combined, Device_Owned == "Smart Speaker")

mean(as.numeric(smart_speaker_users$Data_Stored))




# F18 - No integration selections -----



no_integration = select(
  ssot_new,
  
  notown_adultprivacy,
  
  notown_childrenprivacy,
  
  notown_guestprivacy,
  
  notown_petprivacy,
  
  notown_nobenefit,
  
  notown_cost,
  
  notown_nointernet
)



prop.table(table(no_integration$notown_adultprivacy))

prop.table(table(no_integration$notown_childrenprivacy))

prop.table(table(no_integration$notown_guestprivacy))

prop.table(table(no_integration$notown_cost))

# F19 - No integration selections -----


devicerisk_selected_devices = select(
  ssot_new,
  devicerisk_tv,
  devicerisk_lightbulb,
  devicerisk_speaker
)

tv = as.data.frame(ssot_new$devicerisk_tv)
tv$id = 'tv'

speaker = as.data.frame(ssot_new$devicerisk_speaker)
speaker$id = 'speaker'

lightbulb= as.data.frame(ssot_new$devicerisk_lightbulb)
lightbulb$id = 'lightbulb'


colnames(tv)=colnames(speaker)=colnames(lightbulb)

interesting_devices_risk_stacked = rbind(tv,speaker,lightbulb)
colnames(interesting_devices_risk_stacked) = c('risk','device')
aov_content =  aov(interesting_devices_risk_stacked$risk~ interesting_devices_risk_stacked$device)

tuk = TukeyHSD(aov_content)
print(summary(aov_content))
print(tuk)

##### 9.0 -  Tables ----



# T1 - overview demographics - table1 ----



demographic_data_t1 <-
  
  select(ssot_new,
         
         region,
         
         gender,
         
         age)

names(demographic_data_t1)[names(demographic_data_t1) == "region"] <-
  
  "country"

names(demographic_data_t1)[names(demographic_data_t1) == "gender"] <-
  
  "gender"



attach(demographic_data_t1)

#3 people preferred not to enter their age

age_data <- summaryBy(
  age ~ country,
  
  data = demographic_data_t1,
  
  FUN = c(median, mean, sd, min, max),
  
  na.rm = TRUE
  
)



gender_data <- tabyl(demographic_data_t1, country, gender) %>%
  
  adorn_percentages("row") %>%
  
  adorn_pct_formatting(digits = 1)



gender_data_absolute = tabyl(demographic_data_t1, country, gender)



table1 <-
  
  bind_cols(count(country),
            
            age_data[2:6],
            
            gender_data[2:4],
            
            gender_data_absolute[2:4])



detach(demographic_data_t1)



gs = subset(demographic_data_t1, country == "GS")

uk = subset(demographic_data_t1, country == "UK")

us = subset(demographic_data_t1, country == "US")



count(gs$gender)

count(uk$gender)

count(us$gender)











# T2 - Demographic by device - table 2 ====

device_information_region = select(ssot_new, numberofshds, region)

counts <-
  
  ddply(
    device_information_region,
    
    .(
      device_information_region$numberofshds,
      
      device_information_region$region
      
    ),
    
    nrow
    
  )

table2 <-
  
  data.table(device_information_region) # transpose to data.table

table2 = table2[, list(Freq = .N), by = list(numberofshds, region)] # use list to name var directly





# T3 -  H3_Perception - How is perception affected by the feeling of legislative protection ? (table 3 ) ----



#devicerisk_coffeemaker-20 correspond to the perceived risk of a certain device

#11 Smart Lightbuld; 14 Smart Speaker; 16 Smart TV



# table in bold.

# over all devices. Different devices and use cases generate noise

risk_all_devices <- rowMeans(
  select(
    ssot_new,
    
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

cor.test(ssot_new$LA_Mean, risk_all_devices, adjust.method = "Bonferroni")

subset(select(cor_test(
  select(
    ssot_new,
    
    LA_Mean,
    
    devicerisk_lightbulb,
    
    devicerisk_speaker,
    
    devicerisk_tv,
  )
  
), var1, var2, cor, p),

var1 == "LA_Mean" & var2 != "LA_Mean")



cor.test(ssot_new$regulation_thirdpartyaccess,
         risk_all_devices,
         adjust.method = "Bonferroni")

subset(
  select(cor_test(
    select(
      ssot_new,
      
      regulation_thirdpartyaccess,
      
      devicerisk_lightbulb,
      
      devicerisk_speaker,
      
      devicerisk_tv,
    )
    
  ), var1, var2, cor, p),
  
  var1 == "regulation_thirdpartyaccess" &
    var2 != "regulation_thirdpartyaccess"
)





cor.test(ssot_new$regulation_thirdpartysharing,
         risk_all_devices,
         adjust.method = "Bonferroni")

subset(
  select(cor_test(
    select(
      ssot_new,
      
      regulation_thirdpartysharing,
      
      devicerisk_lightbulb,
      
      devicerisk_speaker,
      
      devicerisk_tv,
    )
    
  ), var1, var2, cor, p),
  
  var1 == "regulation_thirdpartysharing" &
    var2 != "regulation_thirdpartysharing"
)





cor.test(ssot_new$regulation_thirdpartyprocessing,
         risk_all_devices,
         adjust.method = "Bonferroni")

subset(
  select(cor_test(
    select(
      ssot_new,
      
      regulation_thirdpartyprocessing,
      
      devicerisk_lightbulb,
      
      devicerisk_speaker,
      
      devicerisk_tv,
    )
    
  ), var1, var2, cor, p),
  
  var1 == "regulation_thirdpartyprocessing" &
    var2 != "regulation_thirdpartyprocessing"
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
