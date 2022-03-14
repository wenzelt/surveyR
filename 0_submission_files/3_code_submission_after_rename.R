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

cor_test(select(ssot_new, LA_Mean, muipc_PerceivedIntrusion_avg))

# strong negative correlation for non device users.

#### F2 H3_Perception - Perceived Intrusion MUIPC----



cor_test(select(
  subset(ssot_new, numberofshds < 1),
  
  LA_Mean,
  
  muipc_PerceivedIntrusion_avg
))



#### F3 H3_Perception - Perceived Intrusion MUIPC----



cor_test(select(
  subset(ssot_new, numberofshds > 0),
  
  LA_Mean,
  
  muipc_PerceivedIntrusion_avg
))



# F4 manufacturer responsibility for protecting privacy and security ----

cor_test(select(ssot_new, devicesecurity, muipc_PerceivedIntrusion_avg))



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

LA_MEAN_USAGE_DEVICE_INTERESTING <-
  
  dplyr::summarize(devices_grouped, cor(LA_Mean, as.numeric(Usage)))

LA_MEAN_USAGE_DEVICE_INTERESTING[3] <- "Pearson"

LA_MEAN_USAGE_DEVICE_INTERESTING[4] <-
  
  c(
    cor.test(
      subset(devices_interesting, Device_Owned == "Smart Lightbulb")$LA_Mean,
      
      as.numeric(
        subset(devices_interesting, Device_Owned == "Smart Lightbulb")$Usage
        
      ),
      
      method = "pearson"
      
    )$p.value,
    
    cor.test(
      subset(devices_interesting, Device_Owned == "Smart Speaker")$LA_Mean,
      
      as.numeric(
        subset(devices_interesting, Device_Owned == "Smart Speaker")$Usage
        
      ),
      
      method = "pearson"
      
    )$p.value,
    
    cor.test(
      subset(devices_interesting, Device_Owned == "Smart TV")$LA_Mean,
      
      as.numeric(subset(
        devices_interesting, Device_Owned == "Smart TV"
        
      )$Usage),
      
      method = "pearson"
      
    )$p.value
    
  )

colnames(LA_MEAN_USAGE_DEVICE_INTERESTING) <-
  
  c("Device", "Cor", "Method", "P-Value")

LA_MEAN_USAGE_DEVICE_INTERESTING

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
  
  g = as.factor(
    subset(devices_combined, Device_Owned == "Smart TV")$region
    
  ),
  
  method = "bonferroni"
  
)$res



# F 10 - Usage of smart TVs across regions ====

calc_anova_house(select(house_anova, homeowner, numberofshds))



# F 11 - Amount of types of smart home devices and household size  ====

cor.test(ssot_new$numberofshds, as.numeric(ssot_new$householdsize)) # greater hh size higher variance of devices



# F12 - Children no children t test

t.test(
  ssot_new$numberofshds ~ ssot_new$numberofchildren,
  
  var.equal = TRUE,
  
  alternative = "two.sided"
)





# F13 - children impact usage ----

wilcox.test(as.numeric(temp_usage$Usage) ~ temp_usage$Children)



mean(as.numeric(subset(temp_usage, Children == 1)$Usage))

mean(as.numeric(subset(temp_usage, Children == 0)$Usage))



# F14 - smart tv owners with kids higher usage ----



children_smartTV = subset(temp_usage, Device == "Smart TV")

wilcox.test(as.numeric(children_smartTV$Usage) ~ children_smartTV$Children)

t.test(as.numeric(children_smartTV$Usage) ~ children_smartTV$Children) # 231 people enough for t test?



mean(as.numeric(subset(children_smartTV, Children == 1)$Usage))

mean(as.numeric(subset(children_smartTV, Children == 0)$Usage))



# F15 - preference for voice based interaction

data.frame(
  "Usage_type" = c(
    "Voice commands via a Smart Speaker",
    
    "Voice commands via a Smartphone Voice Assistant"
    
  ),
  
  "p_value" = c(
    wilcox_test(ssot_new, interactcomfort_speaker ~ numberofchildren)$p,
    
    wilcox_test(ssot_new, interactcomfort_assistant ~ numberofchildren)$p
    
  ),
  
  "effect_size" = c(
    wilcox_effsize(ssot_new, formula = interactcomfort_speaker ~ numberofchildren)$effsize,
    
    wilcox_effsize(ssot_new, formula = interactcomfort_assistant ~ numberofchildren)$effsize
    
  )
  
)





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





calc_anova(region_anova, 'numberofshds')
