ssot_new <- read_csv("0_submission_files/0_submission_dataset.csv")

# 0.3 appending measures to dataset ----



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
  
  
  
  plotmeans(
    d[[2]] ~ d[[1]],
    
    digits = 2,
    
    ccol = 'red',
    
    mean.labels = F,
    
    ylab = 'mean',
    
  )
  
  
  
  boxplot(d[[2]] ~ d[[1]],
          
          xlab = d[[1]],
          
          ylab = names(d[[2]]),
          
          col = rainbow(7))
  
  
  
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

devices_combined <- rbind(device1, device2, device3)

devices_combined <-
  
  subset(devices_combined
         
         , Usage != 7) #filtering out the options of 'I do not know' due to them not holding additional data



# 0.5.2 - creating dataset of ANOVA household ----



house_anova = select(ssot_new, homeowner, numberofshds, numberofchildren)



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
    
    numberofchildren,
    
    householdsize,
    
  )



temp1 <-
  
  select(
    subset(usage_household, device_picked_1_owned == 1),
    
    id,
    
    region,
    
    device_picked_1_text,
    
    device1_usefrequency,
    
    numberofchildren,
    
    householdsize,
    
  )

temp2 <-
  
  select(
    subset(usage_household, device_picked_2_owned == 1),
    
    id,
    
    region,
    
    device_picked_2_text,
    
    device2_usefrequency,
    
    numberofchildren,
    
    householdsize,
    
  )

temp3 <-
  
  select(
    subset(usage_household, device_picked_3_owned == 1),
    
    id,
    
    region,
    
    device_picked_3_text,
    
    device3_usefrequency,
    
    numberofchildren,
    
    householdsize,
    
  )

colnames(temp1) <-
  
  c(
    "id",
    
    "region",
    
    "Device",
    
    "Usage",
    
    "Children",
    
    "Household"
    
  )

colnames(temp2) <-
  
  c(
    "id",
    
    "region",
    
    "Device",
    
    "Usage",
    
    "Children",
    
    "Household"
    
  )

colnames(temp3) <-
  
  c(
    "id",
    
    "region",
    
    "Device",
    
    "Usage",
    
    "Children",
    
    "Household"
    
  )

temp <- rbind(temp1, temp2, temp3)

temp_usage <- subset(temp, Usage != 7)