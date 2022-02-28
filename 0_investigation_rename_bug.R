devices_old = subset(d,`Current Country of Residence` != "NA")


library(readr)
test_ssot_csv <- read_csv("original_data_set/singleSourceOfTruth.csv")

original_set <- soSciDataSet 



# 0.2.2   read export from prolific academic
prolific_export_DE <- read_csv("Excels/prolific_export_DE.csv")
prolific_export_UK <- read_csv("Excels/prolific_export_UK.csv")
prolific_export_US <- read_csv("Excels/prolific_export_US.csv")

# 0.2.3   generating prolific table from /excels folder
prolific_table_original = rbind(prolific_export_DE, prolific_export_US, prolific_export_UK)
table(prolific_table_original$status)
# APPROVED  REJECTED  RETURNED TIMED-OUT
# 463         2        28        19

# 0.2.4 filter out non-approved people from prolific academic
prolific_table_filtered = subset(prolific_table_original, status == "APPROVED")

# n = 463
# 0.2.5 merge into full data frame by participant ID in the prolific dataset and their entered prolific ID in the survey
SSOT <-
  merge(prolific_table_filtered,
        original_set,
        by.x = "participant_id",
        by.y = "A008_01",
  )

# 0.2.5 remove participants who put "NA" countries of residence
SSOT <-
  subset(SSOT,
         `Current Country of Residence` != "NA")

# 0.2.7 consolidate DACH region
SSOT$`Current Country of Residence` <-
  replace(
    as.character(SSOT$`Current Country of Residence`),
    SSOT$`Current Country of Residence` == "Germany",
    "DACH"
  )
SSOT$`Current Country of Residence` <-
  replace(
    as.character(SSOT$`Current Country of Residence`),
    SSOT$`Current Country of Residence` == "Switzerland",
    "DACH"
  )
SSOT$`Current Country of Residence` <-
  replace(
    as.character(SSOT$`Current Country of Residence`),
    SSOT$`Current Country of Residence` == "Austria",
    "DACH"
  )




# 0.2.8 remove everybody not from one of the specified regions
SSOT <-
  subset(
    SSOT,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States" |
      `Current Country of Residence` == "United Kingdom"
  )

# 0.2.6 remove all people not interested in giving good answers
# Have you answered all questions in the study according to the provided instructions? # reduces by 8 
SSOT <-
  subset(SSOT,
         CS06 == 1)
# Will you provide your best answers to each question in this study? # reduces by 0 
SSOT <-
  subset(SSOT,
         CS10 == 1)


# n = 439 here

SSOT <- subset(SSOT, S101_13 == 1)

# copy to new dataset for modification
ssot_new <- SSOT

#remove attention check column
test_ssot <-
  within(ssot_new, rm(S101_13))


# create bool if participant has children or not from numeric var
ssot_new$A004 <-
  ifelse(ssot_new$A004 > 1, 1, 0) # having children

# 0.2.9 Translating Device names  ----
# 0.2.9 Translating Device names  ----
ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Lautsprecher",
    "Smart Speaker"
  )
ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Glühbirne",
    "Smart Lightbulb"
  )

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Lautsprecher",
    "Smart Speaker"
  )
ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Glühbirne",
    "Smart Lightbulb"
  )

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Lautsprecher",
    "Smart Speaker"
  )
ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Glühbirne",
    "Smart Lightbulb"
  )

# translating rest of devices:
#translating device names from german to english
ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Kaffeemaschine",
    "Smart Coffee Maker"
  )
ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Geschirrspüler",
    "Smart Dishwasher"
  )
ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Gartengerät",
    "Smart Lawnmower"
  )

ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Heiz-/Kühlsystem",
    "Smart Heating/Cooling System"
  )
ssot_new$R232_01 <-
  replace(as.character(ssot_new$R232_01),
          ssot_new$R232_01 == "Smart Herd",
          "Smart Stove")

ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Kühlschrank",
    "Smart Fridge"
  )
ssot_new$R232_01 <-
  replace(as.character(ssot_new$R232_01),
          ssot_new$R232_01 == "Smart Ofen",
          "Smart Oven")
ssot_new$R232_01 <-
  replace(as.character(ssot_new$R232_01),
          ssot_new$R232_01 == "Smart Roboter",
          "Smart Robot")
ssot_new$R232_01 <-
  replace(as.character(ssot_new$R232_01),
          ssot_new$R232_01 == "Smart Spielzeug",
          "Smart Toy")
ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Türklingel",
    "Smart Doorbell"
  )
ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Türschloss",
    "Smart Door Lock"
  )

ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Heim-Überwachungssystem",
    "Smart Home Monitoring System"
  )

ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Waschmaschine",
    "Smart Washing Machine"
  )

ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Stromzähler",
    "Smart Electricity Meter"
  )

ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Staubsauger",
    "Smart Vacuum Cleaner"
  )

ssot_new$R232_01 <-
  replace(
    as.character(ssot_new$R232_01),
    ssot_new$R232_01 == "Smart Steckdose",
    "Smart Electrical Outlet"
  )


## d 2

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Kaffeemaschine",
    "Smart Coffee Maker"
  )
ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Geschirrspüler",
    "Smart Dishwasher"
  )
ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Gartengerät",
    "Smart Lawnmower"
  )

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Heiz-/Kühlsystem",
    "Smart Heating/Cooling System"
  )
ssot_new$R232_02 <-
  replace(as.character(ssot_new$R232_02),
          ssot_new$R232_02 == "Smart Herd",
          "Smart Stove")

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Kühlschrank",
    "Smart Fridge"
  )
ssot_new$R232_02 <-
  replace(as.character(ssot_new$R232_02),
          ssot_new$R232_02 == "Smart Ofen",
          "Smart Oven")
ssot_new$R232_02 <-
  replace(as.character(ssot_new$R232_02),
          ssot_new$R232_02 == "Smart Roboter",
          "Smart Robot")
ssot_new$R232_02 <-
  replace(as.character(ssot_new$R232_02),
          ssot_new$R232_02 == "Smart Spielzeug",
          "Smart Toy")
ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Türklingel",
    "Smart Doorbell"
  )
ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Türschloss",
    "Smart Door Lock"
  )

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Heim-Überwachungssystem",
    "Smart Home Monitoring System"
  )

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Waschmaschine",
    "Smart Washing Machine"
  )

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Stromzähler",
    "Smart Electricity Meter"
  )

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Staubsauger",
    "Smart Vacuum Cleaner"
  )

ssot_new$R232_02 <-
  replace(
    as.character(ssot_new$R232_02),
    ssot_new$R232_02 == "Smart Steckdose",
    "Smart Electrical Outlet"
  )

## d3



ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Kaffeemaschine",
    "Smart Coffee Maker"
  )
ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Geschirrspüler",
    "Smart Dishwasher"
  )
ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Gartengerät",
    "Smart Lawnmower"
  )

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Heiz-/Kühlsystem",
    "Smart Heating/Cooling System"
  )
ssot_new$R232_03 <-
  replace(as.character(ssot_new$R232_03),
          ssot_new$R232_03 == "Smart Herd",
          "Smart Stove")

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Kühlschrank",
    "Smart Fridge"
  )
ssot_new$R232_03 <-
  replace(as.character(ssot_new$R232_03),
          ssot_new$R232_03 == "Smart Ofen",
          "Smart Oven")
ssot_new$R232_03 <-
  replace(as.character(ssot_new$R232_03),
          ssot_new$R232_03 == "Smart Roboter",
          "Smart Robot")
ssot_new$R232_03 <-
  replace(as.character(ssot_new$R232_03),
          ssot_new$R232_03 == "Smart Spielzeug",
          "Smart Toy")
ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Türklingel",
    "Smart Doorbell"
  )
ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Türschloss",
    "Smart Door Lock"
  )

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Heim-Überwachungssystem",
    "Smart Home Monitoring System"
  )

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Waschmaschine",
    "Smart Washing Machine"
  )

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Stromzähler",
    "Smart Electricity Meter"
  )

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Staubsauger",
    "Smart Vacuum Cleaner"
  )

ssot_new$R232_03 <-
  replace(
    as.character(ssot_new$R232_03),
    ssot_new$R232_03 == "Smart Steckdose",
    "Smart Electrical Outlet"
  )

# 0.3 appending measures to dataset ----

# appending avg of our legislative construct to the dataset
ssot_new$LA_Mean <-
  rowMeans(select(ssot_new, LA01_01:LA01_03))
ssot_new$LA02_Mean <-
  rowMeans(select(ssot_new, LA02_01:LA02_03))

# appending Sebis/MUIPC Averages to dataset
ssot_new$sebis_avg <-
  rowMeans(select(ssot_new, S101_01:S101_12))
ssot_new$sebis_DeviceSecurement_avg <-
  rowMeans(select(ssot_new, S101_01:S101_04))
ssot_new$sebis_ProactiveAwareness_avg <-
  rowMeans(select(ssot_new, S101_05:S101_09))
ssot_new$sebis_UpdatingBehaviour_avg <-
  rowMeans(select(ssot_new, S101_10:S101_12))

ssot_new$muipc_avg <-
  rowMeans(select(ssot_new, S102_01:S102_09))
ssot_new$muipc_PersonalInfo_avg <-
  rowMeans(select(ssot_new, S102_01:S102_03))
ssot_new$muipc_PerceivedSur_avg <-
  rowMeans(select(ssot_new, S102_04:S102_06))
ssot_new$muipc_PerceivedIntrusion_avg <-
  rowMeans(select(ssot_new, S102_07:S102_09))

# converting household size to numeric variable
ssot_new$A005 <-
  as.numeric(ssot_new$A005)



# Dataset into different countries for later analyses
Participants_DACH <-
  subset(ssot_new,
         `Current Country of Residence` == "DACH")
Participants_US <-
  subset(ssot_new,
         `Current Country of Residence` == "United States")
Participants_UK <-
  subset(ssot_new,
         `Current Country of Residence` == "United Kingdom")


u <-
  select(
    ssot_new,
    participant_id,
    `Current Country of Residence`,
    R232_01,
    R232_02,
    R232_03,
    R233_01,
    R233_02 ,
    R233_03 ,
    R501,
    R503,
    R505,
  )

d1 <-
  select(subset(u, R233_01 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_01,
         R501)
d2 <-
  select(subset(u, R233_02 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_02,
         R503)
d3 <-
  select(subset(u, R233_03 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_03,
         R505)
colnames(d1) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage")
colnames(d2) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage")
colnames(d3) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage")
d <- rbind(d1, d2, d3)
d <- subset(d, Usage != 7)


dunnTest(
  x = as.numeric(subset(d, Device == "Smart TV")$Usage),
  g = as.factor(subset(d, Device == "Smart TV")$`Current Country of Residence`),
  method = "bonferroni"
)$res


#----


# appending avg of our legislative construct to the dataset
SSOT$LA_Mean <-
  rowMeans(select(SSOT, LA01_01:LA01_03))
SSOT$LA02_Mean <-
  rowMeans(select(SSOT, LA02_01:LA02_03))

# appending Sebis/MUIPC Averages to dataset
SSOT$sebis_avg <-
  rowMeans(select(SSOT, S101_01:S101_12))
SSOT$sebis_DeviceSecurement_avg <-
  rowMeans(select(SSOT, S101_01:S101_04))
SSOT$sebis_ProactiveAwareness_avg <-
  rowMeans(select(SSOT, S101_05:S101_09))
SSOT$sebis_UpdatingBehaviour_avg <-
  rowMeans(select(SSOT, S101_10:S101_12))

SSOT$muipc_avg <-
  rowMeans(select(SSOT, S102_01:S102_09))
SSOT$muipc_PersonalInfo_avg <-
  rowMeans(select(SSOT, S102_01:S102_03))
SSOT$muipc_PerceivedSur_avg <-
  rowMeans(select(SSOT, S102_04:S102_06))
SSOT$muipc_PerceivedIntrusion_avg <-
  rowMeans(select(SSOT, S102_07:S102_09))

# converting household size to numeric variable
SSOT$A005 <-
  as.numeric(SSOT$A005)



# Dataset into different countries for later analyses
Participants_DACH <-
  subset(SSOT,
         `Current Country of Residence` == "DACH")
Participants_US <-
  subset(SSOT,
         `Current Country of Residence` == "United States")
Participants_UK <-
  subset(ssot_new,
         `Current Country of Residence` == "United Kingdom")



users_device <-
  select(
    SSOT,
    participant_id,
    `Current Country of Residence`,
    R232_01,
    R232_02,
    R232_03,
    R233_01,
    R233_02 ,
    R233_03 ,
    R501,
    R503,
    R505,
    LA_Mean
  )

device1 <-
  select(subset(users_device, R233_01 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_01,
         R501,LA_Mean)
device2 <-
  select(subset(users_device, R233_02 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_02,
         R503,LA_Mean)
device3 <-
  select(subset(users_device, R233_03 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_03,
         R505,LA_Mean)
colnames(device1) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage","LA_Mean")
colnames(device2) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage","LA_Mean")
colnames(device3) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage","LA_Mean")
devices_c <- rbind(device1, device2, device3)
devices_c <- subset(devices_c, Usage != 7)


dunnTest(
  x = as.numeric(subset(devices_c, Device == "Smart TV")$Usage),
  g = as.factor(subset(devices_c, Device == "Smart TV")$`Current Country of Residence`),
  method = "bonferroni"
)$res

dunnTest(
  x = as.numeric(subset(devices_c, Device == "Smart TV")$LA_Mean),
  g = as.factor(subset(devices_c, Device == "Smart TV")$`Current Country of Residence`),
  method = "bonferroni"
)$res



