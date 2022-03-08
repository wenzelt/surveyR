##### 0.1 Installation of dependencies ----

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

### 0.2 Data cleaning ----

library(readr)

# 0.2.1   read export from SoSci
original_set <-
  read_delim(
    "original_data_set/rdata_smarthomestudy_2019-12-08_11-45.csv",
    delim = "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  )

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
ssot_new <-
  within(ssot_new, rm(S101_13))

# create bool if participant has children or not from numeric var
ssot_new$A004 <-
  ifelse(ssot_new$A004 > 1, 1, 0) # having children

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



##### 1.0 RQ1

# variance of devices correlates with the Legislative satisfaction lightly
# reporting did not change on this

# 4.1.2 use
cor.test(ssot_new$LA01_01,
         ssot_new$R101) #* unwanted access by third parties.
cor.test(ssot_new$LA01_02,
         ssot_new$R101) #- unwanted sharing with third parties.
cor.test(ssot_new$LA01_03,
         ssot_new$R101) #* unwanted processing and analysis by third parties.
cor.test(ssot_new$LA_Mean,
         ssot_new$R101) #* overall correlation construct average

# mean of device owners and device non owners
# reporting did not change on this
mean(subset(ssot_new, R101 == 0)$LA_Mean)
mean(subset(ssot_new, R101 > 0)$LA_Mean)

# [EXPLANATION] Overall shows that the more the participants feel protected from evil entities by legislation the more different devices they own
wilcox.test(subset(ssot_new, R101 == 0)$LA_Mean,
            subset(ssot_new, R101 > 0)$LA_Mean)

#### 1.1 H1_Pairwise comparison of, only used for discussion ----

# [EXPLANATION] Is there a difference the effect of Legislative opinion on amount of devices by regions investigated?
kruskal_test(rbind(Participants_UK, Participants_US), LA_Mean ~ R101) #*
kruskal_test(rbind(Participants_DACH, Participants_US), LA_Mean ~ R101) #*
kruskal_test(rbind(Participants_DACH, Participants_UK), LA_Mean ~ R101) #*


###UK US
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  LA01_01 ~ R101
)#ns
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  LA01_02 ~ R101
)#s
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  LA01_03 ~ R101
)#s

###DACH US
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  LA01_01 ~ R101
)#s
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  LA01_02 ~ R101
)#s
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  LA01_03 ~ R101
)#ns

###UK DACH
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "DACH"
  ),
  LA01_01 ~ R101
) # s
cor.test(ssot_new$LA01_01,
         ssot_new$R101)

kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "DACH"
  ),
  LA01_02 ~ R101
) #s
kruskal_test(
  subset(
    ssot_new,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "DACH"
  ),
  LA01_03 ~ R101
) #s

#### 1.2.1 Usage of smart home devices influenced by Legislation (LA) ----

# creating table usage device ownership
users_table <-
  select(
    ssot_new,
    participant_id,
    R232_01,
    R232_02,
    R232_03,
    R233_01,
    R233_02 ,
    R233_03 ,
    R501,
    R503,
    R505,
    `Current Country of Residence`,
    LA_Mean
    
  )

# creating dataset of personally owned devices by participants
device1 <-
  select(
    subset(users_table, R233_01 == 1),
    participant_id,
    R232_01,
    R501,
    `Current Country of Residence`,
    LA_Mean
  )
device2 <-
  select(
    subset(users_table, R233_02 == 1),
    participant_id,
    R232_02,
    R503,
    `Current Country of Residence`,
    LA_Mean
  )

device3 <-
  select(
    subset(users_table, R233_03 == 1),
    participant_id,
    R232_03,
    R505,
    `Current Country of Residence`,
    LA_Mean
  )
colnames(device1) <-
  c("participant_id",
    "Device_Owned",
    "Usage",
    "Current Country of Residence",
    "LA_Mean")
colnames(device2) <-
  c("participant_id",
    "Device_Owned",
    "Usage",
    "Current Country of Residence",
    "LA_Mean")
colnames(device3) <-
  c("participant_id",
    "Device_Owned",
    "Usage",
    "Current Country of Residence",
    "LA_Mean")
devices_combined <- rbind(device1, device2, device3)
devices_combined <-
  subset(devices_combined
         , Usage != 7) #filtering out the options of 'I do not know' due to them not holding additional data

cor.test(devices_combined$LA_Mean,
         as.numeric(devices_combined$Usage))

# [Explanation] we find that the usage of devices correlates positively when the participants felt protected by legislation


#Device Usage x LA_Mean
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

#overall correlation over all 3 devices
cor.test(devices_interesting$LA_Mean,
         as.numeric(devices_interesting$Usage),
         method = "pearson")

#All devices in one table
ddply(
  devices_grouped,
  "Device_Owned",
  summarise,
  corr = cor(LA_Mean, as.numeric(Usage), method = "pearson")
)

#individual correlations
cor.test(
  subset(devices_interesting, Device_Owned == "Smart TV")$LA_Mean,
  as.numeric(subset(
    devices_interesting, Device_Owned == "Smart TV"
  )$Usage),
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
  subset(devices_interesting, Device_Owned == "Smart Lightbulb")$LA_Mean,
  as.numeric(
    subset(devices_interesting, Device_Owned == "Smart Lightbulb")$Usage
  ),
  method = "pearson"
)

#### 1.2.2 H2_Disabled Features ----
# creating disabled features table
disabled_features <-
  select(
    ssot_new,
    participant_id,
    R507,
    R507_01,
    R510,
    R510_01,
    R513,
    R513_01,
    LA01_01,
    LA01_02,
    LA01_03,
  )
disabled_features$choice <-
  ifelse(
    disabled_features$R507 == 1 |
      disabled_features$R510 == 1 |
      disabled_features$R513 == 1,
    1,
    0
  )
disabled_features["LA01_Mean"] = rowMeans(select(disabled_features, LA01_01:LA01_03))
wilcox.test(disabled_features$LA01_Mean, disabled_features$choice) #p-value < 2.2e-16
wilcox.test(disabled_features$LA01_01, disabled_features$choice) #p-value < 2.2e-16
wilcox.test(disabled_features$LA01_02, disabled_features$choice) #p-value < 2.2e-16
wilcox.test(disabled_features$LA01_03, disabled_features$choice) #p-value < 2.2e-16
aggregate(LA01_01 ~ choice, data = disabled_features, mean)
aggregate(LA01_02 ~ choice, data = disabled_features, mean)
aggregate(LA01_03 ~ choice, data = disabled_features, mean)
table(disabled_features$choice)

#### 1.3.1 H3_Perception - How is perception affected by the feeling of legislative protection ? (table) ----

#E201_01-20 correspond to the perceived risk of a certain device
#11 Smart Lightbuld; 14 Smart Speaker; 16 Smart TV

# table in bold.
# over all devices. Different devices and use cases generate noise
risk_all_devices <- rowMeans(select(ssot_new, E201_01:E201_20))


#correlation of interesting devices (3)
# table non bold - first section
cor.test(ssot_new$LA_Mean, risk_all_devices, adjust.method = "Bonferroni")
subset(select(cor_test(
  select(ssot_new,
         LA_Mean,
         E201_11,
         E201_14,
         E201_16,)
), var1, var2, cor, p),
var1 == "LA_Mean" & var2 != "LA_Mean")

cor.test(ssot_new$LA01_01, risk_all_devices, adjust.method = "Bonferroni")
subset(select(cor_test(
  select(ssot_new,
         LA01_01,
         E201_11,
         E201_14,
         E201_16,)
), var1, var2, cor, p),
var1 == "LA01_01" & var2 != "LA01_01")


cor.test(ssot_new$LA01_02, risk_all_devices, adjust.method = "Bonferroni")
subset(select(cor_test(
  select(ssot_new,
         LA01_02,
         E201_11,
         E201_14,
         E201_16,)
), var1, var2, cor, p),
var1 == "LA01_02" & var2 != "LA01_02")


cor.test(ssot_new$LA01_03, risk_all_devices, adjust.method = "Bonferroni")
subset(select(cor_test(
  select(ssot_new,
         LA01_03,
         E201_11,
         E201_14,
         E201_16,)
), var1, var2, cor, p),
var1 == "LA01_03" & var2 != "LA01_03")

#### 1.3.2 H3_Perception - Perceived Responsibility----
#Correlation between LA01 and Manufacturer responsibility
subset(select(cor_test(
  select(ssot_new, LA_Mean, A204_01:A204_06), conf.level = 0.95,
), var1, var2, cor, p),
var1 == "LA_Mean" & var2 != "LA_Mean")

#### 1.3.3 H3_Perception - Perceived Surveillance MUIPC----

# perception - how does perceived legislative protection influence perceived surveillance
cor_test(select(ssot_new, LA_Mean, muipc_PerceivedSur_avg))
# strong negative correlation for non device users.
cor_test(select(subset(ssot_new, R101 < 1),
                LA_Mean,
                muipc_PerceivedSur_avg))
cor_test(select(subset(ssot_new, R101 > 0),
                LA_Mean,
                muipc_PerceivedSur_avg))
# higher legislative protection negatively impacts perceived surveillance greatly


cor.test(ssot_new$sebis_ProactiveAwareness_avg, rowMeans(select(ssot_new, E201_11)))#device risk
cor.test(ssot_new$sebis_ProactiveAwareness_avg, rowMeans(select(ssot_new, E203)))
chisq_test(ssot_new$sebis_ProactiveAwareness_avg,ssot_new$E203_04)


#### 1.3.4 #non users more privacy aware? ----
no_devices <- subset(select(ssot_new,LA_Mean, LA02_Mean, sebis_avg, R101, A204_01:A204_06, muipc_avg, muipc_PerceivedSur_avg),R101==0) 
devices <- subset(select(ssot_new,LA_Mean, LA02_Mean, sebis_avg, R101,  A204_01:A204_06, muipc_avg, muipc_PerceivedSur_avg),R101>0) 
devices$R101 = 1

devices_test <- rbind(no_devices,devices)
wilcox.test(devices_test$sebis_avg,devices_test$R101)
wilcox.test(devices_test$sebis_avg,devices_test$R101)

mean(no_devices$sebis_avg)
mean(devices$sebis_avg)
wilcox.test(devices$sebis_avg,(no_devices$sebis_avg))
mean(no_devices$sebis_avg)
mean(devices$sebis_avg)

wilcox.test((no_devices$muipc_avg),devices$muipc_avg)
mean(no_devices$muipc_avg)
mean(devices$muipc_avg)


subset(select(cor_test(
  select(devices, LA_Mean, A204_01:A204_06), conf.level = 0.95,
), var1, var2, cor, p),
var1 == "LA_Mean" & var2 != "LA_Mean")

# manufacturer responsibility for protecting privacy and security
cor_test(select(ssot_new, A204_04, muipc_PerceivedSur_avg))

#### 1.3.5 #non users more privacy aware? ----
cor.test(ssot_new$muipc_PerceivedSur_avg,ssot_new$LA_Mean)
cor.test(no_devices$muipc_PerceivedSur_avg,no_devices$LA_Mean)
cor.test(devices$muipc_PerceivedSur_avg,devices$LA_Mean)

#### 1.3.6 - perceived manufacturer responsibility} for privacy and security protection. ----
cor.test(no_devices$LA_Mean,no_devices$A204_04)
cor.test(ssot_new$LA_Mean,rowMeans(select(ssot_new,A204_01:A204_06)))
cor.test(devices$LA_Mean,devices$A204_06)

#### 1.3.7 - device_owners more disabled features ? 

disabled_features_users <-
  select(
    ssot_new,
    participant_id,
    R507,
    R510,
    R513,
    R101
  )
disabled_features_users = subset(disabled_features_users,R101!=0) 
disabled_features_users$choice <-
  ifelse(
    disabled_features_users$R507 == 1 |
      disabled_features_users$R510 == 1 |
      disabled_features_users$R513 == 1,
    1,
    0
  )
count(disabled_features_users$choice)


##### 2.0 RQ2 ----

cor.test(Participants_DACH$LA_Mean,
         Participants_DACH$R101) #*
cor.test(Participants_UK$LA_Mean,
         Participants_UK$R101)
cor.test(Participants_US$LA_Mean,
         Participants_US$R101)

#[EXPLANATION] Check if there is a significant difference in amount of devices over regions
kruskal_test(ssot_new,
             R101 ~ `Current Country of Residence`)
#### 2.2.1 H2 Usage affected by region ----

#testing for connection between current region of residence and the amount of usaage over ALL smart home devices
dunnTest(
  x = as.numeric(devices_combined$Usage),
  g = as.factor(devices_combined$`Current Country of Residence`),
  method = "bonferroni"
)$res


# we investigate for specific high favourability devices
dunnTest(
  x = subset(devices_combined, Device_Owned == "Smart TV")$Usage,
  g = as.factor(
    subset(devices_combined, Device_Owned == "Smart TV")$`Current Country of Residence`
  ),
  method = "bonferroni"
)$res
aggregate(as.numeric(
  subset(devices_combined, Device_Owned == "Smart TV")$Usage
), list(as.factor(
  subset(devices_combined, Device_Owned == "Smart TV")$`Current Country of Residence`
)), mean)


dunnTest(
  x = as.numeric(
    subset(devices_combined, Device_Owned == "Smart Speaker")$Usage
  ),
  g = as.factor(
    subset(devices_combined, Device_Owned == "Smart Speaker")$`Current Country of Residence`
  ),
  method = "bonferroni"
)$res

dunnTest(
  x = as.numeric(
    subset(devices_combined, Device_Owned == "Smart Lightbulb")$Usage
  ),
  g = as.factor(
    subset(devices_combined, Device_Owned == "Smart Lightbulb")$`Current Country of Residence`
  ),
  method = "bonferroni"
)$res

aggregate(as.numeric(
  subset(devices_combined, Device_Owned == "Smart Lightbulb")$Usage
), list(as.factor(
  subset(devices_combined, Device_Owned == "Smart Lightbulb")$`Current Country of Residence`
)), mean)

dunnTest(
  x = subset(
      devices_combined,
      Device_Owned != "Smart Lightbulb" & Device_Owned != "Smart Speaker" & Device_Owned != "Smart TV"
    )$Usage,
  g = as.factor(
    subset(
      devices_combined,
      Device_Owned != "Smart Lightbulb" & Device_Owned != "Smart Speaker" & Device_Owned != "Smart TV"
    )$`Current Country of Residence`
  ),
  method = "bonferroni"
)$res

aggregate(as.numeric(
  subset(devices_combined, Device_Owned != "Smart Lightbulb" & Device_Owned != "Smart Speaker" & Device_Owned != "Smart TV")$Usage
), list(as.factor(
  subset(devices_combined, Device_Owned != "Smart Lightbulb" & Device_Owned != "Smart Speaker" & Device_Owned != "Smart TV")$`Current Country of Residence`
)), mean)

#investigation into smart TV users

smartTVUsers <- subset(devices_combined, Device_Owned == "Smart TV")
epsilonSquared(x = as.numeric(smartTVUsers$Usage),
               g = smartTVUsers$`Current Country of Residence`)

sTV_UK <- subset(smartTVUsers,
                 `Current Country of Residence` == "United Kingdom")##
sTV_US <- subset(smartTVUsers,
                 `Current Country of Residence` == "United States")
sTV_DACH <- subset(smartTVUsers,
                   `Current Country of Residence` == "DACH")
summary(sTV_US)
summary(sTV_UK)
summary(sTV_DACH)


#### 2.3.1 H3 The perception towards Smart Home devices differs internationally. ----

dunnTest(ssot_new$A204_04, as.factor(ssot_new$`Current Country of Residence`), method = "bonferroni")

epsilonSquared(
  x = as.numeric(
    subset(
      ssot_new,
      `Current Country of Residence` == "DACH" |
        `Current Country of Residence` == "United Kingdom"
    )$A204_04
  ),
  g = subset(
    ssot_new,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United Kingdom"
  )$`Current Country of Residence`
)

epsilonSquared(
  x = as.numeric(
    subset(
      ssot_new,
      `Current Country of Residence` == "DACH" |
        `Current Country of Residence` == "United States"
    )$A204_04
  ),
  g = subset(
    ssot_new,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  )$`Current Country of Residence`
)


countryPerception = select(ssot_new,
                           `Current Country of Residence`,
                           A204_04)
aggregate(countryPerception[, 2],
          list(countryPerception$`Current Country of Residence`),
          mean)

countryPerception_N = select(subset(ssot_new, R101 < 1),
                             `Current Country of Residence`,
                             A204_04)
aggregate(
  countryPerception_N[, 2],
  list(countryPerception_N$`Current Country of Residence`),
  mean
)

#check users
countryPerception_U = select(subset(ssot_new, R101 > 0),
                             `Current Country of Residence`,
                             A204_04)
aggregate(
  countryPerception_U[, 2],
  list(countryPerception_U$`Current Country of Residence`),
  mean
)
# We find that germany sees smart home device security to lie more in their individual hands
# whereas the english speaking regions see it more in the manufacturers hands


# 2.3.2 H3 - we are investigating the benefits of smart home device perceived benefits and if they change by country ----

# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value

p <- c(
  kruskal_test(
    ssot_new,
    formula = A307_01 ~ `Current Country of Residence`
  )[5],
  # 0.272
  kruskal_test(
    ssot_new,
    formula = A307_02 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    ssot_new,
    formula = A307_03 ~ `Current Country of Residence`
  )[5],
  #0.508 # not stat sig
  kruskal_test(
    ssot_new,
    formula = A307_04 ~ `Current Country of Residence`
  )[5],
  #0.0268
  kruskal_test(
    ssot_new,
    formula = A307_05 ~ `Current Country of Residence`
  )[5],
  #0.125
  kruskal_test(
    ssot_new,
    formula = A307_06 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    ssot_new,
    formula = A307_07 ~ `Current Country of Residence`
  )[5],
  #0.0867
  kruskal_test(
    ssot_new,
    formula = A307_08 ~ `Current Country of Residence`
  )[5],
  #0.00615
  kruskal_test(
    ssot_new,
    formula = A307_09 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    ssot_new,
    formula = A307_10 ~ `Current Country of Residence`
  )[5]
) #0.000274
p <- unlist(p, use.names = FALSE)
p.adjust(p, method = "bonferroni", n = length(p))

# 4	A307_04	Perceived benefits: Enhancing leisure activities


dunnTest(ssot_new$A307_04, as.factor(ssot_new$`Current Country of Residence`), method = "bonferroni")
increaseLeisure = select(ssot_new,
                         `Current Country of Residence`,
                         A307_04)

aggregate(increaseLeisure[, 2],
          list(increaseLeisure$`Current Country of Residence`),
          mean)

# 6	A307_06	Perceived benefits: Providing comfort


dunnTest(ssot_new$A307_06, as.factor(ssot_new$`Current Country of Residence`), method = "bonferroni")

providingComfort = select(ssot_new,
                          `Current Country of Residence`,
                          A307_06)

aggregate(providingComfort[, 2],
          list(providingComfort$`Current Country of Residence`),
          mean)


# Pairwise testing by  country ~ increasing safety
#testing for smart home device preference

dunnTest(ssot_new$A307_07, as.factor(ssot_new$`Current Country of Residence`), method = "bonferroni")

increasingSafety = select(ssot_new,
                          `Current Country of Residence`,
                          A307_07)
aggregate(increasingSafety[, 2],
          list(increasingSafety$`Current Country of Residence`),
          mean)

# we find that increasing safety is important to US
# rank: DACH / UK / US

#---

# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value


# Pairwise testing by country 
#testing for smart home device preference country ~ providing care

dunnTest(ssot_new$A307_08, as.factor(ssot_new$`Current Country of Residence`), method = "bonferroni")
providingCare = select(ssot_new,
                       `Current Country of Residence`,
                       A307_08)
aggregate(providingCare[, 2],
          list(providingCare$`Current Country of Residence`),
          mean)

# providing care is different for DACH - US/UK
# rank DACH / US-UK (close)

# Pairwise testing by country

#starting pairwise testing per country
# Country and adding to the property value


dunnTest(ssot_new$A307_10, as.factor(ssot_new$`Current Country of Residence`), method = "bonferroni")
epsilonSquared(x = as.numeric(ssot_new$A307_10), g = ssot_new$`Current Country of Residence`)
countryIncreaseProperty = select(ssot_new,
                                 `Current Country of Residence`,
                                 A307_10)
aggregate(
  countryIncreaseProperty[, 2],
  list(countryIncreaseProperty$`Current Country of Residence`),
  mean
)




# 2.3.3 H3 - # testing for country by perceived device risk ----


# 1	E201_01	Device risk: Smart Coffee Maker
# 2	E201_02	Device risk: Smart Dishwasher
# 3	E201_03	Device risk: Smart Door Lock
# 4	E201_04	Device risk: Smart Doorbell
# 5	E201_05	Device risk: Smart Electricity Meter
# 6	E201_06	Device risk: Smart Electrical Outlet
# 7	E201_07	Device risk: Smart Fridge
# 8	E201_08	Device risk: Smart Gardening Equipment
# 9	E201_09	Device risk: Smart Heating/Cooling System
# 10	E201_10	Device risk: Smart Home Monitoring System
# 11	E201_11	Device risk: Smart Lightbulb
# 12	E201_12	Device risk: Smart Oven
# 13	E201_13	Device risk: Smart Robot
# 14	E201_14	Device risk: Smart Speaker
# 15	E201_15	Device risk: Smart Stove
# 16	E201_16	Device risk: Smart TV
# 17	E201_17	Device risk: Smart Thermostat
# 18	E201_18	Device risk: Smart Toy
# 19	E201_19	Device risk: Smart Vacuum Cleaner
# 20	E201_20	Device risk: Smart Washing Machine

p <- c((
  kruskal_test(
    ssot_new,
    formula = E201_11 ~ `Current Country of Residence`
  )[5]
),
# smart lights
kruskal_test(
  ssot_new,
  formula = E201_14 ~ `Current Country of Residence`
)[5],
# smart speaker
kruskal_test(
  ssot_new,
  formula = E201_16 ~ `Current Country of Residence`
)[5]
)# smart TV - significantly different for countries p = 0.0000555
# --- plot means by country to find out which is different and higher / lower
p.adjust(p, "bonferroni") #1.0000000 0.2760000 0.0001665


E201_SMART_TV_RISK_CCR <-
  cbind(
    dunnTest(
      ssot_new$E201_16,
      as.factor(ssot_new$`Current Country of Residence`),
      method = "bonferroni"
    )$res,
    c(
      epsilonSquared(
        x = subset(
          ssot_new,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United Kingdom"
        )$E201_16,
        g = subset(
          ssot_new,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United Kingdom"
        )$`Current Country of Residence`
      ),
      epsilonSquared(
        x = subset(
          ssot_new,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United States"
        )$E201_16,
        g = subset(
          ssot_new,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United States"
        )$`Current Country of Residence`
      ),
      "NA"
    )
  )
names(E201_SMART_TV_RISK_CCR)[5] = "Epsilon^2"
E201_SMART_TV_RISK_CCR <- E201_SMART_TV_RISK_CCR[-c(3)]

avg_smart_tv_risk = select(ssot_new,
                           E201_16,
                           `Current Country of Residence`)

aggregate(avg_smart_tv_risk[, 1],
          list(avg_smart_tv_risk$`Current Country of Residence`),
          mean)

# we find that the smart tv posed risk is significant by region
# rank UK / US /  - DACH

# 2.3.4 H3 - # testing for country by perceived device benefits ----


perceived_benefits <-
  select(
    ssot_new,
    `Current Country of Residence`,
    A307_04,
    A307_07,
    A307_08,
    A307_10
  )

A307_LATEX <-
  cbind(
    c(
      "A307_04",
      "A307_04",
      "A307_04",
      "A307_07",
      "A307_07",
      "A307_07",
      "A307_08",
      "A307_08",
      "A307_08",
      "A307_10",
      "A307_10",
      "A307_10"
    ),
    rbind(
      data.frame(
        dunnTest(
          ssot_new$A307_04,
          as.factor(ssot_new$`Current Country of Residence`),
          method = "bonferroni"
        )$res
      ),
      data.frame(
        dunnTest(
          ssot_new$A307_07,
          as.factor(ssot_new$`Current Country of Residence`),
          method = "bonferroni"
        )$res
      ),
      data.frame(
        dunnTest(
          ssot_new$A307_08,
          as.factor(ssot_new$`Current Country of Residence`),
          method = "bonferroni"
        )$res
      ),
      data.frame(
        dunnTest(
          ssot_new$A307_10,
          as.factor(ssot_new$`Current Country of Residence`),
          method = "bonferroni"
        )$res
      )
    )
  )

names(A307_LATEX)[1] <- "Code"
A307_LATEX <- A307_LATEX[-c(4)]
A307_LATEX
#smart lights actually smart home device (benefiting from smart capabilities)
CCR_Device_Smart_Benefit <-
  select(ssot_new,
         `Current Country of Residence`,
         A302_01:A302_19)
kruskal.test(
  CCR_Device_Smart_Benefit$`Current Country of Residence`,
  CCR_Device_Smart_Benefit$A302_11
)
kruskal.test(
  CCR_Device_Smart_Benefit$`Current Country of Residence`,
  CCR_Device_Smart_Benefit$A302_14
)
kruskal.test(
  CCR_Device_Smart_Benefit$`Current Country of Residence`,
  CCR_Device_Smart_Benefit$A302_16
)
table(CCR_Device_Smart_Benefit$A302_11) #light
table(CCR_Device_Smart_Benefit$A302_14) #speaker
table(CCR_Device_Smart_Benefit$A302_16) #TV

# 2.3.5 - Disabled features by country 

disabled_features_country <-
  select(
    ssot_new,
    participant_id,
    R507,
    R510,
    R513,
    `Current Country of Residence`
  )
disabled_features_country$choice <-
  ifelse(
    disabled_features_country$R507 == 1 |
      disabled_features_country$R510 == 1 |
      disabled_features_country$R513 == 1,
    1,
    0
  )

chisq.test(disabled_features_country$`Current Country of Residence`,
           disabled_features_country$choice)

# 3.0.0 - Household Factors ----

# 3.0.1 - Discussion Data####
#p Discussion
# the below test can only be done before splitting into >1 
cor.test(ssot_new$R101, as.numeric(ssot_new$A005)) # greater hh size higher variance of devices
cor.test(ssot_new$R101, as.numeric(ssot_new$A004)) 
wilcox.test(ssot_new$R101,as.numeric(ssot_new$A004))

#Discussion
wilcox_test(ssot_new, formula = E201_11 ~ A005) # smart home smart speaker smart tv risk assessment
wilcox_test(ssot_new, formula = E201_14 ~ A005) # household size
wilcox_test(ssot_new, formula = E201_16 ~ A005)

wilcox_test(ssot_new, R101 ~ A004) 


device_interaction <- select(ssot_new, R534_01:R534_06,R536_01:R536_06,R538_01:R538_06)
i1 <- select(ssot_new, R534_01:R534_06)
i2 <- select(ssot_new, R536_01:R536_06)
i3 <- select(ssot_new, R538_01:R538_06)

i1 <- unname(select(ssot_new, R534_01:R534_06))
i2 <- unname(select(ssot_new, R536_01:R536_06))
i3 <- unname(select(ssot_new, R538_01:R538_06))

colnames(i1) = c("Voice Assistant", "App on my phone", "Physical buttons on the device", "Screen on the device", "Internet based service connected to the device", "Home Internet router")
colnames(i2) = c("Voice Assistant", "App on my phone", "Physical buttons on the device", "Screen on the device", "Internet based service connected to the device", "Home Internet router")
colnames(i3) = c("Voice Assistant", "App on my phone", "Physical buttons on the device", "Screen on the device", "Internet based service connected to the device", "Home Internet router")

device_interaction <- rbind(i1,i2,i3)

# removing people that neither rented or owned a home 
rent_own_test = subset(select(ssot_new, A007, R101),A007 != 3)
wilcox.test(rent_own_test$R101 ~ as.factor(rent_own_test$A007)) # no statistical significance found

kruskal_test(rent_own_test, formula = R101 ~ A007) 

# 3.1.0 - Household size Device adoption ####


children_in_household <- subset(ssot_new, A004>0)
cor.test(children_in_household$R101, as.numeric(children_in_household$A005))

no_children_in_household <- subset(ssot_new, A004==0)
cor.test(no_children_in_household$R101, as.numeric(no_children_in_household$A005))

## 3.2.0 H2 - Household Size ~ Daily Usage of devices - R501, R503, R505 ----
# creating table usage device ownership

d1 <- select(subset(ssot_new, R233_01 == 1), participant_id, R232_01, R501, A005)
d2 <- select(subset(ssot_new, R233_02 == 1), participant_id, R232_02, R503, A005)
d3 <- select(subset(ssot_new, R233_03 == 1), participant_id, R232_03, R505, A005)
colnames(d1) <- c("participant_id", "Device_Owned", "Usage", "A005")
colnames(d2) <- c("participant_id", "Device_Owned", "Usage", "A005")
colnames(d3) <- c("participant_id", "Device_Owned", "Usage", "A005")
d <- rbind(d1, d2, d3)
d <- subset(d, Usage != 7)

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

#H2 - Household size ~ Device Location - R528, R530, R532 
#//low priority, no interesting findings expected


deviceLocation <-
  select(
    ssot_new,
    R528_01:R528_12,
    R529_01:R529_12,
    R530_01:R530_12,
    participant_id,
    A004
  )
#deviceLocationSmartSpeaker <- merge(deviceLocation,dSmartSpeaker, by="participant_id")

# 3.3.0 Device Perception x A005 ----

asd = select(ssot_new, A005,A204_01,A204_02,A204_03,A204_04,A204_05,A204_06)
cor.test(as.numeric(asd$A005),as.numeric(asd$A204_01))
cor.test(as.numeric(asd$A005),as.numeric(asd$A204_02))
cor.test(as.numeric(asd$A005),as.numeric(asd$A204_03))
cor.test(as.numeric(asd$A005),as.numeric(asd$A204_04))
cor.test(as.numeric(asd$A005),as.numeric(asd$A204_05))
cor.test(as.numeric(asd$A005),as.numeric(asd$A204_06))


plot(as.numeric(asd$A005),as.numeric(asd$A204_01))
abline(lm(as.numeric(d$A005)~as.numeric(d$Usage)), col="red") # regression line (y~x)


#H1 - Household size ~ Disabled features - R507, 510, R513
##/no little disabled features (n=37)-> low priority


#H2####
##testing wilcox test for children > 0 impact on amount of devices##

#usage factoring into 
# Adding all device owners to the same data frame and stacking them for analysis
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
    A004,
    A005,
  )

d1 <-
  select(subset(u, R233_01 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_01,
         R501,
         A004,
         A005,)
d2 <-
  select(subset(u, R233_02 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_02,
         R503,
         A004,
         A005,)
d3 <-
  select(subset(u, R233_03 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_03,
         R505,
         A004,
         A005,)
colnames(d1) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "Children", "Household")
colnames(d2) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "Children", "Household")
colnames(d3) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "Children", "Household")
d <- rbind(d1, d2, d3)
d <- subset(d, Usage != 7)


mean(as.numeric(subset(d,Children==1)$Usage))
mean(as.numeric(subset(d,Children==0)$Usage))

median(as.numeric(subset(d,Children==1)$Usage))
median(as.numeric(subset(d,Children==0)$Usage))


wilcox.test(as.numeric(d$Usage)~d$Children)

children_smartTV = subset(d, Device == "Smart TV")
wilcox.test(as.numeric(children_smartTV$Usage)~children_smartTV$Children)
mean(as.numeric(subset(children_smartTV,Children==1)$Usage))
mean(as.numeric(subset(children_smartTV,Children==0)$Usage))

median(as.numeric(subset(children_smartTV,Children==1)$Usage))
median(as.numeric(subset(children_smartTV,Children==0)$Usage))
#--
children_smartTV = subset(d, Device == "Smart Speaker")
wilcox.test(as.numeric(children_smartTV$Usage)~children_smartTV$Children)
mean(as.numeric(subset(children_smartTV,Children==1)$Usage))
mean(as.numeric(subset(children_smartTV,Children==0)$Usage))

median(as.numeric(subset(children_smartTV,Children==1)$Usage))
median(as.numeric(subset(children_smartTV,Children==0)$Usage))
#--
children_smartTV = subset(d, Device == "Smart Lightbulb")
wilcox.test(as.numeric(children_smartTV$Usage)~children_smartTV$Children)
mean(as.numeric(subset(children_smartTV,Children==1)$Usage))
mean(as.numeric(subset(children_smartTV,Children==0)$Usage))

median(as.numeric(subset(children_smartTV,Children==1)$Usage))
median(as.numeric(subset(children_smartTV,Children==0)$Usage))
#--

#dunnTest(ssot_new$A004, as.factor(select(ssot_new,E201_01:E201_20)), method = "bonferroni")

riskChildren_LATEX <- data.frame(
  "Usage_type" = c("Smart Lightbulb",
                   "Smart Speaker",
                   "Smart TV"
  ), 
  "p_value"= c(wilcox_test(ssot_new, E201_11 ~ A004)$p,
               wilcox_test(ssot_new, E201_14 ~ A004)$p,
               wilcox_test(ssot_new, E201_16 ~ A004)$p
  ), 
  "effect_size" = c(wilcox_effsize(ssot_new, formula = E201_11 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = E201_14 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = E201_16 ~ A004)$effsize
  ))



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
  "p_value"= c(wilcox_test(ssot_new, A204_01 ~ A004)$p,
               wilcox_test(ssot_new, A204_02 ~ A004)$p,
               wilcox_test(ssot_new, A204_03 ~ A004)$p,
               wilcox_test(ssot_new, A204_04 ~ A004)$p,
               wilcox_test(ssot_new, A204_05 ~ A004)$p,
               wilcox_test(ssot_new, A204_06 ~ A004)$p
  ), 
  "effect_size" = c(wilcox_effsize(ssot_new, formula = A204_01 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = A204_02 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = A204_03 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = A204_04 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = A204_05 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = A204_06 ~ A004)$effsize
  ))

# 3.3.3 - testing for children affecting the type of usage the user is comfortable with ---- 

# 1 E205_01	Usage type: Voice commands via a Smart Speaker
# 2	E205_02	Usage type: Voice commands via a Smartphone Voice Assistant
# 3	E205_03	Usage type: Smartphone App for the Device
# 4	E205_04	Usage type: Smartphone Widgets or Shortcuts
# 5	E205_05	Usage type: Sensors inside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 6	E205_06	Usage type: Sensors outside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 7	E205_07	Usage type: Automatic Operation based on Device Programming

aggregate(ssot_new$E205_01,
          list(ssot_new$A004),
          mean)
aggregate(ssot_new$E205_02,
          list(ssot_new$A004),
          mean)


data.frame(
  "Usage_type" = c("Voice commands via a Smart Speaker",
                   "Voice commands via a Smartphone Voice Assistant"
  ), 
  "p_value"= c(wilcox_test(ssot_new, E205_01 ~ A004)$p,
               wilcox_test(ssot_new, E205_02 ~ A004)$p
  ), 
  "effect_size" = c(wilcox_effsize(ssot_new, formula = E205_01 ~ A004)$effsize,
                    wilcox_effsize(ssot_new, formula = E205_02 ~ A004)$effsize
  ))

# 3.3.3 - testing for children affecting the R101 ---- 
t.test(ssot_new$R101~ssot_new$A004, var.equal = TRUE, alternative = "two.sided")


# 4.0 Discussion ==== 



