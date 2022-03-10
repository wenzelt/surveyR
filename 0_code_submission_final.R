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
        by.y = "A008_01",)

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

# reverse one item in Sebis proactive awareness
SSOT <- mutate(SSOT, comparison = 7 - S101_05)
SSOT <- mutate(SSOT, comparison = 7 - S101_06)
SSOT <- mutate(SSOT, comparison = 7 - S101_07)
SSOT <- mutate(SSOT, comparison = 7 - S101_09)


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
  rowMeans(select(ssot_new, LA01_01, LA01_02, LA01_03))
ssot_new$LA02_Mean <-
  rowMeans(select(ssot_new, LA02_01, LA02_01, LA02_03))

# appending Sebis/MUIPC Averages to dataset
ssot_new$sebis_avg <-
  rowMeans(
    select(
      ssot_new,
      S101_01,
      S101_02,
      S101_03,
      S101_04,
      S101_05,
      S101_06,
      S101_07,
      S101_08,
      S101_09,
      S101_10,
      S101_11,
      S101_12
    )
  )
ssot_new$sebis_DeviceSecurement_avg <-
  rowMeans(select(ssot_new, S101_01, S101_02, S101_03, S101_04))
ssot_new$sebis_ProactiveAwareness_avg <-
  rowMeans(select(ssot_new, S101_05, S101_06, S101_07, S101_08, S101_09))
ssot_new$sebis_UpdatingBehaviour_avg <-
  rowMeans(select(ssot_new, S101_10, S101_11, S101_12))

ssot_new$muipc_avg <-
  rowMeans(
    select(
      ssot_new,
      S102_01,
      S102_02,
      S102_03,
      S102_04,
      S102_05,
      S102_06,
      S102_07,
      S102_08,
      S102_09
    )
  )
ssot_new$muipc_PersonalInfo_avg <-
  rowMeans(select(ssot_new, S102_01, S102_02, S102_03))
ssot_new$muipc_PerceivedSur_avg <-
  rowMeans(select(ssot_new, S102_04, S102_05, S102_06))
ssot_new$muipc_PerceivedIntrusion_avg <-
  rowMeans(select(ssot_new, S102_07, S102_08, S102_09))

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



# 0.4 function definitions ====

# 0.4.1 Definition ANOVA function Countries -----
country_anova = select(
  ssot_new,
  `Current Country of Residence`,
  sebis_avg,
  R101,
  A204_01:A204_06,
  E201_01:E201_20,
  A307_01:A307_10,
)

titles <- hash()

calc_anova <- function(data, column_to_use) {
  if (!is.null(data[[column_to_use]])) {
    data <- data[data[[column_to_use]] >= 0, ]
  } else{
    print("Column does not Exist")
  }
  
  # pdf(sprintf("Plot_'%s'by_region.pdf", titles[[column_to_use]]))
  means <-
    round(tapply(as.numeric(data[[column_to_use]]),
                 data$`Current Country of Residence`,
                 mean),
          digits = 2)
  
  
  plotmeans(
    data[[column_to_use]] ~ data$`Current Country of Residence`,
    digits = 2,
    ccol = 'red',
    mean.labels = F,
    ylab = 'mean',
    main = sprintf("Plot of '%s' means by region", titles[[column_to_use]]),
  )
  
  boxplot(
    data[[column_to_use]] ~ data$`Current Country of Residence`,
    main = sprintf("Plot of '%s' means by region", titles[[column_to_use]]),
    xlab = "'region'",
    ylab = titles[[column_to_use]],
    col = rainbow(7)
  )
  
  
  
  # F statistics = Variation among sample means / Variation within groups
  
  aov_content =  aov(data[[column_to_use]] ~ data$`Current Country of Residence`)
  if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
    tuk = TukeyHSD(aov_content)
    
    print(summary(aov_content))
    print(tuk)
    plot(tuk)
    print(means)
    
    # dev.off()
    #return(tuk)
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

# 0.5.1 - creating dataset of personally owned devices by participants ----
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

# 0.5.2 - creating dataset of ANOVA household ----

house_anova = select(ssot_new, A007, R101, A004)

# 0.5.3 - creating dataset of ANOVA country ----
country_anova = select(
  ssot_new,
  `Current Country of Residence`,
  sebis_avg,
  R101,
  A204_01:A204_06,
  E201_01:E201_20,
  A307_01:A307_10,
)


# 0.5.4 - creating dataset of usage Household ----

# Adding all device owners to the same data frame and stacking them for analysis
usage_household <-
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

temp1 <-
  select(
    subset(usage_household, R233_01 == 1),
    participant_id,
    `Current Country of Residence`,
    R232_01,
    R501,
    A004,
    A005,
  )
temp2 <-
  select(
    subset(usage_household, R233_02 == 1),
    participant_id,
    `Current Country of Residence`,
    R232_02,
    R503,
    A004,
    A005,
  )
temp3 <-
  select(
    subset(usage_household, R233_03 == 1),
    participant_id,
    `Current Country of Residence`,
    R232_03,
    R505,
    A004,
    A005,
  )
colnames(temp1) <-
  c(
    "participant_id",
    "Current Country of Residence",
    "Device",
    "Usage",
    "Children",
    "Household"
  )
colnames(temp2) <-
  c(
    "participant_id",
    "Current Country of Residence",
    "Device",
    "Usage",
    "Children",
    "Household"
  )
colnames(temp3) <-
  c(
    "participant_id",
    "Current Country of Residence",
    "Device",
    "Usage",
    "Children",
    "Household"
  )
temp <- rbind(temp1, temp2, temp3)
temp_usage <- subset(temp, Usage != 7)

##### 1.0 Findings ----

# - D3 - Findings - Interesting device users demographics.

demographic_data <-
  select(ssot_new,
         A002,
         age, participant_id)

device_owners <-
  devices_combined %>% distinct(participant_id, .keep_all = TRUE)
demographic_data <-
  merge(x = demographic_data, y = device_owners, by = "participant_id")


names(demographic_data)[names(demographic_data) == "Current Country of Residence"] <-
  "country"
names(demographic_data)[names(demographic_data) == "A002"] <-
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


#### F1 H3_Perception - Perceived Surveillance MUIPC----

# perception - how does perceived legislative protection influence perceived surveillance
cor_test(select(ssot_new, LA_Mean, muipc_PerceivedSur_avg))
# strong negative correlation for non device users.
#### F2 H3_Perception - Perceived Surveillance MUIPC----

cor_test(select(subset(ssot_new, R101 < 1),
                LA_Mean,
                muipc_PerceivedSur_avg))
#### F3 H3_Perception - Perceived Surveillance MUIPC----

cor_test(select(subset(ssot_new, R101 > 0),
                LA_Mean,
                muipc_PerceivedSur_avg))

# F4 manufacturer responsibility for protecting privacy and security ----
cor_test(select(ssot_new, A204_04, muipc_PerceivedSur_avg))

# F5 - perceived regulatory protection} and the \textsl{number of unique types of devices} ====
cor.test(ssot_new$LA01_01,
         ssot_new$R101) #* unwanted access by third parties.
cor.test(ssot_new$LA01_03,
         ssot_new$R101) #* unwanted processing and analysis by third parties.
cor.test(ssot_new$LA_Mean,
         ssot_new$R101) #* overall correlation construct average

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

titles$A204_04 = "Keeping the Smart Home device secure"
titles$A204_05 = "Fixing a hardware failure"


calc_anova(country_anova, 'A204_04')
calc_anova(country_anova, 'A204_05')

no_users = subset(country_anova, R101 == 0)
calc_anova(no_users, 'A204_04')


# F 8 - Perception Devices Adoption by country and legislative protection ----
cor.test(Participants_DACH$LA_Mean, Participants_DACH$R101)
cor.test(Participants_UK$LA_Mean, Participants_UK$R101)
cor.test(Participants_US$LA_Mean, Participants_US$R101)

# F 9 - Usage of smart TVs across regions ====
dunnTest(
  x = subset(devices_combined, Device_Owned == "Smart TV")$Usage,
  g = as.factor(
    subset(devices_combined, Device_Owned == "Smart TV")$`Current Country of Residence`
  ),
  method = "bonferroni"
)$res

# F 10 - Usage of smart TVs across regions ====
calc_anova_house(select(house_anova, A007, R101))

# F 11 - Amount of types of smart home devices and household size  ====
cor.test(ssot_new$R101, as.numeric(ssot_new$A005)) # greater hh size higher variance of devices

# F12 - Children no children t test
t.test(ssot_new$R101 ~ ssot_new$A004,
       var.equal = TRUE,
       alternative = "two.sided")


# F13 - children impact usage ----
wilcox.test(as.numeric(temp_usage$Usage) ~ temp_usage$Children)

mean(as.numeric(subset(d, Children == 1)$Usage))
mean(as.numeric(subset(d, Children == 0)$Usage))

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
    wilcox_test(ssot_new, E205_01 ~ A004)$p,
    wilcox_test(ssot_new, E205_02 ~ A004)$p
  ),
  "effect_size" = c(
    wilcox_effsize(ssot_new, formula = E205_01 ~ A004)$effsize,
    wilcox_effsize(ssot_new, formula = E205_02 ~ A004)$effsize
  )
)


# F16 - mean users non users LA_MEAN ----
mean(subset(ssot_new, R101 == 0)$LA_Mean)
mean(subset(ssot_new, R101 > 0)$LA_Mean)

# F17 - wilcox test MUIPC ----

no_devices <-
  subset(
    select(
      ssot_new,
      LA_Mean,
      LA02_Mean,
      sebis_avg,
      R101,
      A204_01:A204_06,
      muipc_avg,
      muipc_PerceivedSur_avg
    ),
    R101 == 0
  )
devices <-
  subset(
    select(
      ssot_new,
      LA_Mean,
      LA02_Mean,
      sebis_avg,
      R101,
      A204_01:A204_06,
      muipc_avg,
      muipc_PerceivedSur_avg
    ),
    R101 > 0
  )
devices$R101 = 1

devices_test <- rbind(no_devices, devices)

wilcox.test((no_devices$muipc_avg), devices$muipc_avg)
mean(no_devices$muipc_avg)
mean(devices$muipc_avg)

# F18 - No integration selections -----

no_integration = select(ssot_new, E203_01,E203_02,E203_03,E203_04,E203_05,E203_06,E203_07)

prop.table(table(no_integration$E203_01))
prop.table(table(no_integration$E203_02))
prop.table(table(no_integration$E203_03))
prop.table(table(no_integration$E203_06))

##### 9.0 -  Tables ----

# T1 - overview demographics - table1 ----

demographic_data_t1 <-
  select(ssot_new,
         `Current Country of Residence`,
         A002,
         age)
names(demographic_data_t1)[names(demographic_data_t1) == "Current Country of Residence"] <-
  "country"
names(demographic_data_t1)[names(demographic_data_t1) == "A002"] <-
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

gs = subset(demographic_data_t1, country == "DACH")
uk = subset(demographic_data_t1, country == "United Kingdom")
us = subset(demographic_data_t1, country == "United States")

count(gs$gender)
count(uk$gender)
count(us$gender)





# T2 - Demographic by device - table 2 ====
device_information_region = select(ssot_new, R101, `Current Country of Residence`)
counts <-
  ddply(
    device_information_region,
    .(
      device_information_region$R101,
      device_information_region$`Current Country of Residence`
    ),
    nrow
  )
table2 <-
  data.table(device_information_region) # transpose to data.table
table2 = table2[, list(Freq = .N), by = list(R101, `Current Country of Residence`)] # use list to name var directly


# T3 -  H3_Perception - How is perception affected by the feeling of legislative protection ? (table 3 ) ----

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
         E201_16, )
), var1, var2, cor, p),
var1 == "LA_Mean" & var2 != "LA_Mean")

cor.test(ssot_new$LA01_01, risk_all_devices, adjust.method = "Bonferroni")
subset(select(cor_test(
  select(ssot_new,
         LA01_01,
         E201_11,
         E201_14,
         E201_16, )
), var1, var2, cor, p),
var1 == "LA01_01" & var2 != "LA01_01")


cor.test(ssot_new$LA01_02, risk_all_devices, adjust.method = "Bonferroni")
subset(select(cor_test(
  select(ssot_new,
         LA01_02,
         E201_11,
         E201_14,
         E201_16, )
), var1, var2, cor, p),
var1 == "LA01_02" & var2 != "LA01_02")


cor.test(ssot_new$LA01_03, risk_all_devices, adjust.method = "Bonferroni")
subset(select(cor_test(
  select(ssot_new,
         LA01_03,
         E201_11,
         E201_14,
         E201_16, )
), var1, var2, cor, p),
var1 == "LA01_03" & var2 != "LA01_03")

# T4 - perceived benefits of smart home devices across countries ANOVA
#A307 Perceived Feature benefit ----

titles$A307_01 = "Saving money"
titles$A307_02 = "Saving energy"
titles$A307_03 = "Increasing convenience"
titles$A307_04 = "Enhancing leisure activities"
titles$A307_05 = "Providing peace of mind"
titles$A307_06 = "Providing comfort"
titles$A307_07 = "Increasing safety"
titles$A307_08 = "Providing care"
titles$A307_09 = "Improving quality of life"
titles$A307_10 = "Increasing property value"

calc_anova(country_anova, 'A307_04')
calc_anova(country_anova, 'A307_05')
calc_anova(country_anova, 'A307_06')
calc_anova(country_anova, 'A307_07')
calc_anova(country_anova, 'A307_08')
calc_anova(country_anova, 'A307_10')


calc_anova(country_anova, 'R101')
