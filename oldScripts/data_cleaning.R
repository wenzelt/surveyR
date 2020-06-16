smartDeviceNames = c(
  "Smart Coffee Maker",
  "Smart Dishwasher",
  "Smart Door Lock",
  "Smart Doorbell",
  "Smart Electricity Meter",
  "Smart Electrical Outlet",
  "Smart Fridge",
  "Smart Gardening Equipment",
  "Smart Heating/Cooling System",
  "Smart Home Monitoring System",
  "Smart Lightbulb",
  "Smart Oven",
  "Smart Robot",
  "Smart Speaker",
  "Smart Stove",
  "Smart TV",
  "Smart Thermostat",
  "Smart Toy",
  "Smart Vacuum Cleaner",
  "Smart Washing Machine"
)

##Data Cleaning##
library(readr)
singleSourceOfTruthAppended <- read_csv("singleSourceOfTruthAppended.csv",stringsAsFactors = True)
list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr", "tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

load(file = "likertScaleCheck.Rdata")
threshold_lower = median(singleSourceOfTruthAppended$time_taken) - 2*sd(singleSourceOfTruthAppended$TIME_SUM)
threshold_upper = median(singleSourceOfTruthAppended$time_taken) + 2*sd(singleSourceOfTruthAppended$TIME_SUM)##change to median
#ssot_filtered_sosci = subset(singleSourceOfTruthAppended, TIME_SUM < threshold_upper & TIME_SUM >threshold_lower)
ssot_filtered_sosci = subset(singleSourceOfTruthAppended, TIME_SUM >threshold_lower)


ssot_merged_likertCheck <- merge(x = ssot_filtered_sosci, y = select(likertScaleCheck, participant_id, var_flag), by = "participant_id" , all.x = TRUE)
ssot_clean = subset(ssot_merged_likertCheck, var_flag == 0)
ssot_flagged = subset(ssot_merged_likertCheck, var_flag == 1)

description = describe(nums)

devices <- select(singleSourceOfTruthAppended, participant_id, R101_01:R101_20)
colnames(devices)[2:21] <- smartDeviceNames
devices_long <- gather(devices, device_code, truthful, -participant_id)
participant_devices <- subset(devices_long, truthful == TRUE)
