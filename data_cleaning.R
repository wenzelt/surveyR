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
orange <- merge(x = ssot_filtered_sosci, y = likertScaleCheck, by = "participant_id" , all.x = TRUE)

threshold_lower = mean(singleSourceOfTruthAppended$time_taken) - 2*sd(singleSourceOfTruthAppended$time_taken)
threshold_upper = mean(singleSourceOfTruthAppended$time_taken) + 2*sd(singleSourceOfTruthAppended$time_taken)
ssot_filtered = subset(singleSourceOfTruthAppended, time_taken < threshold_upper & time_taken >threshold_lower)

threshold_lower = mean(singleSourceOfTruthAppended$time_taken) - 2*sd(singleSourceOfTruthAppended$TIME_SUM)
threshold_upper = mean(singleSourceOfTruthAppended$time_taken) + 2*sd(singleSourceOfTruthAppended$TIME_SUM)
ssot_filtered_sosci = subset(singleSourceOfTruthAppended, TIME_SUM < threshold_upper & TIME_SUM >threshold_lower)


ssot_Corrected_LA <- cleanSD(LA01_01)

description = describe(nums)

devices <- select(singleSourceOfTruthAppended, participant_id, R101_01:R101_20)
colnames(devices)[2:21] <- smartDeviceNames
devices_long <- gather(devices, device_code, truthful, -participant_id)
participant_devices <- subset(devices_long, truthful == TRUE)
