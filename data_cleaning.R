##Data Cleaning##
library(readr)
singleSourceOfTruthAppended <- read_csv("singleSourceOfTruthAppended.csv",stringsAsFactors = True)
list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

threshold_lower = mean(time_taken) - 2*sd(time_taken)
threshold_upper = mean(time_taken) + 2*sd(time_taken)
ssot_filtered = subset(singleSourceOfTruthAppended, time_taken < threshold_upper & time_taken >threshold_lower)
ssot_filtered_sosci = subset(singleSourceOfTruthAppended, TIME_SUM < threshold_upper & TIME_SUM >threshold_lower)
threshold_upper = mean(LA01_01)

ssot_Corrected_LA <- cleanSD(LA01_01)

