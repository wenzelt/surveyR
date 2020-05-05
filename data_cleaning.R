##Data Cleaning##

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