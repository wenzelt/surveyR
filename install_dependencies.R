#this script installs the necessary dependencies and loads our dataset reliably.

list.of.packages <-
  c("ggplot2", "Hmisc","tidyverse","summarytools","stringi","sjPlot","data.table","plyr","psych","lattice","multcompView", "dplyr", "ggpubr","QCA", "FSA", "dunn.test","rcompanion")
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



setwd("~/Documents/surveyR")

singleSourceOfTruthAppended <- read_delim("SSOT.csv", ";", escape_double = FALSE,  na = "NA", trim_ws = TRUE)[-1]

#load index of questions into dataframe 
Code_Answer_transpose <- read_delim("Excels/Code_Answer_transpose.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)


singleSourceOfTruthAppended$A004 <-ifelse(singleSourceOfTruthAppended$A004 > 0, 1, 0) # having children 

# adding avg of legislation 
singleSourceOfTruthAppended$LA_Mean <-rowMeans(select(singleSourceOfTruthAppended,LA01_01:LA01_03))
singleSourceOfTruthAppended$LA02_Mean <-rowMeans(select(singleSourceOfTruthAppended,LA02_01:LA02_03))

# adding Sebis/MUIPC Averages to dataset
singleSourceOfTruthAppended$sebis_avg <- rowMeans(select(singleSourceOfTruthAppended, S101_01:S101_12))
singleSourceOfTruthAppended$sebis_DeviceSecurement_avg <- rowMeans(select(singleSourceOfTruthAppended, S101_01:S101_04))
singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg <- rowMeans(select(singleSourceOfTruthAppended, S101_05:S101_09))
singleSourceOfTruthAppended$sebis_UpdatingBehaviour_avg <- rowMeans(select(singleSourceOfTruthAppended, S101_10:S101_12))

# subsetting into different countries 
Participants_DACH <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "DACH")
Participants_US <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United States")
Participants_UK <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United Kingdom")
