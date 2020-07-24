#this script installs the necessary dependencies and loads our dataset reliably.

list.of.packages <-
  c("ggplot2", "tidyverse","psych","lattice","multcompView", "dplyr", "ggpubr","QCA", "FSA", "dunn.test","rcompanion")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(rcompanion)
library(FSA)
library(dunn.test)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(readxl)
library(psych)
library(QCA)


setwd("~/Documents/surveyR")

singleSourceOfTruthAppended <-
  read_xlsx("Excels/singleSourceOfTruthAppended_P.xlsx")
singleSourceOfTruthAppended <-
  subset(singleSourceOfTruthAppended,
         `Current Country of Residence` != "NA")

singleSourceOfTruthAppended$A004 <-
  cut(singleSourceOfTruthAppended$A004, breaks = c(0, 1, Inf)) ## adding levels to children
singleSourceOfTruthAppended$A004 <-
  as.factor(singleSourceOfTruthAppended$A004)

Participants_DACH <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "DACH")
Participants_US <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United States")
Participants_UK <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United Kingdom")


