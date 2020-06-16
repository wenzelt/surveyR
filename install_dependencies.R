#this script installs the necessary dependencies and loads our dataset reliably.

list.of.packages <-
  c("ggplot2", "tidyverse","psych","lattice","multcompView", "dplyr", "ggpubr", "FSA", "dunn.test","rcompanion")
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


setwd("~/surveyR")

singleSourceOfTruthAppended <-
  read_xlsx("Excels/singleSourceOfTruthAppended_P.xlsx")
singleSourceOfTruthAppended <-
  subset(singleSourceOfTruthAppended,
         `Current Country of Residence` != "NA")

attach(singleSourceOfTruthAppended)