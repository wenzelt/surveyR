list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(readxl)


singleSourceOfTruthAppended <-
  read_xlsx("singleSourceOfTruthAppended_P.xlsx")
singleSourceOfTruthAppended <-
  subset(singleSourceOfTruthAppended,
         `Current Country of Residence` != "NA")

table(singleSourceOfTruthAppended$Sex)
table(singleSourceOfTruthAppended$age)
table(singleSourceOfTruthAppended$A003)#edu
table(singleSourceOfTruthAppended$A006)
range(rowMeans(select(singleSourceOfTruthAppended, S101_01:S101_12)))
range(singleSourceOfTruthAppended$sebis_avg)
range(singleSourceOfTruthAppended$sebis_DeviceSecurement_avg)
range(singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg)
range(singleSourceOfTruthAppended$sebis_UpdatingBehaviour_avg)
