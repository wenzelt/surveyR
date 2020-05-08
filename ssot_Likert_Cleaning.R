
list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

likertScaleCheck <- select(singleSourceOfTruthAppended,participant_id, E201_01:E201_20,A305_01:A305_09,S101_01:S101_12,S102_01:S102_09)

likertScaleCheck$E2avg <- rowMeans(likertScaleCheck$E201_01:E201_20)
