list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)


smartTvAnswers <- subset(singleSourceOfTruthAppended, R232_01 == "Smart TV" | R232_02 == "Smart TV"| R232_03 == "Smart TV")
smartSpeakerAnswers <- subset(singleSourceOfTruthAppended, R232_01 == "Smart Speaker"| R232_01 == "Smart Lautsprecher"| R232_02 == "Smart Speaker"| R232_02 == "Smart Lautsprecher"| R232_03 == "Smart Speaker"| R232_03 == "Smart Lautsprecher")
smartLightAnswers <- subset(singleSourceOfTruthAppended, R232_01 == "Smart Lightbulb" | R232_02 == "Smart Lightbulb"| R232_03 == "Smart Lightbulb" | R232_01 == "Smart GlÃ¼hbirne" | R232_02 == "Smart GlÃ¼hbirne"| R232_03 == "Smart GlÃ¼hbirne")
