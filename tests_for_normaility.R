list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)


load.Rdata( filename="SSOT.Rdata", "singleSourceOfTruthAppended" ) ###this loads the ssot from the root dir directly as a data frame 

