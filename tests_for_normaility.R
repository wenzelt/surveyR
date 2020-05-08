list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)


load.Rdata( filename="SSOT.Rdata", "singleSourceOfTruthAppended" ) ###this loads the ssot from the root dir directly as a data frame 
nums = dplyr::select_if(singleSourceOfTruthAppended, is.numeric)

df.shapiro <- apply(nums,2 , shapiro.test)
df2 <- mutate_all(nums, function(x) as.numeric(as.character(x)))

nums <- dplyr::select_if(singleSourceOfTruthAppended, is.numeric)
nums <- select(nums, -c(R233_01,R233_02,R233_03, S101_13))
nums <- select(nums, R101:S102_09)

normality <- do.call(rbind, lapply(nums, function(x) shapiro.test(x)[c("statistic", "p.value")]))

