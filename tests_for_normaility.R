list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr","dlookr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(dlookr)


load.Rdata( filename="SSOT.Rdata", "singleSourceOfTruthAppended" ) ###this loads the ssot from the root dir directly as a data frame 
nums = dplyr::select_if(singleSourceOfTruthAppended, is.numeric)

df.shapiro <- apply(nums,2 , shapiro.test)
df2 <- mutate_all(nums, function(x) as.numeric(as.character(x)))

nums <- dplyr::select_if(singleSourceOfTruthAppended, is.numeric) ## load all numeric values we have in the dataset
nums <- select(nums, -c(R222, R223,R233_01,R233_02,R233_03, S101_13)) # drop all columns that have identical values 
nums <- select(nums, R101:S102_09)

lshap <- lapply(nums, shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value"))
t(lres)

shapiro <- do.call(rbind, lapply(nums, function(x) shapiro.test(x)[c("statistic", "p.value")]))

normality <- normality(nums)

corrected = subset(select(likertScaleCheck, E201_01:var_flag),  var_flag != 1)
orange <- normality(select(corrected, -c(var_flag,S101_13)))
