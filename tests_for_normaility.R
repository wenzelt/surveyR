list.of.packages <- c("ggplot2", "tidyverse", "dplyr", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)


load.Rdata( filename="SSOT.Rdata", "singleSourceOfTruthAppended" ) ###this loads the ssot from the root dir directly as a data frame 
nums = dplyr::select_if(singleSourceOfTruthAppended, is.numeric)

f <- function(x) {
  if (diff(range(x)) == 0) list() else shapiro.test(x)
}

saveqqplot <- function(column){
  ggqqplot(column, title = colnames(column)[0])
}
apply(nums, MARGIN = 2, saveqqplot)

df.shapiro <- apply(nums,2 , shapiro.test)

do.call(rbind, lapply(df,shapiro.test(nums)[c("statistic", "p.value")]))
