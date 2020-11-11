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

singleSourceOfTruthAppended <-
  read_xlsx("Excels/singleSourceOfTruthAppended_P.xlsx")
singleSourceOfTruthAppended <-
  subset(singleSourceOfTruthAppended,
         `Current Country of Residence` != "NA")

#load index of questions into dataframe 
Code_Answer_transpose <- read_delim("Excels/Code_Answer_transpose.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)

#corrections in the code 
singleSourceOfTruthAppended$A004 <-ifelse(singleSourceOfTruthAppended$A004 > 0, 1, 0)

# adding avg of legislation 
singleSourceOfTruthAppended$LA_Mean <-rowMeans(select(singleSourceOfTruthAppended,LA01_01:LA01_03))
singleSourceOfTruthAppended$LA02_Mean <-rowMeans(select(singleSourceOfTruthAppended,LA02_01:LA02_03))

singleSourceOfTruthAppended <- subset(singleSourceOfTruthAppended,)

# subsetting into different countries 
#Participants_DACH <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "DACH")
#Participants_US <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United States")
#Participants_UK <- subset(singleSourceOfTruthAppended, `Current Country of Residence` == "United Kingdom")



