install.packages("Hmisc") # Only run on first use
install.packages("tidyverse") # Only run on first use
library(tidyverse)
library(Hmisc)
rawdata <- select(SSoTAppended, LA01_01:LA01_03, E205_01:E205_07)
ccs <- as.matrix(rawdata)
rcorr(ccs, type="pearson") # You can also use "spearman"


rawdata <- select(SSoTAppended,E203_01:E203_07,LA01_01:LA01_03,LA02_01:LA02_03)
rawdata <- select(SSoTAppended,E203,LA01_01:LA01_03,LA02_01:LA02_03)

rawdata <- select(ssotNon,E203_01:E203_07,LA01_01:LA01_03,LA02_01:LA02_03)
ssotNon <- subset(SSoTAppended, user_type == 'nonUsers')

rawdata <- select(ssotNon,A101_01:A101_07,LA01_01:LA01_03,LA02_01:LA02_03)
