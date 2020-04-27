list.of.packages <- c("ggplot2", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)

#householasdd <- select(singleSourceOfTruthAppended,participant_id,A004,A005,A006,A007,`Current Country of Residence`)

barplot(table(singleSourceOfTruthAppended$A007), main = "Count Ownership Situation", ylim = c(0,300)) #count ownership situation
dev.copy(png,'RENT_OWN.png')
dev.off()


barplot(table(singleSourceOfTruthAppended$A004), main = "Children in singleSourceOfTruthAppended", ylim = c(0,300)) #count ownership situation
dev.copy(png,'Children_in_Household.png')
dev.off()

barplot(table(singleSourceOfTruthAppended$A005), main = "singleSourceOfTruthAppended Size", ylim = c(0,300)) #count ownership situation
dev.copy(png,'singleSourceOfTruthAppended_Size.png')
dev.off()

barplot(table(singleSourceOfTruthAppended$A006), main = "Personal Income", ylim = c(0,300)) #count ownership situation
dev.copy(png,'Personal_Income.png')
dev.off()

barplot(table(singleSourceOfTruthAppended$`Current Country of Residence`), main = "Current Country of Residence", ylim = c(0,300)) #count ownership situation
dev.copy(png,'Current Country of Residence.png')
dev.off()


barplot(table(singleSourceOfTruthAppended$R101), main = "Amount of Smart Device from Device List Owned", ylim = c(0,300)) #count ownership situation
dev.copy(png,'Number_Smart_devices_owned.png')
dev.off()

par(mar=c(12, 4 ,4.1 ,2.1))
barplot(table(singleSourceOfTruthAppended$R204), main = "Purchase Location", ylim = c(0,150), las = 3) #count ownership situation
dev.copy(png,'Purchase_Location.png')
dev.off()
par(mar=c(5.1, 4.1, 4.1, 2.1)

#################Plotting true false 

R216 <- select(singleSourceOfTruthAppended, R216_01:R216_06)
barplot(colSums(R216, na.rm = T), ylim = c(0,300),main = "Which of the following Sources did you consult prior to purchasing this device?")
dev.copy(png,'Source_Consultation.png')
dev.off()

R210 <- select(singleSourceOfTruthAppended, R210_01:R210_06)
barplot(colSums(R216, na.rm = T), ylim = c(0,300),main = "Which of the following Sources did you consult prior to purchasing this device?")
dev.copy(png,'Use_case.png')
dev.off()

