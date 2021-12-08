# 4.3.2 – the paper mentions the correlation between the number 
# of household members and the number of unique smart home devices. 
# Is there any correlation between the number of household members 
# and the adoption of specific types of devices? I ask this question because, 
# in 4.3.3, the paper presents the statistical test on the device level 
# (although only the most owned devices), so I wonder whether a similar 
# type of test has been conducted for 4.3.2. As a result, the current 
# analysis feels incomplete.

library(ggplot2)
theme_set(
  theme_classic() + 
    theme(legend.position = "top")
)

u <-
  select(
    singleSourceOfTruthAppended,
    participant_id,
    `Current Country of Residence`,
    R232_01,
    R232_02,
    R232_03,
    R233_01,
    R233_02 ,
    R233_03 ,
    R501,
    R503,
    R505,
    A005
  )

d1 <-
  select(subset(u, R233_01 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_01,
         R501, A005)
d2 <-
  select(subset(u, R233_02 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_02,
         R503, A005)
d3 <-
  select(subset(u, R233_03 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_03,
         R505, A005)
colnames(d1) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "Household")
colnames(d2) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "Household")
colnames(d3) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "Household")
d <- rbind(d1, d2, d3)
d <- subset(d, Usage != "Don't know")
d$Usage <-
  factor(
    d$Usage,
    levels = c(
      "0 times",
      "1-5 times",
      "6-10 times",
      "11-20 times",
      "21-30 times",
      "30+ times"
    )
  )

counts <- table(d$Device, d$Household)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears",  legend = rownames(counts), beside=TRUE, col =colors())

