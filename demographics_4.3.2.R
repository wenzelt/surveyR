# 4.3.2 – the paper mentions the correlation between the number 
# of household members and the number of unique smart home devices. 
# Is there any correlation between the number of household members 
# and the adoption of specific types of devices? I ask this question because, 
# in 4.3.3, the paper presents the statistical test on the device level 
# (although only the most owned devices), so I wonder whether a similar 
# type of test has been conducted for 4.3.2. As a result, the current 
# analysis feels incomplete.

library(ggplot2)
library(pastecs)
library(ryouready)

theme_set(
  theme_classic() + 
    theme(legend.position = "top")
)

# translating rest of devices: 
#translating device names from german to english
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Kaffeemaschine",
    "Smart Coffee Maker"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Geschirrspüler",
    "Smart Dishwasher"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Gartengerät",
    "Smart Lawnmower"
  )

singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Heiz-/Kühlsystem",
    "Smart Heating/Cooling System"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Herd",
    "Smart Stove"
  )

singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Kühlschrank",
    "Smart Fridge"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Ofen",
    "Smart Oven"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Roboter",
    "Smart Robot"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Spielzeug",
    "Smart Toy"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Türklingel",
    "Smart Doorbell"
  )
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Türschloss",
    "Smart Door Lock"
  )

##
singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Heim-Überwachungssystem",
    "Smart Home Monitoring System"
  )

singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Waschmaschine",
    "Smart Washing Machine"
  )

singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Stromzähler",
    "Smart Electricity Meter"
  )

singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Staubsauger",
    "Smart Vacuum Cleaner"
  )

singleSourceOfTruthAppended$R232_01 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_01 == "Smart Steckdose",
    "Smart Electrical Outlet"
  )


## d 2

singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Kaffeemaschine",
    "Smart Coffee Maker"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Geschirrspüler",
    "Smart Dishwasher"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Gartengerät",
    "Smart Lawnmower"
  )

singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Heiz-/Kühlsystem",
    "Smart Heating/Cooling System"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Herd",
    "Smart Stove"
  )

singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Kühlschrank",
    "Smart Fridge"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Ofen",
    "Smart Oven"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Roboter",
    "Smart Robot"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Spielzeug",
    "Smart Toy"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Türklingel",
    "Smart Doorbell"
  )
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Türschloss",
    "Smart Door Lock"
  )
##
singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Heim-Überwachungssystem",
    "Smart Home Monitoring System"
  )

singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Waschmaschine",
    "Smart Washing Machine"
  )

singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Stromzähler",
    "Smart Electricity Meter"
  )

singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_02),
    singleSourceOfTruthAppended$R232_02 == "Smart Staubsauger",
    "Smart Vacuum Cleaner"
  )

singleSourceOfTruthAppended$R232_02 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_02 == "Smart Steckdose",
    "Smart Electrical Outlet"
  )

## d3 



singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Kaffeemaschine",
    "Smart Coffee Maker"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Geschirrspüler",
    "Smart Dishwasher"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Gartengerät",
    "Smart Lawnmower"
  )

singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Heiz-/Kühlsystem",
    "Smart Heating/Cooling System"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Herd",
    "Smart Stove"
  )

singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Kühlschrank",
    "Smart Fridge"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Ofen",
    "Smart Oven"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Roboter",
    "Smart Robot"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Spielzeug",
    "Smart Toy"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Türklingel",
    "Smart Doorbell"
  )
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Türschloss",
    "Smart Door Lock"
  )


##
singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Heim-Überwachungssystem",
    "Smart Home Monitoring System"
  )

singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Waschmaschine",
    "Smart Washing Machine"
  )

singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Stromzähler",
    "Smart Electricity Meter"
  )

singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_03),
    singleSourceOfTruthAppended$R232_03 == "Smart Staubsauger",
    "Smart Vacuum Cleaner"
  )

singleSourceOfTruthAppended$R232_03 <-
  replace(
    as.character(singleSourceOfTruthAppended$R232_01),
    singleSourceOfTruthAppended$R232_03 == "Smart Steckdose",
    "Smart Electrical Outlet"
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
    A005,
    R101
  )

d1 <-
  select(subset(u, R233_01 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_01,
         R501, A005,R101)
d2 <-
  select(subset(u, R233_02 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_02,
         R503, A005,R101)
d3 <-
  select(subset(u, R233_03 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_03,
         R505, A005,R101)
colnames(d1) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "People in Household", "Number Devices in Household")
colnames(d2) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "People in Household", "Number Devices in Household")
colnames(d3) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage", "People in Household", "Number Devices in Household")
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

table(d$`Number Devices in Household`, d$`People in Household`)
freq(d$`Number Devices in Household`, order = "freq")
stat.desc(d$`People in Household`)
qqnorm(d$`People in Household`)
qqline(d$`People in Household`, col = "red")

by(d$`People in Household`, INDICES = d$`Number Devices in Household`, FUN = stat.desc)

eta(d$`Number Devices in Household`,d$`People in Household`, breaks = NULL, na.rm = T)

barplot(counts, main="Devices by Count",
        xlab="Number of Devices in Household",  legend = rownames(counts), beside=TRUE, col =colors())

temp = select(d,Device,`Number Devices in Household`,`People in Household`)
