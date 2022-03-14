##### 0.1 Installation of dependencies ----
list.of.packages <- c(
  "Hmisc",
  
  "tidyverse",
  
  "summarytools",
  
  "stringi",
  
  "sjPlot",
  
  "doBy",
  
  "janitor" ,
  
  "data.table",
  
  "plyr",
  
  "psych",
  
  "lattice",
  
  "multcompView",
  
  "dplyr",
  
  "ggpubr",
  
  "QCA",
  
  "FSA",
  
  "dunn.test",
  
  "rcompanion",
  
  "gplots",
  
  "hash",
  
  'knitr',
  
  'pagedown',
  
  'rmarkdown',
  
  "gtools"
  
)



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

library(doBy)

library(janitor)

library(gplots)

library(hash)

library(knitr)

library(pagedown)

library(rmarkdown)



### 0.2 Data cleaning ----



library(readr)



# 0.2.1   read export from SoSci

original_set <- deClutteredData



# 0.2.2   read export from prolific academic

prolific_export_DE <- read_csv("Excels/prolific_export_DE.csv")

prolific_export_UK <- read_csv("Excels/prolific_export_UK.csv")

prolific_export_US <- read_csv("Excels/prolific_export_US.csv")



# 0.2.3   generating prolific table from /excels folder

prolific_table_original = rbind(prolific_export_DE, prolific_export_US, prolific_export_UK)

table(prolific_table_original$status)

# APPROVED  REJECTED  RETURNED TIMED-OUT

# 463         2        28        19



# 0.2.4 filter out non-approved people from prolific academic

prolific_table_filtered = subset(prolific_table_original, status == "APPROVED")

prolific_table_filtered = dplyr::rename(prolific_table_filtered, prolific_age = age)


# n = 463

# 0.2.5 merge into full data frame by participant ID in the prolific dataset and their entered prolific ID in the survey

SSOT <-
  
  merge(prolific_table_filtered,
        
        original_set,
        
        by.x = "participant_id",
        
        by.y = "prolificID",
  )



# 0.2.5 remove participants who put "NA" countries of residence

SSOT <-
  
  subset(SSOT,
         
         `Current Country of Residence` != "NA")



# 0.2.7 consolidate GS region

SSOT$`Current Country of Residence` <-
  
  replace(
    as.character(SSOT$`Current Country of Residence`),
    
    SSOT$`Current Country of Residence` == "Germany",
    
    "GS"
    
  )

SSOT$`Current Country of Residence` <-
  
  replace(
    as.character(SSOT$`Current Country of Residence`),
    
    SSOT$`Current Country of Residence` == "Switzerland",
    
    "GS"
    
  )

SSOT$`Current Country of Residence` <-
  
  replace(
    as.character(SSOT$`Current Country of Residence`),
    
    SSOT$`Current Country of Residence` == "Austria",
    
    "GS"
    
  )


# renaming US UK
SSOT$`Current Country of Residence` <-
  
  replace(
    as.character(SSOT$`Current Country of Residence`),
    
    SSOT$`Current Country of Residence` == "United States",
    
    "US"
    
  )

SSOT$`Current Country of Residence` <-
  
  replace(
    as.character(SSOT$`Current Country of Residence`),
    
    SSOT$`Current Country of Residence` == "United Kingdom",
    
    "UK"
    
  )




# 0.2.8 remove everybody not from one of the specified regions

SSOT <-
  
  subset(
    SSOT,
    
    `Current Country of Residence` == "GS" |
      
      `Current Country of Residence` == "US" |
      
      `Current Country of Residence` == "UK"
    
  )



# 0.2.6 remove all people not interested in giving good answers

# Have you answered all questions in the study according to the provided instructions? # reduces by 8

SSOT <-
  
  subset(SSOT,
         
         followedinstructions == 1)

# Will you provide your best answers to each question in this study? # reduces by 0

SSOT <-
  
  subset(SSOT,
         
         commitment == 1)





# n = 439 here



SSOT <- subset(SSOT, attentioncheck == 1)



# reverse one item in Sebis proactive awareness

SSOT <- mutate(SSOT, comparison = 7 - sebis_awareness_openlink)

SSOT <- mutate(SSOT, comparison = 7 - sebis_awareness_sitelookfeel)

SSOT <-
  mutate(SSOT, comparison = 7 - sebis_awareness_urlverification)

SSOT <- mutate(SSOT, comparison = 7 - sebis_awareness_someoneelse)





# copy to new dataset for modification

ssot_new <- SSOT



#remove attention check column

# ssot_new <-
#   
#   within(ssot_new, rm(attentioncheck))



# create bool if participant has children or not from numeric var

ssot_new$numberofchildren <-
  
  ifelse(ssot_new$numberofchildren > 1, 1, 0) # having children



# 0.2.9 Translating Device names  ----

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Lautsprecher",
    
    "Smart Speaker"
    
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Glühbirne",
    
    "Smart Lightbulb"
    
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Lautsprecher",
    
    "Smart Speaker"
    
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Glühbirne",
    
    "Smart Lightbulb"
    
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Lautsprecher",
    
    "Smart Speaker"
    
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Glühbirne",
    
    "Smart Lightbulb"
    
  )



# translating rest of devices:

#translating device names from german to english

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Kaffeemaschine",
    
    "Smart Coffee Maker"
    
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Geschirrspüler",
    
    "Smart Dishwasher"
    
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Gartengerät",
    
    "Smart Lawnmower"
    
  )



ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Heiz-/Kühlsystem",
    
    "Smart Heating/Cooling System"
    
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Herd",
    
    "Smart Stove"
  )



ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Kühlschrank",
    
    "Smart Fridge"
    
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Ofen",
    
    "Smart Oven"
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Roboter",
    
    "Smart Robot"
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Spielzeug",
    
    "Smart Toy"
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Türklingel",
    
    "Smart Doorbell"
    
  )

ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Türschloss",
    
    "Smart Door Lock"
    
  )



ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Heim-Überwachungssystem",
    
    "Smart Home Monitoring System"
    
  )



ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Waschmaschine",
    
    "Smart Washing Machine"
    
  )



ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Stromzähler",
    
    "Smart Electricity Meter"
    
  )



ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Staubsauger",
    
    "Smart Vacuum Cleaner"
    
  )



ssot_new$device_picked_1_text <-
  
  replace(
    as.character(ssot_new$device_picked_1_text),
    
    ssot_new$device_picked_1_text == "Smart Steckdose",
    
    "Smart Electrical Outlet"
    
  )





## d 2



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Kaffeemaschine",
    
    "Smart Coffee Maker"
    
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Geschirrspüler",
    
    "Smart Dishwasher"
    
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Gartengerät",
    
    "Smart Lawnmower"
    
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Heiz-/Kühlsystem",
    
    "Smart Heating/Cooling System"
    
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Herd",
    
    "Smart Stove"
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Kühlschrank",
    
    "Smart Fridge"
    
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Ofen",
    
    "Smart Oven"
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Roboter",
    
    "Smart Robot"
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Spielzeug",
    
    "Smart Toy"
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Türklingel",
    
    "Smart Doorbell"
    
  )

ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Türschloss",
    
    "Smart Door Lock"
    
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Heim-Überwachungssystem",
    
    "Smart Home Monitoring System"
    
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Waschmaschine",
    
    "Smart Washing Machine"
    
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Stromzähler",
    
    "Smart Electricity Meter"
    
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Staubsauger",
    
    "Smart Vacuum Cleaner"
    
  )



ssot_new$device_picked_2_text <-
  
  replace(
    as.character(ssot_new$device_picked_2_text),
    
    ssot_new$device_picked_2_text == "Smart Steckdose",
    
    "Smart Electrical Outlet"
    
  )



## d3







ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Kaffeemaschine",
    
    "Smart Coffee Maker"
    
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Geschirrspüler",
    
    "Smart Dishwasher"
    
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Gartengerät",
    
    "Smart Lawnmower"
    
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Heiz-/Kühlsystem",
    
    "Smart Heating/Cooling System"
    
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Herd",
    
    "Smart Stove"
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Kühlschrank",
    
    "Smart Fridge"
    
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Ofen",
    
    "Smart Oven"
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Roboter",
    
    "Smart Robot"
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Spielzeug",
    
    "Smart Toy"
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Türklingel",
    
    "Smart Doorbell"
    
  )

ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Türschloss",
    
    "Smart Door Lock"
    
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Heim-Überwachungssystem",
    
    "Smart Home Monitoring System"
    
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Waschmaschine",
    
    "Smart Washing Machine"
    
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Stromzähler",
    
    "Smart Electricity Meter"
    
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Staubsauger",
    
    "Smart Vacuum Cleaner"
    
  )



ssot_new$device_picked_3_text <-
  
  replace(
    as.character(ssot_new$device_picked_3_text),
    
    ssot_new$device_picked_3_text == "Smart Steckdose",
    
    "Smart Electrical Outlet"
    
  )



# rename country variable

ssot_new = dplyr::rename(ssot_new, region = `Current Country of Residence`)
ssot_new$id <- 1:nrow(ssot_new)


submission_dataset = select(
  ssot_new,
  id,
  region,
  consent,
  followedinstructions,
  distractions,
  commitment,
  numberofshds,
  own_smartcoffeemaker,
  own_smartdishwasher,
  own_smartdoorlock,
  own_smartdoorbell,
  own_smartmeter,
  own_smartoutlet,
  own_smartfridge,
  own_smartgardening,
  own_smarthvac,
  own_smarthomemonitor,
  own_smartlightbulb,
  own_smartoven,
  own_smartrobot,
  own_smartspeaker,
  own_smartstove,
  own_smarttv,
  own_smartthermostat,
  own_smarttoy,
  own_smartvaccum,
  own_smartwasher,
  own_smartdevice_other,
  own_smartdevice_othertext,
  device1_price,
  device2_price,
  device3_price,
  device1_purchaselocation,
  device2_purchaselocation,
  device2_purchaselocation_othertext,
  device3_purchaselocation,
  device3_purchaselocation_othertext,
  device1_used,
  device2_used,
  device3_used,
  device1_primaryuse_convenience,
  device1_primaryuse_savemoney,
  device1_primaryuse_savepower,
  device1_primaryuse_safety,
  device1_primaryuse_automation,
  device1_primaryuse_latesttech,
  device1_primaryuse_other,
  device1_primaryuse_othertext,
  device1_primaryuse_convenience,
  device1_primaryuse_savemoney,
  device1_primaryuse_savepower,
  device1_primaryuse_safety,
  device1_primaryuse_automation,
  device1_primaryuse_latesttech,
  device1_primaryuse_other,
  device1_primaryuse_othertext,
  device2_primaryuse_convenience,
  device2_primaryuse_savemoney,
  device2_primaryuse_savepower,
  device2_primaryuse_safety,
  device2_primaryuse_automation,
  device2_primaryuse_latesttech,
  device2_primaryuse_other,
  device2_primaryuse_othertext,
  device2_primaryuse_convenience,
  device2_primaryuse_savemoney,
  device2_primaryuse_savepower,
  device2_primaryuse_safety,
  device2_primaryuse_automation,
  device2_primaryuse_latesttech,
  device2_primaryuse_other,
  device2_primaryuse_othertext,
  device3_primaryuse_convenience,
  device3_primaryuse_savemoney,
  device3_primaryuse_savepower,
  device3_primaryuse_safety,
  device3_primaryuse_automation,
  device3_primaryuse_latesttech,
  device3_primaryuse_other,
  device3_primaryuse_othertext,
  device3_primaryuse_convenience,
  device3_primaryuse_savemoney,
  device3_primaryuse_savepower,
  device3_primaryuse_safety,
  device3_primaryuse_automation,
  device3_primaryuse_latesttech,
  device3_primaryuse_other,
  device3_primaryuse_othertext,
  device1_infosources_reviews,
  device1_infosources_forums,
  device1_infosources_printmedia,
  device1_infosources_friendsfamily,
  device1_infosources_news,
  device1_infosources_other,
  device1_infosources_othertext,
  device1_infosources_reviews,
  device1_infosources_forums,
  device1_infosources_printmedia,
  device1_infosources_friendsfamily,
  device1_infosources_news,
  device1_infosources_other,
  device1_infosources_othertext,
  device2_infosources_reviews,
  device2_infosources_forums,
  device2_infosources_printmedia,
  device2_infosources_friendsfamily,
  device2_infosources_news,
  device2_infosources_other,
  device2_infosources_othertext,
  device2_infosources_reviews,
  device2_infosources_forums,
  device2_infosources_printmedia,
  device2_infosources_friendsfamily,
  device2_infosources_news,
  device2_infosources_other,
  device2_infosources_othertext,
  device3_infosources_reviews,
  device3_infosources_forums,
  device3_infosources_printmedia,
  device3_infosources_friendsfamily,
  device3_infosources_news,
  device3_infosources_other,
  device3_infosources_othertext,
  device3_infosources_reviews,
  device3_infosources_forums,
  device3_infosources_printmedia,
  device3_infosources_friendsfamily,
  device3_infosources_news,
  device3_infosources_other,
  device3_infosources_othertext,
  device1_purchasereason,
  device2_purchasereason,
  device3_purchasereason,
  device1_hearinfo_tv,
  device1_hearinfo_internet,
  device1_hearinfo_printmedia,
  device1_hearinfo_friendsfamily,
  device1_hearinfo_store,
  device1_hearinfo_tradeshow,
  device1_hearinfo_other,
  device1_hearinfo_othertext,
  device2_hearinfo_tv,
  device2_hearinfo_internet,
  device2_hearinfo_printmedia,
  device2_hearinfo_friendsfamily,
  device2_hearinfo_store,
  device2_hearinfo_tradeshow,
  device2_hearinfo_other,
  device2_hearinfo_othertext,
  device3_hearinfo_tv,
  device3_hearinfo_internet,
  device3_hearinfo_printmedia,
  device3_hearinfo_friendsfamily,
  device3_hearinfo_store,
  device3_hearinfo_tradeshow,
  device3_hearinfo_other,
  device3_hearinfo_othertext,
  device1_hearinfo_tv,
  device1_hearinfo_internet,
  device1_hearinfo_printmedia,
  device1_hearinfo_friendsfamily,
  device1_hearinfo_store,
  device1_hearinfo_tradeshow,
  device1_hearinfo_other,
  device1_hearinfo_othertext,
  device2_hearinfo_tv,
  device2_hearinfo_internet,
  device2_hearinfo_printmedia,
  device2_hearinfo_friendsfamily,
  device2_hearinfo_store,
  device2_hearinfo_tradeshow,
  device2_hearinfo_other,
  device2_hearinfo_othertext,
  device3_hearinfo_tv,
  device3_hearinfo_internet,
  device3_hearinfo_printmedia,
  device3_hearinfo_friendsfamily,
  device3_hearinfo_store,
  device3_hearinfo_tradeshow,
  device3_hearinfo_other,
  device3_hearinfo_othertext,
  device1_protectdata,
  device1_preventthirdparty,
  device2_protectdata,
  device2_preventthirdparty,
  device3_protectdata,
  device3_preventthirdparty,
  device1_continueeducation,
  device1_continueeducation_text,
  device2_continueeducation,
  device2_continueeducation_text,
  device3_continueeducation,
  device3_continueeducation_text,
  device1_whenpurchased,
  device2_whenpurchased,
  device3_whenpurchased,
  device1_usefrequency,
  device1_alwayson,
  device2_usefrequency,
  device2_alwayson,
  device3_usefrequency,
  device3_alwayson,
  device1_disabledfeatures,
  device1_disabledfeatures_text,
  device1_disabledfeatures_battery,
  device1_disabledfeatures_savepower,
  device1_disabledfeatures_privacy,
  device1_disabledfeatures_security,
  device1_disabledfeatures_ux,
  device1_disabledfeatures_unused,
  device1_disabledfeatures_other,
  device1_disabledfeatures_othertext,
  device1_disabledfeatures_battery,
  device1_disabledfeatures_savepower,
  device1_disabledfeatures_privacy,
  device1_disabledfeatures_security,
  device1_disabledfeatures_ux,
  device1_disabledfeatures_unused,
  device1_disabledfeatures_other,
  device1_disabledfeatures_othertext,
  device2_disabledfeatures,
  device2_disabledfeatures_text,
  device2_disabledfeatures_battery,
  device2_disabledfeatures_savepower,
  device2_disabledfeatures_privacy,
  device2_disabledfeatures_security,
  device2_disabledfeatures_ux,
  device2_disabledfeatures_unused,
  device2_disabledfeatures_other,
  device2_disabledfeatures_othertext,
  device2_disabledfeatures_battery,
  device2_disabledfeatures_savepower,
  device2_disabledfeatures_privacy,
  device2_disabledfeatures_security,
  device2_disabledfeatures_ux,
  device2_disabledfeatures_unused,
  device2_disabledfeatures_other,
  device2_disabledfeatures_othertext,
  device3_disabledfeatures,
  device3_disabledfeatures_text,
  device3_disabledfeatures_battery,
  device3_disabledfeatures_savepower,
  device3_disabledfeatures_privacy,
  device3_disabledfeatures_security,
  device3_disabledfeatures_ux,
  device3_disabledfeatures_unused,
  device3_disabledfeatures_other,
  device3_disabledfeatures_othertext,
  device3_disabledfeatures_battery,
  device3_disabledfeatures_savepower,
  device3_disabledfeatures_privacy,
  device3_disabledfeatures_security,
  device3_disabledfeatures_ux,
  device3_disabledfeatures_unused,
  device3_disabledfeatures_other,
  device3_disabledfeatures_othertext,
  device1_uselocation,
  device1_useroutines,
  device1_usespeech,
  device1_usevideo,
  device1_useimages,
  device1_useenvironment,
  device1_useinternet,
  device1_onlinepurchases,
  device1_useofflinepurchases,
  device1_usepowerconsumption,
  device1_useactivities,
  device1_useother,
  device1_useothertext,
  device1_uselocation,
  device1_useroutines,
  device1_usespeech,
  device1_usevideo,
  device1_useimages,
  device1_useenvironment,
  device1_useinternet,
  device1_onlinepurchases,
  device1_useofflinepurchases,
  device1_usepowerconsumption,
  device1_useactivities,
  device1_useother,
  device1_useothertext,
  device2_uselocation,
  device2_useroutines,
  device2_usespeech,
  device2_usevideo,
  device2_useimages,
  device2_useenvironment,
  device2_useinternet,
  device2_onlinepurchases,
  device2_useofflinepurchases,
  device2_usepowerconsumption,
  device2_useactivities,
  device2_useother,
  device2_useothertext,
  device2_uselocation,
  device2_useroutines,
  device2_usespeech,
  device2_usevideo,
  device2_useimages,
  device2_useenvironment,
  device2_useinternet,
  device2_onlinepurchases,
  device2_useofflinepurchases,
  device2_usepowerconsumption,
  device2_useactivities,
  device2_useother,
  device2_useothertext,
  device3_uselocation,
  device3_useroutines,
  device3_usespeech,
  device3_usevideo,
  device3_useimages,
  device3_useenvironment,
  device3_useinternet,
  device3_onlinepurchases,
  device3_useofflinepurchases,
  device3_usepowerconsumption,
  device3_useactivities,
  device3_useother,
  device3_useothertext,
  device3_uselocation,
  device3_useroutines,
  device3_usespeech,
  device3_usevideo,
  device3_useimages,
  device3_useenvironment,
  device3_useinternet,
  device3_onlinepurchases,
  device3_useofflinepurchases,
  device3_usepowerconsumption,
  device3_useactivities,
  device3_useother,
  device3_useothertext,
  device1_storeinternet,
  device1_storecloud,
  device1_storelocal,
  device1_storeisp,
  device1_storemanufacturer,
  device1_storethirdparties,
  device1_storeother,
  device1_storeothertext,
  device1_storeinternet,
  device1_storecloud,
  device1_storelocal,
  device1_storeisp,
  device1_storemanufacturer,
  device1_storethirdparties,
  device1_storeother,
  device1_storeothertext,
  device2_storeinternet,
  device2_storecloud,
  device2_storelocal,
  device2_storeisp,
  device2_storemanufacturer,
  device2_storethirdparties,
  device2_storeother,
  device2_storeothertext,
  device2_storeinternet,
  device2_storecloud,
  device2_storelocal,
  device2_storeisp,
  device2_storemanufacturer,
  device2_storethirdparties,
  device2_storeother,
  device2_storeothertext,
  device3_storeinternet,
  device3_storecloud,
  device3_storelocal,
  device3_storeisp,
  device3_storemanufacturer,
  device3_storethirdparties,
  device3_storeother,
  device3_storeothertext,
  device3_storeinternet,
  device3_storecloud,
  device3_storelocal,
  device3_storeisp,
  device3_storemanufacturer,
  device3_storethirdparties,
  device3_storeother,
  device3_storeothertext,
  device1_locationattic,
  device1_locationbalcony,
  device1_locationbasement,
  device1_locationkidsroom,
  device1_locationdining,
  device1_locationgarage,
  device1_locationguestbedroom,
  device1_locationhallway,
  device1_locationkitchen,
  device1_locationlivingroom,
  device1_locationmasterbedroom,
  device1_locationpatio,
  device1_locationyard,
  device1_locationother,
  device1_locationothertext,
  device1_locationattic,
  device1_locationbalcony,
  device1_locationbasement,
  device1_locationkidsroom,
  device1_locationdining,
  device1_locationgarage,
  device1_locationguestbedroom,
  device1_locationhallway,
  device1_locationkitchen,
  device1_locationlivingroom,
  device1_locationmasterbedroom,
  device1_locationpatio,
  device1_locationyard,
  device1_locationother,
  device1_locationothertext,
  device2_locationattic,
  device2_locationbalcony,
  device2_locationbasement,
  device2_locationkidsroom,
  device2_locationdining,
  device2_locationgarage,
  device2_locationguestbedroom,
  device2_locationhallway,
  device2_locationkitchen,
  device2_locationlivingroom,
  device2_locationmasterbedroom,
  device2_locationpatio,
  device2_locationyard,
  device2_locationother,
  device2_locationothertext,
  device2_locationattic,
  device2_locationbalcony,
  device2_locationbasement,
  device2_locationkidsroom,
  device2_locationdining,
  device2_locationgarage,
  device2_locationguestbedroom,
  device2_locationhallway,
  device2_locationkitchen,
  device2_locationlivingroom,
  device2_locationmasterbedroom,
  device2_locationpatio,
  device2_locationyard,
  device2_locationother,
  device2_locationothertext,
  device3_locationattic,
  device3_locationbalcony,
  device3_locationbasement,
  device3_locationkidsroom,
  device3_locationdining,
  device3_locationgarage,
  device3_locationguestbedroom,
  device3_locationhallway,
  device3_locationkitchen,
  device3_locationlivingroom,
  device3_locationmasterbedroom,
  device3_locationpatio,
  device3_locationyard,
  device3_locationother,
  device3_locationothertext,
  device3_locationattic,
  device3_locationbalcony,
  device3_locationbasement,
  device3_locationkidsroom,
  device3_locationdining,
  device3_locationgarage,
  device3_locationguestbedroom,
  device3_locationhallway,
  device3_locationkitchen,
  device3_locationlivingroom,
  device3_locationmasterbedroom,
  device3_locationpatio,
  device3_locationyard,
  device3_locationother,
  device3_locationothertext,
  device1_interactvoice,
  device1_interactapp,
  device1_interactbuttons,
  device1_interactscreen,
  device1_interactinternet,
  device1_interactrouter,
  device1_interactother,
  device1_interactothertext,
  device1_interactvoice,
  device1_interactapp,
  device1_interactbuttons,
  device1_interactscreen,
  device1_interactinternet,
  device1_interactrouter,
  device1_interactother,
  device1_interactothertext,
  device2_interactvoice,
  device2_interactapp,
  device2_interactbuttons,
  device2_interactscreen,
  device2_interactinternet,
  device2_interactrouter,
  device2_interactother,
  device2_interactothertext,
  device2_interactvoice,
  device2_interactapp,
  device2_interactbuttons,
  device2_interactscreen,
  device2_interactinternet,
  device2_interactrouter,
  device2_interactother,
  device2_interactothertext,
  device3_interactvoice,
  device3_interactapp,
  device3_interactbuttons,
  device3_interactscreen,
  device3_interactinternet,
  device3_interactrouter,
  device3_interactother,
  device3_interactothertext,
  device3_interactvoice,
  device3_interactapp,
  device3_interactbuttons,
  device3_interactscreen,
  device3_interactinternet,
  device3_interactrouter,
  device3_interactother,
  device3_interactothertext,
  shddata_kinds,
  shddata_value,
  shddata_collection,
  shddata_regularcollection,
  shddata_internet,
  shddata_resource,
  shddata_remotestorage,
  shddata_awarestoragelocation,
  shddata_awareanalysis,
  shddata_awareparties,
  shddata_homestorage,
  shddata_externalstorage,
  shddata_localprocessing,
  shddata_externalprocessing,
  softwareupdate,
  ensureprivacy,
  shdecosystemprotection,
  devicesecurity,
  hardwarefix,
  softwarefix,
  selfupdate,
  securityupdates,
  datasecurity,
  remotemanagement,
  ensureuptodate,
  stopsupportnotice,
  updateunsure,
  updateforever,
  smartbenefit_coffeemaker,
  smartbenefit_dishwasher,
  smartbenefit_doorlock,
  smartbenefit_doorbell,
  smartbenefit_electricitymeter,
  smartbenefit_outlet,
  smartbenefit_fridge,
  smartbenefit_gardening,
  smartbenefit_hvac,
  smartbenefit_homemonitor,
  smartbenefit_lightbulb,
  smartbenefit_oven,
  smartbenefit_speaker,
  smartbenefit_stove,
  smartbenefit_tv,
  smartbenefit_thermostat,
  smartbenefit_toy,
  smartbenefit_vacuum,
  smartbenefit_washer,
  smartbenefit_other,
  smartbenefit_othertext,
  smartsecurityrisk_,
  smartsecurityrisk_coffeemaker,
  smartsecurityrisk_dishwasher,
  smartsecurityrisk_doorlock,
  smartsecurityrisk_doorbell,
  smartsecurityrisk_electricitymeter,
  smartsecurityrisk_outlet,
  smartsecurityrisk_fridge,
  smartsecurityrisk_gardening,
  smartsecurityrisk_hvac,
  smartsecurityrisk_homemonitor,
  smartsecurityrisk_lightbulb,
  smartsecurityrisk_oven,
  smartsecurityrisk_speaker,
  smartsecurityrisk_stove,
  smartsecurityrisk_tv,
  smartsecurityrisk_thermostat,
  smartsecurityrisk_toy,
  smartsecurityrisk_vacuum,
  smartsecurityrisk_washer,
  smartsecurityrisk_other,
  smartsecurityrisk_othertext,
  smartfunction_media,
  smartfunction_appliances,
  smartfunction_hvac,
  smartfunction_powerconsumption,
  smartfunction_detectproblems,
  smartfunction_monitorhealth,
  smartfunction_safety,
  smartfunction_communication,
  smartfunction_othertext,
  benefit_savemoney,
  benefit_saveenergy,
  benefit_convenience,
  benefit_leisure,
  benefit_peaceofmind,
  benefit_comfort,
  benefit_safety,
  benefit_care,
  benefit_lifequality,
  benefit_propertyvalue,
  benefit_othertext,
  purpose_appliances,
  purpose_homemonitoring,
  purpose_internalcommunication,
  purpose_externalcommunication,
  purpose_automation,
  purpose_othertext,
  shdexperience,
  shdknowledge,
  devicerisk_coffeemaker,
  devicerisk_dishwasher,
  devicerisk_doorlock,
  devicerisk_doorbell,
  devicerisk_electricitymeter,
  devicerisk_outlet,
  devicerisk_fridge,
  devicerisk_gardening,
  devicerisk_hvac,
  devicerisk_homemonitor,
  devicerisk_lightbulb,
  devicerisk_oven,
  devicerisk_robot,
  devicerisk_speaker,
  devicerisk_stove,
  devicerisk_tv,
  devicerisk_thermostat,
  devicerisk_toy,
  devicerisk_vacuum,
  devicerisk_washer,
  notown_adultprivacy,
  notown_childrenprivacy,
  notown_guestprivacy,
  notown_petprivacy,
  notown_nobenefit,
  notown_cost,
  notown_nointernet,
  notown_other,
  notown_othertext,
  shdposedrisks,
  shdcomfort_home,
  shdcomfort_away,
  shdcomfort_otherdevice,
  shdcomfort_owndevice,
  interactcomfort_speaker,
  interactcomfort_assistant,
  interactcomfort_app,
  interactcomfort_widget,
  interactcomfort_insidesensor,
  interactcomfort_outsidesensor,
  interactcomfort_automation,
  interactcomfort_othertext,
  consult_friendsfamily,
  consult_forums,
  consult_printmedia,
  consult_reviews,
  consult_other,
  consult_othertext,
  purchaseinfluence_price,
  purchaseinfluence_bundle,
  purchaseinfluence_trial,
  purchaseinfluence_sale,
  purchaseinfluence_discount,
  purchasecomfort_online_domestic,
  purchasecomfort_online_foreign,
  purchasecomfort_store,
  purchasecomfort_departmentalstore,
  regulation_thirdpartyaccess,
  regulation_thirdpartysharing,
  regulation_thirdpartyprocessing,
  regulation_accesspenalty,
  regulation_sharepenalty,
  regulation_storepenalty,
  sebis_securement_screenlockauto,
  sebis_securement_computerpassword,
  sebis_securement_screenlockmanual,
  sebis_securement_mobilepassword,
  sebis_awareness_openlink,
  sebis_awareness_sitelookfeel,
  sebis_awareness_urlverification,
  attentioncheck,
  sebis_awareness_mouseover,
  sebis_awareness_someoneelse,
  sebis_updating_immediate,
  sebis_updating_ensure,
  sebis_updating_antivirus,
  muipc_surveillance_location,
  muipc_surveillance_toomuch,
  muipc_surveillance_activities,
  muipc_intrusion_othersknow,
  muipc_intrusion_readilyavailable,
  muipc_intrusion_privacyinvasion,
  muipc_secondaryuse_unauthorized,
  muipc_secondaryuse_otherpurposes,
  muipc_secondaryuse_shareunauthorized,
  age,
  gender,
  gender_othertext,
  education,
  education_othertext,
  numberofchildren,
  householdsize,
  income,
  homeowner,
  homeowner_othertext,
  ethnicity_nativeamerican,
  ethnicity_asian,
  ethnicity_african,
  ethnicity_hawaiian,
  ethnicity_white,
  ethnicity_hispanic,
  ethnicity_other,
  ethnicity_othertext,
  employment_fulltime,
  employmentparttime,
  employment_unemployedlooking,
  employment_unemployednotloooking,
  employment_homemaker,
  employmentstudent,
  employment_retired,
  employment_other,
  employment_othertext,
  occupation,
  fieldofstudy,
  
  device_picked_1_text,
  device_picked_2_text,
  device_picked_3_text,
  device_picked_1_owned,
  device_picked_2_owned,
  device_picked_3_owned,
)


# write CSV to dataset
write.csv(submission_dataset,"0_submission_files/0_submission_dataset.csv", row.names = FALSE)
