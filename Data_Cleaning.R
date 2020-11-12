#get data from sosci
eval(parse("https://www.soscisurvey.de/smarthomestudy/index.php?act=Vook4VGznvfZ1cfCMSZuRtUh&vQuality&useSettings&rScript", encoding="UTF-8"))

#copy dataset for safety
ssot <- soSciDataSet

#generating prolific table from /excels folder
prolific_table = rbind(prolific_export_DE,prolific_export_US,prolific_export_UK)
table(prolific_table$status)
prolific_table = subset(prolific_table, status == "APPROVED")

#merge into full data frame 
SSOT <- merge(prolific_table,soSciDataSet, by.x = "participant_id",by.y ="A008_01",)

#remove "NA" countries of residence
SSOT <-
  subset(SSOT,
         `Current Country of Residence` != "NA")    

#remove all people not interested in giving good answers
SSOT <-
  subset(SSOT,
         CS06 == "I answered all questions according to the provided instructions.")              
SSOT <-
  subset(SSOT,
         CS10 == "I will provide my best answers.")   

#consolidate DACH region
SSOT$`Current Country of Residence`<- replace(as.character(SSOT$`Current Country of Residence`), SSOT$`Current Country of Residence` == "Germany", "DACH")
SSOT$`Current Country of Residence`<- replace(as.character(SSOT$`Current Country of Residence`), SSOT$`Current Country of Residence` == "Switzerland", "DACH")
SSOT$`Current Country of Residence`<- replace(as.character(SSOT$`Current Country of Residence`), SSOT$`Current Country of Residence` == "Austria", "DACH")

#remove all Non-US/DACH/UK
SSOT <- subset(SSOT, `Current Country of Residence` == "DACH" | `Current Country of Residence` == "United States" | `Current Country of Residence` == "United Kingdom" )

write.csv2(SSOT, "SSOT.csv")
