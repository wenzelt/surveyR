library(readr)

# 0.1   read export from SoSci 
original_set <- read_delim("original_data_set/rdata_smarthomestudy_2019-12-08_11-45.csv", 
                                                    delim = "\t", escape_double = FALSE, 
                                                    trim_ws = TRUE)

# 0.2   read export from prolific academic 
prolific_export_DE <- read_csv("Excels/prolific_export_DE.csv")
prolific_export_UK <- read_csv("Excels/prolific_export_UK.csv")
prolific_export_US <- read_csv("Excels/prolific_export_US.csv")

# 0.3   generating prolific table from /excels folder
prolific_table_original = rbind(prolific_export_DE,prolific_export_US,prolific_export_UK)
table(prolific_table_original$status)
    # APPROVED  REJECTED  RETURNED TIMED-OUT 
    # 463         2        28        19 

# 0.4 filter out non-approved people from prolific academic
prolific_table_filtered = subset(prolific_table_original, status == "APPROVED")

# 0.5 merge into full data frame by participant ID in the prolific dataset and their entered prolific ID in the survey 
SSOT <- merge(prolific_table_filtered,original_set, by.x = "participant_id",by.y ="A008_01",)

# 0.5 remove participants who put "NA" countries of residence
SSOT <-
  subset(SSOT,
         `Current Country of Residence` != "NA")    

# 0.6 remove all people not interested in giving good answers
    # Have you answered all questions in the study according to the provided instructions? 
SSOT <-
  subset(SSOT,
         CS06 == 1)     
    # Will you provide your best answers to each question in this study?
SSOT <-
  subset(SSOT,
         CS10 == 1) 

# 0.7 consolidate DACH region
SSOT$`Current Country of Residence`<- replace(as.character(SSOT$`Current Country of Residence`), SSOT$`Current Country of Residence` == "Germany", "DACH")
SSOT$`Current Country of Residence`<- replace(as.character(SSOT$`Current Country of Residence`), SSOT$`Current Country of Residence` == "Switzerland", "DACH")
SSOT$`Current Country of Residence`<- replace(as.character(SSOT$`Current Country of Residence`), SSOT$`Current Country of Residence` == "Austria", "DACH")

# 0.8 remove everybody not from one of the specified regions
SSOT <- subset(SSOT, `Current Country of Residence` == "DACH" | `Current Country of Residence` == "United States" | `Current Country of Residence` == "United Kingdom" )

# n = 439 here 

SSOT <- subset(SSOT, S101_13 == 1 )

