# Demographic overview of the sample. Please run 'install_dependencies.R first'

demographic_data <-
  select(singleSourceOfTruthAppended,
         Sex,
         age, participant_id)

device_owners <- d %>% distinct(participant_id, .keep_all = TRUE)
demographic_data <- merge(x=demographic_data, y=device_owners, by = "participant_id")


names(demographic_data)[names(demographic_data) == "Current Country of Residence"] <-
  "country"
names(demographic_data)[names(demographic_data) == "Sex"] <-
  "gender"

attach(demographic_data)

demographic_data <- subset(demographic_data, Device == "Smart Speaker" | Device == "Smart TV"| Device == "Smart Lightbulb")
anon_device <- data.frame(lapply(demographic_data, function(x) {gsub("Smart Speaker", "Device", x)}))
anon_device <- data.frame(lapply(anon_device, function(x) {gsub("Smart TV", "Device", x)}))
anon_device <- data.frame(lapply(anon_device, function(x) {gsub("Smart Lightbulb", "Device", x)}))

anon_mean <- mean(as.numeric(anon_device$age))
anon_median <- median(as.numeric(anon_device$age))

#3 people preferred not to enter their age
age_data <- summaryBy(
  age ~ Device,
  data = anon_device,
  FUN = c(median, mean, sd, min, max),
  na.rm = TRUE
)

gender_data <- tabyl(anon_device, Device, gender) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

demographic_overview_table <- bind_cols(count(Device),age_data[2:6], gender_data[2:3])

subset <- subset(demographic_overview_table, x == "Smart Speaker" | x == "Smart TV"| x == "Smart Lightbulb")


detach(demographic_data)
