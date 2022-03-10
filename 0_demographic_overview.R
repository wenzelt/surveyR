

# D1 - table1 ----

# Demographic overview of the sample. Please run 'install_dependencies.R first'

demographic_data <-
  select(ssot_new,
         `Current Country of Residence`,
         A002,
         age)
names(demographic_data)[names(demographic_data) == "Current Country of Residence"] <-
  "country"
names(demographic_data)[names(demographic_data) == "A002"] <-
  "gender"

median(as.numeric(na.omit(demographic_data$age)))

attach(demographic_data)


#3 people preferred not to enter their age
age_data <- summaryBy(
  age ~ country,
  data = demographic_data,
  FUN = c(median, mean, sd, min, max),
  na.rm = TRUE
)

gender_data <- tabyl(demographic_data, country, gender) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)

gender_data_absolute = tabyl(demographic_data,country,gender)

demographic_overview_table <- bind_cols(count(country),age_data[2:6], gender_data[2:4], gender_data_absolute[2:4])

detach(demographic_data)

gs = subset(demographic_data, country == "DACH")
uk = subset(demographic_data, country == "United Kingdom")
us = subset(demographic_data, country == "United States")

count(gs$gender)
count(uk$gender)
count(us$gender)



# D2 - Demographic by device - table 2 ====
device_information_region = select(ssot_new, R101,`Current Country of Residence`)
count(device_information_region, "R101")
counts <- ddply(device_information_region, .(device_information_region$R101, device_information_region$`Current Country of Residence`), nrow)
dt <- data.table(device_information_region) # transpose to data.table
dt = dt[, list(Freq =.N), by=list(R101,`Current Country of Residence`)] # use list to name var directly


