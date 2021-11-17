# Demographic overview of the sample. Please run 'install_dependencies.R first'

demographic_data <-
  select(singleSourceOfTruthAppended,
         `Current Country of Residence`,
         A002,
         age)
names(demographic_data)[names(demographic_data) == "Current Country of Residence"] <-
  "country"
names(demographic_data)[names(demographic_data) == "A002"] <-
  "gender"

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

