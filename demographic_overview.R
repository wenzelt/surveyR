# Demographic overview of the sample. Please run 'install_dependencies.R first'
install.packages('doBy')
install.packages('janitor')

library(doBy)
library(janitor)

demographic_data <-
  select(singleSourceOfTruthAppended,
         `Current Country of Residence`,
         Sex,
         age)
names(demographic_data)[names(demographic_data) == "Current Country of Residence"] <-
  "country"
names(demographic_data)[names(demographic_data) == "Sex"] <-
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

asd <- bind_cols(age_data, gender_data[2:3])

detach(demographic_data)