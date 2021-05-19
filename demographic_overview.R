# Demographic overview of the sample. Please run 'install_dependencies.R first'


demographic_data <-
  select(singleSourceOfTruthAppended,
         `Current Country of Residence`,
         Sex,
         age)
names(demographic_data)[names(demographic_data) == "Current Country of Residence"] <-
  "country"

#install.packages('doBy')
library(doBy)
#three people preferred not to enter their age
summaryBy(
  age ~ country,
  data = demographic_data,
  FUN = c(median, mean, sd, min, max),
  na.rm = TRUE
)
