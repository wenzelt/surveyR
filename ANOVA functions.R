country_anova = select(
  singleSourceOfTruthAppended,
  `Current Country of Residence`,
  sebis_avg,
  R101,
  E201_01:E201_20
)




calc_anova <- function(data, column_to_use) {
  if (!is.null(data[[column_to_use]])) {
    data <- data[data[[column_to_use]] >= 0,]
  } else{
    print("Column does not Exist")
  }
  
  
  means <-
    round(tapply(as.numeric(data[[column_to_use]]),
                 data$`Current Country of Residence`,
                 mean),
          digits = 2)
  
  
  plotmeans(
    data[[column_to_use]] ~ data$`Current Country of Residence`,
    digits = 2,
    ccol = 'red',
    mean.labels = F,
    ylab = 'mean',
    main = sprintf("Plot of %s means by region", column_to_use),
  )
  
  boxplot(
    data[[column_to_use]] ~ data$`Current Country of Residence`,
    main = sprintf("Plot of %s means by region", column_to_use),
    xlab = "'region'",
    ylab = column_to_use,
    col = rainbow(7)
  )
  # F statistics = Variation among sample means / Variation within groups
  
  aov_content =  aov(data[[column_to_use]] ~ data$`Current Country of Residence`)
  if ((summary(aov_content)[[1]][["Pr(>F)"]]) < 0.1) {
    tuk = TukeyHSD(aov_content)
    
    print(summary(aov_content))
    print(tuk)
    plot(tuk)
  }
  
  
  
}
#calc_anova(country_anova, "E201_11")
#calc_anova(country_anova, "E201_14")
calc_anova(country_anova, "E201_16") # significant Smart TV
calc_anova(country_anova, "E201_19") # significant Smart Vacuum
calc_anova(country_anova, "E201_18") # significant Smart Toy


# 1	E201_01	Device risk: Smart Coffee Maker
# 2	E201_02	Device risk: Smart Dishwasher
# 3	E201_03	Device risk: Smart Door Lock
# 4	E201_04	Device risk: Smart Doorbell
# 5	E201_05	Device risk: Smart Electricity Meter
# 6	E201_06	Device risk: Smart Electrical Outlet
# 7	E201_07	Device risk: Smart Fridge
# 8	E201_08	Device risk: Smart Gardening Equipment
# 9	E201_09	Device risk: Smart Heating/Cooling System
# 10	E201_10	Device risk: Smart Home Monitoring System
# 11	E201_11	Device risk: Smart Lightbulb
# 12	E201_12	Device risk: Smart Oven
# 13	E201_13	Device risk: Smart Robot
# 14	E201_14	Device risk: Smart Speaker
# 15	E201_15	Device risk: Smart Stove
# 16	E201_16	Device risk: Smart TV
# 17	E201_17	Device risk: Smart Thermostat
# 18	E201_18	Device risk: Smart Toy
# 19	E201_19	Device risk: Smart Vacuum Cleaner
# 20	E201_20	Device risk: Smart Washing Machine