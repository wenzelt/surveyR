country_anova = select(
  ssot_new,
  `Current Country of Residence`,
  sebis_avg,
  R101,
  A204_01:A204_06,
  E201_01:E201_20,
  A307_01:A307_10,
)

#Definition ANOVA function -----

titles <- hash()

calc_anova <- function(data, column_to_use) {
  if (!is.null(data[[column_to_use]])) {
    data <- data[data[[column_to_use]] >= 0,]
  } else{
    print("Column does not Exist")
  }
  
  # pdf(sprintf("Plot_'%s'by_region.pdf", titles[[column_to_use]])) 
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
    main = sprintf("Plot of '%s' means by region", titles[[column_to_use]]),
  )
  
  boxplot(
    data[[column_to_use]] ~ data$`Current Country of Residence`,
    main = sprintf("Plot of '%s' means by region", titles[[column_to_use]]),
    xlab = "'region'",
    ylab = titles[[column_to_use]],
    col = rainbow(7)
  )
  
  
  
  # F statistics = Variation among sample means / Variation within groups
  
  aov_content =  aov(data[[column_to_use]] ~ data$`Current Country of Residence`)
  if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
    tuk = TukeyHSD(aov_content)
    
    print(summary(aov_content))
    print(tuk)
    plot(tuk)
    print(means)
    
    # dev.off() 
    #return(tuk)
  }
  print(means)
  
}

#E201 Device Risk ----
titles$E201_01 = "Smart Coffee Maker"
titles$E201_02 = "Smart Dishwasher"
titles$E201_03 = "Smart Door Lock"
titles$E201_04 = "Smart Doorbell"
titles$E201_05 = "Smart Electricity Meter"
titles$E201_06 = "Smart Electrical Outlet"
titles$E201_07 = "Smart Fridge"
titles$E201_08 = "Smart Gardening Equipment"
titles$E201_09 = "Smart Heating/Cooling System"
titles$E201_10 = "Smart Home Monitoring System"
titles$E201_11 = "Smart Lightbulb"
titles$E201_12 = "Smart Oven"
titles$E201_13 = "Smart Robot"
titles$E201_14 = "Smart Speaker"
titles$E201_15 = "Smart Stove"
titles$E201_16 = "Smart TV"
titles$E201_17 = "Smart Thermostat"
titles$E201_18 = "Smart Toy"
titles$E201_19 = "Smart Vacuum Cleaner"
titles$E201_20 = "Smart Washing Machine"

calc_anova(country_anova, "E201_11") # not significant Smart Lightbulb
calc_anova(country_anova, "E201_14") # not significant Smart Speaker
calc_anova(country_anova, "E201_16") # significant Smart TV
calc_anova(country_anova, "E201_18") # significant Smart Toy
calc_anova(country_anova, "E201_19") # significant Smart Vacuum


#A204 Perception of responsibility ----

titles$A204_01 = "Keeping the Smart Home device software up-to-date"
titles$A204_02 = "Ensuring my privacy"
titles$A204_03 = "Protecting my Smart Home ecosystem as a whole"
titles$A204_04 = "Keeping the Smart Home device secure"
titles$A204_05 = "Fixing a hardware failure"
titles$A204_06 = "Fixing a software failure"

calc_anova(country_anova, 'A204_04')
calc_anova(country_anova, 'A204_05')

no_users = subset(country_anova, R101==0)
calc_anova(no_users, 'A204_04')


country_anova$risk_avg = rowMeans(select(country_anova,A204_01:A204_06))
calc_anova(country_anova, 'risk_avg')

#A307 Perceived Feature benefit ----

titles$A307_01 = "Saving money"
titles$A307_02 = "Saving energy"
titles$A307_03 = "Increasing convenience"
titles$A307_04 = "Enhancing leisure activities"
titles$A307_05 = "Providing peace of mind"
titles$A307_06 = "Providing comfort"
titles$A307_07 = "Increasing safety"
titles$A307_08 = "Providing care"
titles$A307_09 = "Improving quality of life"
titles$A307_10 = "Increasing property value"

calc_anova(country_anova,'A307_04')
calc_anova(country_anova,'A307_05')
calc_anova(country_anova,'A307_06')
calc_anova(country_anova,'A307_07')
calc_anova(country_anova,'A307_08')
calc_anova(country_anova,'A307_10')


calc_anova(country_anova,'R101')

