#E201 Device Risk ----

set_device_titles <- function() {
  # defines names of values for easier read- and plotability
  titles <- hash()

  titles$R101 = "Unique Devices in the Home"

  titles$`Current Country of Residence` = "Region"

  titles$muipc_PerceivedSur_avg = '[MUIPC] Perceived Surveillance avg'

  titles$A005 = "Household size"

  titles$LA_Mean = "Perception of Regulation"

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

  #A204 Perception of responsibility ----

  titles$A204_01 = "Keeping the Smart Home device software up-to-date"
  titles$A204_02 = "Ensuring my privacy"
  titles$A204_03 = "Protecting my Smart Home ecosystem as a whole"
  titles$A204_04 = "Keeping the Smart Home device secure"
  titles$A204_05 = "Fixing a hardware failure"
  titles$A204_06 = "Fixing a software failure"


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
}

set_device_titles()

data = select(
  singleSourceOfTruthAppended,
  LA_Mean,
  age,
  Sex,
  sebis_avg,
  R101,
  A204_01:A204_06,
  E201_01:E201_20,
  A307_01:A307_10,
  `Current Country of Residence`,
  muipc_PerceivedSur_avg,
  R101,
  A005,
  LA01_01:LA01_03
)


#Definition LR function -----
calc_lr <- function(d) {
  # pdf(sprintf("Plot_LA_MEAN_%s.pdf", titles[[names(d[2])]])) #Optional PDF output

  print(sprintf("Running Regression on %s and '%s'", names(d[1]), titles[[names(d[2])]]))
  d[, 2][d[, 2] < 0] = NA #remove negative values
  lmtemp = lm(d[[1]] ~ d[[2]] + as.character(d$Sex) + I(d$age), data = d)
  # Creates a linear model with the first two columns
  # of data passed moderated for age and gender

  plot(
    lmtemp$residuals,
    pch = 16,
    col = "red",
    main = sprintf("'%s'~ '%s' LR residuals", titles[[names(d[1])]], titles[[names(d[2])]]),
  ) #Plots the residuals of the linear model created from the first to columns

  plot(
    d[1:2],
    ylab = sprintf("'%s' risk assessment", titles[[names(d[2])]]),
    xlab = sprintf("'%s' regulatory perception", titles[[names(d[1])]]),
    col = 'blue',
    main = sprintf("'%s'~ '%s' LR values", titles[[names(d[1])]], titles[[names(d[2])]]),
  ) #Plots the actual values of the first two columns passed to the function in blue

  abline(lmtemp)
  print(summary(lmtemp)) #summary linear model
  # dev.off()
}


calc_lr(select(data, LA_Mean, E201_11, Sex, age)) # Device Risk: Smart Lightbulb
calc_lr(select(data, LA_Mean, E201_14, Sex, age)) # Device Risk: Smart Speaker
calc_lr(select(data, LA_Mean, E201_16, Sex, age)) # Device Risk: Smart TV

calc_lr(select(data, LA_Mean, muipc_PerceivedSur_avg, Sex, age))

# Device Adoption + LA_MEAN
calc_lr(select(data, LA_Mean, R101, Sex, age))

calc_lr(select(Participants_DACH, LA_Mean, R101, Sex, age))
calc_lr(select(Participants_UK, LA_Mean, R101, Sex, age))
calc_lr(select(Participants_US, LA_Mean, R101, Sex, age))

calc_lr(select(data, A005, R101, Sex, age))


knitr::stitch_rhtml('RQ1_Linear_Regression.r')
knitr::stitch('RQ1_Linear_Regression.r')
stitch_rhtml('RQ1_Linear_Regression.R')
stitch("RQ1_LR_REPORT.Rmd")
rmarkdown::render("RQ1_LR_REPORT.Rmd")
rmarkdown::render("RQ1_Linear_Regression.R", "html")
