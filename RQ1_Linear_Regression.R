

#E201 Device Risk ----
set_device_titles <- function() {
  titles <- hash()
  
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
  `Current Country of Residence`
)


#Definition LR function -----
calc_lr <- function(d) {
  d = d[1:2]
  d[, 2][d[, 2] < 0] = NA #remove negative values
  lmtemp = lm(d[[1]] ~ d[[2]] + data$age , data = d)
  plot(
    lmtemp$residuals,
    pch = 16,
    col = "red",
    main = sprintf("'%s'~ '%s' LR residuals", names(d[1]), titles[[names(d[2])]]),
  )
  plot(d,
       col = 'blue',
       main = sprintf("'%s'~ '%s' LR values", names(d[1]), titles[[names(d[2])]]),
  )
  abline(lmtemp)
  print(summary(lmtemp))
  
}



calc_lr(select(data, LA_Mean, E201_11))
calc_lr(select(data, LA_Mean, E201_14))
calc_lr(select(data, LA_Mean, E201_16))
