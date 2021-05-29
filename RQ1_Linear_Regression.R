la_regression = select(
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
calc_lr <- function(data) {
  devices.lm <- lm(formula = data$LA_Mean ~ data$R101 + age + Sex,
                   data = data)
  devices$value <- devices.lm$fitted.values
  head(devices)
  
  
  plot(
    x = devices.lm$model$R101,
    # True values on x-axis
    y = devices.lm$fitted.values,
    # fitted values on y-axis
    xlab = "True Values",
    ylab = "Model Fitted Values",
    main = "Regression fits of diamond values"
  )

  abline(b = 1, a = 0)
  summary(devices.lm)
}
calc_lr(la_regression)

