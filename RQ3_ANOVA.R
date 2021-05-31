house_anova = select(singleSourceOfTruthAppended, A007, R101, A004)

calc_anova_house <- function(d) {
  # pdf(sprintf("Plot_LA_MEAN_%s.pdf", titles[[names(d[2])]]))
  
  print(sprintf("Running ANOVA on %s and '%s'", names(d[1]), titles[[names(d[2])]]))
  d[, 2][d[, 2] < 0] = NA #remove negative values
  
  
  means <-
    round(tapply(as.numeric(d[[2]]),
                 d[[1]],
                 mean),
          digits = 2)
  
  plotmeans(
    d[[2]] ~ d[[1]],
    digits = 2,
    ccol = 'red',
    mean.labels = F,
    ylab = 'mean',
  )
  
  boxplot(
    d[[2]] ~ d[[1]],
    xlab = d[[1]],
    ylab = names(d[[2]]),
    col = rainbow(7)
  )
  
  aov_content =  aov(d[[2]] ~ d[[1]])
  if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
    tuk = TukeyHSD(aov_content)
    
    print(summary(aov_content))
    print(tuk)
    plot(tuk)
    print(means)
    
    # dev.off()
  }
}

calc_anova_house(select(house_anova,A007,R101))
calc_anova_house(select(house_anova,A004,R101))
