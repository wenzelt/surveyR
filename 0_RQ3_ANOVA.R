house_anova = select(ssot_new, A007, R101, A004)
#house_anova = subset(house_anova, A007 != 3)

asd = select(ssot_new,A007,A204_01,A204_02,A204_03,A204_04,A204_05,A204_06)
#asd = subset(asd, A007 != 3)
aov_content = aov(as.numeric(asd$A204_01) ~ asd$A007 )
print(summary(aov_content))
if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
  print("significant")
}
aov_content = aov(as.numeric(asd$A204_02) ~ asd$A007 )
print(summary(aov_content))
if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
  print("significant")
}
aov_content = aov(as.numeric(asd$A204_03) ~ asd$A007 )
print(summary(aov_content))
if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
  print("significant")
}
aov_content = aov(as.numeric(asd$A204_04) ~ asd$A007 )
print(summary(aov_content))
if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
  print("significant")
}


aov_content = aov(as.numeric(asd$A204_05) ~ as.factor(asd$A007) )
print(summary(aov_content))
if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
  print("significant")
  tuk = TukeyHSD(aov_content)
  
  print(summary(aov_content))
  print(tuk)
}



aov_content = aov(as.numeric(asd$A204_06) ~  as.factor(asd$A007 ))
print(summary(aov_content))
if ((summary(aov_content)[[1]][["Pr(>F)"]])[1] < 0.1) {
  print("significant")
}



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
  
  aov_content =  aov(d[[2]] ~ as.factor(d[[1]]))
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

aov(ssot_new$A007 ~ ssot_new$R101)

