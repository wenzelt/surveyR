
########################## RQ_03 ###################################################

##H1####
#plotting sex against amount of devices (purely out of interest)
ggboxplot(
  singleSourceOfTruthAppended,
  x = "Sex",
  y = "R101",
  color = "Sex",
  palette = c("#00AFBB", "#E7B800"),
  ylab = "Amount of devices",
  xlab = "Sex"
)


# testing for amount of devices per property ownership / renting a property

ggboxplot(
  singleSourceOfTruthAppended,
  x = "A007",
  y = "R101",
  color = "A007",
  palette = c("#00AFBB", "#E7B800", "#E7F800"),
  ylab = "Amount of devices",
  xlab = "Renting or owning"
)

wilcox.test(R101 ~ A007 == "Rent" |
              A007 == "Own") # no statistical significance found
kruskal_test(singleSourceOfTruthAppended, formula = R101 ~ A007) # 0.0701

# testing for amount of children in household

ggboxplot(
  singleSourceOfTruthAppended,
  x = "A004",
  y = "R101",
  color = "A004",
  palette = c("#00AFBB", "#E7B800"),
  ylab = "Amount of devices",
  xlab = "Children or no children"
)
##H2####



##H3####
##testing wilcox test for children > 0 impact on amount of devices##
singleSourceOfTruthAppended$A004 <-
  cut(singleSourceOfTruthAppended$A004, breaks = c(0, 1, Inf)) ## adding levels to children
singleSourceOfTruthAppended$A004 <-
  as.factor(singleSourceOfTruthAppended$A004)


#plotting having children and not having children against eachother
ggboxplot(
  singleSourceOfTruthAppended,
  x = "A004",
  y = "R101",
  color = "A004",
  palette = c("#00AFBB", "#E7B800"),
  ylab = "Amount of devices",
  xlab = "Children or no children"
)

wilcox.test(R101, na.omit(A004))##mann whitney u test #not significant

wilcox_test(singleSourceOfTruthAppended, R101 ~ A004) #p = 0.888

#testing for children affecting the type of usage the user is comfortable with

# 1 E205_01	Usage type: Voice commands via a Smart Speaker
# 2	E205_02	Usage type: Voice commands via a Smartphone Voice Assistant
# 3	E205_03	Usage type: Smartphone App for the Device
# 4	E205_04	Usage type: Smartphone Widgets or Shortcuts
# 5	E205_05	Usage type: Sensors inside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 6	E205_06	Usage type: Sensors outside the Home (e.g., Motion Sensors, Light Sensors, etc.)
# 7	E205_07	Usage type: Automatic Operation based on Device Programming

wilcox_test(singleSourceOfTruthAppended, E205_01 ~ A004) #ns
wilcox_test(singleSourceOfTruthAppended, E205_02 ~ A004) #ns
wilcox_test(singleSourceOfTruthAppended, E205_05 ~ A004) #ns
wilcox_test(singleSourceOfTruthAppended, E205_06 ~ A004) #ns


dunnTest(E205_06, as.factor(A004), method = "bonferroni")

wilcox_test(singleSourceOfTruthAppended, formula = E201_11 ~ A004) # 0.0701
wilcox_test(singleSourceOfTruthAppended, formula = E201_14 ~ A004) # 0.0701
wilcox_test(singleSourceOfTruthAppended, formula = E201_16 ~ A004) # 0.0701

wilcox_test(singleSourceOfTruthAppended, formula = A204_01 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_02 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_03 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_04 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_05 ~ A004)
wilcox_test(singleSourceOfTruthAppended, formula = A204_06 ~ A004)

singleSourceOfTruthAppended$A005 <-
  cut(as.numeric(singleSourceOfTruthAppended$A005),
      breaks = c(0, 1, Inf)) ## adding levels to children
singleSourceOfTruthAppended$A005 <-
  as.factor(singleSourceOfTruthAppended$A005)
wilcox_test(singleSourceOfTruthAppended, R101 ~ A005)


wilcox_test(singleSourceOfTruthAppended, formula = E201_11 ~ A005) # 0.0701
wilcox_test(singleSourceOfTruthAppended, formula = E201_14 ~ A005) # 0.0701
wilcox_test(singleSourceOfTruthAppended, formula = E201_16 ~ A005)
