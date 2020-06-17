

########################## RQ_02 ###################################################
# RQ2: How does the cultural context impact Smart Home device adoption and use?
# H1: The purchasing trends of buying a Smart Home device differs internationally.
# H2: The cultural background of participants affects the usage of Smart Home devices.
# H3: The perception towards Smart Home devices differs internationally.



## H1 ####

# 1 R216_01	Education about Device D1: Online reviews
# 2	R216_02	Education about Device D1: Online forums
# 3	R216_03	Education about Device D1: Print media (e.g., Newspapers, Magazines, etc.)
# 4	R216_04	Education about Device D1: Friends and Family
# 5	R216_05	Education about Device D1: Online news sites

singleSourceOfTruthAppended$`Current Country of Residence` <-
  as.factor(singleSourceOfTruthAppended$`Current Country of Residence`)

## testing dependencies between current country of residence and consulting online reviews for their smart devices
chisq.test(R216_01,`Current Country of Residence`)
chisq.test(R216_02,`Current Country of Residence`)
chisq.test(R216_03,`Current Country of Residence`)
chisq.test(R216_04,`Current Country of Residence`)
chisq.test(R216_05,`Current Country of Residence`)

# 1	HP02_01	Market tools: Low prices
# 2	HP02_02	Market tools: Bundled offers (e.g., including other devices with purchase of one or more devices)
# 3	HP02_03	Market tools: Trials (e.g., 30-day free use of a service)
# 4	HP02_04	Market tools: Periodic sales
# 5	HP02_05	Market tools: Discounts (e.g., coupons)

# testing for dependencies in country of residence in purchase was influenced by:

kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_01 ~ `Current Country of Residence`)#n.s
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_02 ~ `Current Country of Residence`)#s p = 0.0127
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_03 ~ `Current Country of Residence`)#s P = 0.0103
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_04 ~ `Current Country of Residence`)#s p = 0.0000000901
kruskal_test(singleSourceOfTruthAppended,
             formula = HP02_05 ~ `Current Country of Residence`)#n.s


##H2####
u <-
  select(
    singleSourceOfTruthAppended,
    participant_id,
    `Current Country of Residence`,
    R232_01,
    R232_02,
    R232_03,
    R233_01,
    R233_02 ,
    R233_03 ,
    R501,
    R503,
    R505
  )

d1 <-
  select(subset(u, R233_01 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_01,
         R501)
d2 <-
  select(subset(u, R233_02 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_02,
         R503)
d3 <-
  select(subset(u, R233_03 == 1),
         participant_id,
         `Current Country of Residence`,
         R232_03,
         R505)
colnames(d1) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage")
colnames(d2) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage")
colnames(d3) <-
  c("participant_id",
    "Current Country of Residence",
    "Device",
    "Usage")
d <- rbind(d1, d2, d3)
d <- subset(d, Usage != "Don't know")
d$Usage <-
  factor(
    d$Usage,
    levels = c(
      "0 times",
      "1-5 times",
      "6-10 times",
      "11-20 times",
      "21-30 times",
      "30+ times"
    )
  )
#usage and current country of residence shows no connection
kruskal_test(d, formula = Usage ~ `Current Country of Residence`) #ns no effect on usage by region could be measured
kruskal_test(subset(d, Device == "Smart TV"),
             formula = Usage ~ `Current Country of Residence`)
kruskal_test(subset(d, Device == "Smart Speaker"),
             formula = Usage ~ `Current Country of Residence`) #ns no effect on usage by region could be measured
kruskal_test(subset(d, Device == "Smart Lightbulb"),
             formula = Usage ~ `Current Country of Residence`) #ns no effect on usage by region could be measured
kruskal_test(
  subset(
    d,
    Device != "Smart Lightbulb" &&
      Device != "Smart Speaker" &&
      Device != "Smart TV"
  ),
  formula = Usage ~ `Current Country of Residence`
) #ns no effect on usage by region could be measured


##
smartTVUsers <- subset(d, Device == "Smart TV")
DT = dunnTest(x=as.numeric(smartTVUsers$Usage),g=smartTVUsers$`Current Country of Residence`, method = "bonferroni")
epsilonSquared(x=as.numeric(smartTVUsers$Usage),g=smartTVUsers$`Current Country of Residence`)
#compact letter display: 
PT = DT$res
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

kruskal_test(
  subset(
    smartTVUsers,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "United States"
  ),
  Usage ~ `Current Country of Residence`
)#ns

kruskal_test(
  subset(
    smartTVUsers,
    `Current Country of Residence` == "DACH" |
      `Current Country of Residence` == "United States"
  ),
  Usage ~ `Current Country of Residence`
)#s


kruskal_test(
  subset(
    smartTVUsers,
    `Current Country of Residence` == "United Kingdom" |
      `Current Country of Residence` == "DACH"
  ),
  Usage ~ `Current Country of Residence`
) # s

##
sTV_UK <- subset(smartTVUsers,
                 `Current Country of Residence` == "United Kingdom")##
sTV_US <- subset(smartTVUsers,
                 `Current Country of Residence` == "United States")
sTV_DACH <- subset(smartTVUsers,
                   `Current Country of Residence` == "DACH")
summary(sTV_US)
summary(sTV_UK)
summary(sTV_DACH)

mean(as.factor(sTV_DACH$Usage))

#disabled features and residence
disabled_features_country <-
  select(
    singleSourceOfTruthAppended,
    participant_id,
    R507,
    R510,
    R513,
    `Current Country of Residence`
  )
disabled_features_country$choice <-
  ifelse(disabled_features_country$R507 == "Yes" |
           R510 == "Yes" | R513 == "Yes",
         1,
         0)
chisq.test(
  disabled_features_country$`Current Country of Residence`,
  disabled_features_country$choice
) #ns no effect on usage by region could be measured
kruskal.test(
  disabled_features_country$`Current Country of Residence`,
  disabled_features_country$choice
) #ns no effect on usage by region could be measured


#usage type by current country of residence


##H3####
#The perception towards Smart Home devices differs internationally.

# 1 A204_01	Manufacturer responsibilitiy: Keeping the Smart Home device software up-to-date
# 2	A204_02	Manufacturer responsibilitiy: Ensuring my privacy
# 3	A204_03	Manufacturer responsibilitiy: Protecting my Smart Home ecosystem as a whole
# 4	A204_04	Manufacturer responsibilitiy: Keeping the Smart Home device secure
# 5	A204_05	Manufacturer responsibilitiy: Fixing a hardware failure
# 6	A204_06	Manufacturer responsibilitiy: Fixing a software failure

#####
p <- c(
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A204_01 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A204_02 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A204_03 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A204_04 ~ `Current Country of Residence`
  )[5],
  #p-adj: 0.01362 #dach - us / us - uk  / dach - uk correct p values for pairwise testing
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A204_05 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A204_06 ~ `Current Country of Residence`
  )[5]
)
p.adjust(p, method = "bonferroni", n = length(p))
#1.00000 1.00000 0.26160 0.01362 0.38100 1.00000

#pairwise testing for A204_04
#starting pairwise testing per country

dunnTest(A204_04, `Current Country of Residence`, method = "bonferroni")


countryPerception = select(singleSourceOfTruthAppended,
                           `Current Country of Residence`,
                           A204_04)
aggregate(countryPerception[, 2],
          list(countryPerception$`Current Country of Residence`),
          mean)
# Group.1  A204_04
# 1           DACH 3.940741
# 2 United Kingdom 4.535484
# 3  United States 4.675862




# connection between country of residence and smart home device preferences

# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value


p <- c(
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_01 ~ `Current Country of Residence`
  )[5],
  # 0.272
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_02 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_03 ~ `Current Country of Residence`
  )[5],
  #0.508 # not stat sig
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_04 ~ `Current Country of Residence`
  )[5],
  #0.0268
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_05 ~ `Current Country of Residence`
  )[5],
  #0.125
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_06 ~ `Current Country of Residence`
  )[5],
  #0.00141 -- adding pairwise testing -- p.adjust(p, method = "bonferroni", n = length(p))
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_07 ~ `Current Country of Residence`
  )[5],
  #0.0867
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_08 ~ `Current Country of Residence`
  )[5],
  #0.00615
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_09 ~ `Current Country of Residence`
  )[5],
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = A307_10 ~ `Current Country of Residence`
  )[5]
) #0.000274
p <- unlist(p, use.names = FALSE)
p.adjust(p, method = "bonferroni", n = length(p))
# 1       2        3        4        5        6        7        8        9        10
#0.272000 1.000000 0.508000 0.026800 0.125000 0.001410 0.086700 0.006150 1.000000 0.000274

# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value


#-------------------------------------------------------------------------------
#testing for smart home device preference country ~ enhancing leisure activities
dunnTest(A307_04, `Current Country of Residence`, method = "bonferroni")

countryIncreaseProperty = select(singleSourceOfTruthAppended,
                                 `Current Country of Residence`,
                                 A307_04)
aggregate(
  countryIncreaseProperty[, 2],
  list(countryIncreaseProperty$`Current Country of Residence`),
  mean
)
# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value

#-------------------------------------------------------------------------------
#testing for smart home device preference country ~ providing comfort


dunnTest(A307_06, `Current Country of Residence`, method = "bonferroni")
test = subset(
  singleSourceOfTruthAppended,
  `Current Country of Residence` == "DACH" |
    `Current Country of Residence` == "United Kingdom"
)
epsilonSquared(test$A307_06, test$`Current Country of Residence`)
freemanTheta(test$A307_06, test$`Current Country of Residence`)




countryIncreaseProperty = select(singleSourceOfTruthAppended,
                                 `Current Country of Residence`,
                                 A307_06)
aggregate(
  countryIncreaseProperty[, 2],
  list(countryIncreaseProperty$`Current Country of Residence`),
  mean
)

# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value

#------------------------------------------------------------------------------------
#testing for smart home device preference country ~ increasing safety

dunnTest(A307_07, `Current Country of Residence`, method = "bonferroni")

countryIncreaseProperty = select(singleSourceOfTruthAppended,
                                 `Current Country of Residence`,
                                 A307_07)
aggregate(
  countryIncreaseProperty[, 2],
  list(countryIncreaseProperty$`Current Country of Residence`),
  mean
)

# 1	A307_01	Perceived benefits: Saving money
# 2	A307_02	Perceived benefits: Saving energy
# 3	A307_03	Perceived benefits: Increasing convenience
# 4	A307_04	Perceived benefits: Enhancing leisure activities
# 5	A307_05	Perceived benefits: Providing peace of mind
# 6	A307_06	Perceived benefits: Providing comfort
# 7	A307_07	Perceived benefits: Increasing safety
# 8	A307_08	Perceived benefits: Providing care
# 9	A307_09	Perceived benefits: Improving quality of life
# 10	A307_10	Perceived benefits: Increasing property value


#------------------------------------------------------------------------------------
#testing for smart home device preference country ~ providing care



dunnTest(A307_08, `Current Country of Residence`, method = "bonferroni")
countryIncreaseProperty = select(singleSourceOfTruthAppended,
                                 `Current Country of Residence`,
                                 A307_08)
aggregate(
  countryIncreaseProperty[, 2],
  list(countryIncreaseProperty$`Current Country of Residence`),
  mean
)

#------------------------------------------------------------------------------------

#starting pairwise testing per country
# Country and adding to the property value


dunnTest(A307_10, `Current Country of Residence`, method = "bonferroni")
countryIncreaseProperty = select(singleSourceOfTruthAppended,
                                 `Current Country of Residence`,
                                 A307_10)
aggregate(
  countryIncreaseProperty[, 2],
  list(countryIncreaseProperty$`Current Country of Residence`),
  mean
)

#-------------------------------------------------------------------------------
# testing for country by perceived device risk

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

p <- c((
  kruskal_test(
    singleSourceOfTruthAppended,
    formula = E201_11 ~ `Current Country of Residence`
  )[5]
),
# smart lights
kruskal_test(
  singleSourceOfTruthAppended,
  formula = E201_14 ~ `Current Country of Residence`
)[5],
# smart speaker
kruskal_test(
  singleSourceOfTruthAppended,
  formula = E201_16 ~ `Current Country of Residence`
)[5]
)# smart TV - significantly different for countries p = 0.0000555
# --- plot means by country to find out which is different and higher / lower
p.adjust(p, "bonferroni") #1.0000000 0.2760000 0.0001665

dunnTest(E201_16, `Current Country of Residence`, method = "bonferroni")

#####Country mal A307_04
test = select(singleSourceOfTruthAppended, A307_04,`Current Country of Residence`)
test$`Current Country of Residence` = factor(test$`Current Country of Residence`,levels = unique(test$`Current Country of Residence`))
test$A307_04.f = factor(test$A307_04, ordered = T)

str(test)
summary(test)
hist(~ A307_04.f | `Current Country of Residence`,
     data=test,
     layout=c(1,3))
kruskal.test(A307_04.f~`Current Country of Residence`, data= test)
epsilonSquared(x = test$A307_04, g=test$`Current Country of Residence`)
freemanTheta(x = test$A307_04.f, g=test$`Current Country of Residence`)
