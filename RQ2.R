

########################## RQ_02 ###################################################
# RQ2: How does the cultural context impact Smart Home device adoption and use?
# H1: The purchasing trends of buying a Smart Home device differs internationally.
# H2: The cultural background of participants affects the usage of Smart Home devices.
# H3: The perception towards Smart Home devices differs internationally.


## H1 #### -- not used in paper as we decided purchasing is not part of our initial investigation
attach(singleSourceOfTruthAppended)
# 1 R216_01	Education about Device D1: Online reviews
# 2	R216_02	Education about Device D1: Online forums
# 3	R216_03	Education about Device D1: Print media (e.g., Newspapers, Magazines, etc.)
# 4	R216_04	Education about Device D1: Friends and Family
# 5	R216_05	Education about Device D1: Online news sites

singleSourceOfTruthAppended$`Current Country of Residence` <-
  as.factor(singleSourceOfTruthAppended$`Current Country of Residence`)

## testing dependencies between current country of residence and consulting online reviews for their smart devices
chisq.test(R216_01, `Current Country of Residence`)
chisq.test(R216_02, `Current Country of Residence`)
chisq.test(R216_03, `Current Country of Residence`)
chisq.test(R216_04, `Current Country of Residence`)
chisq.test(R216_05, `Current Country of Residence`)

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

#How does legislation affect the adoption of different devices over different regions 
cor.test(Participants_DACH$LA_Mean,
         Participants_DACH$R101) #*
cor.test(Participants_UK$LA_Mean,
         Participants_UK$R101)
cor.test(Participants_US$LA_Mean,
         Participants_US$R101)

kruskal_test(singleSourceOfTruthAppended,
             formula = R101 ~ `Current Country of Residence`)#n.s




##H2#### - usage affected by region?

# Adding all device owners to the same data frame and stacking them for analysis
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

#testing for connection between current region of residence and the amount of usaage over ALL smart home devices
Usage_CCR_LATEX <-
  dunnTest(
    x = as.numeric(d$Usage),
    g = as.factor(d$`Current Country of Residence`),
    method = "bonferroni"
  )$res
Usage_CCR_LATEX <- Usage_CCR_LATEX[-c(3)]

view(Usage_CCR_LATEX)
aggregate(as.numeric(d$Usage),
          list(d$`Current Country of Residence`),
          mean)
# we find that there is no significant connection between the region of residence
# and the amount of usage of smart home devices overall



# we investigate for specific high favourability devices
Usage_CCR_LATEX_SMART_TV <-
  dunnTest(
    x = as.numeric(subset(d, Device == "Smart TV")$Usage),
    g = as.factor(subset(d, Device == "Smart TV")$`Current Country of Residence`),
    method = "bonferroni"
  )$res
Usage_CCR_LATEX_SMART_TV <- Usage_CCR_LATEX_SMART_TV[-c(3)]
view(Usage_CCR_LATEX_SMART_TV)
aggregate(as.numeric(subset(d, Device == "Smart TV")$Usage), list(as.factor(
  subset(d, Device == "Smart TV")$`Current Country of Residence`
)), mean)

# we find that there is a significant difference in usage between DE-UK and DE - US

#---

#we investigate the same for the smart speaker
Usage_CCR_LATEX_SMART_SPEAKER <-
  dunnTest(
    x = as.numeric(subset(d, Device == "Smart Speaker")$Usage),
    g = as.factor(
      subset(d, Device == "Smart Speaker")$`Current Country of Residence`
    ),
    method = "bonferroni"
  )$res

Usage_CCR_LATEX_SMART_SPEAKER <-
  Usage_CCR_LATEX_SMART_SPEAKER[-c(3)]

aggregate(as.numeric(subset(d, Device == "Smart Speaker")$Usage), list(as.factor(
  subset(d, Device == "Smart Speaker")$`Current Country of Residence`
)), mean)
#we find that there is no significant difference between the participants from different regions in smart speaker usage

#---

#we investigate the smart lightbulb
Usage_CCR_LATEX_SMART_LIGHTBULB <-
  dunnTest(
    x = as.numeric(subset(d, Device == "Smart Lightbulb")$Usage),
    g = as.factor(
      subset(d, Device == "Smart Lightbulb")$`Current Country of Residence`
    ),
    method = "bonferroni"
  )$res

Usage_CCR_LATEX_SMART_LIGHTBULB <-
  Usage_CCR_LATEX_SMART_LIGHTBULB[-c(3)]

aggregate(as.numeric(subset(d, Device == "Smart Lightbulb")$Usage), list(as.factor(
  subset(d, Device == "Smart Lightbulb")$`Current Country of Residence`
)), mean)
#we find that there is no sgnificant difference in smart lightbulb use across different regions

# we investigate all other devices
Usage_CCR_LATEX_OTHER <-
  dunnTest(
    x = as.numeric(
      subset(
        d,
        Device != "Smart Lightbulb" &&
          Device != "Smart Speaker" && Device != "Smart TV"
      )$Usage
    ),
    g = as.factor(
      subset(
        d,
        Device != "Smart Lightbulb" &&
          Device != "Smart Speaker" &&
          Device != "Smart TV"
      )$`Current Country of Residence`
    ),
    method = "bonferroni"
  )$res

Usage_CCR_LATEX_OTHER <- Usage_CCR_LATEX_OTHER[-c(3)]
# we find that the other devices do not have a significant change in use because of the region of residence

#---

# we investigate further into smart TV users
smartTVUsers <- subset(d, Device == "Smart TV")
epsilonSquared(x = as.numeric(smartTVUsers$Usage),
               g = smartTVUsers$`Current Country of Residence`)
# we determine the effect size
sTV_UK <- subset(smartTVUsers,
                 `Current Country of Residence` == "United Kingdom")##
sTV_US <- subset(smartTVUsers,
                 `Current Country of Residence` == "United States")
sTV_DACH <- subset(smartTVUsers,
                   `Current Country of Residence` == "DACH")
summary(sTV_US)
summary(sTV_UK)
summary(sTV_DACH)

#disabled features and residence -- are not using this due to small sample size
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
  ifelse(
    disabled_features_country$R507 == "Yes" |
      disabled_features_country$R510 == "Yes" |
      disabled_features_country$R513 == "Yes",
    1,
    0
  )

test <-
  merge(disabled_features_country,
        rbind(sTV_DACH, sTV_UK, sTV_US),
        by = "participant_id")
chisq.test(test$`Current Country of Residence.y`,
           test$choice)


DISABLED_FEATURES_COUNTRY_LATEX_CHI <-
  chisq.test(
    disabled_features_country$`Current Country of Residence`,
    disabled_features_country$choice
  )
DISABLED_FEATURES_COUNTRY_LATEX_CHI <-
  data.frame(
    cbind(
      DISABLED_FEATURES_COUNTRY_LATEX_CHI$p.value,
      DISABLED_FEATURES_COUNTRY_LATEX_CHI$method
    )
  )
names(DISABLED_FEATURES_COUNTRY_LATEX_CHI) <- c("p", "X^2")
#ns no effect on usage by region could be measured

#---




##H3####
#The perception towards Smart Home devices differs internationally.

# 1 A204_01	Manufacturer responsibilitiy: Keeping the Smart Home device software up-to-date
# 2	A204_02	Manufacturer responsibilitiy: Ensuring my privacy
# 3	A204_03	Manufacturer responsibilitiy: Protecting my Smart Home ecosystem as a whole
# 4	A204_04	Manufacturer responsibilitiy: Keeping the Smart Home device secure
# 5	A204_05	Manufacturer responsibilitiy: Fixing a hardware failure
# 6	A204_06	Manufacturer responsibilitiy: Fixing a software failure
##
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

# we investigate how the regions perception change in regard to manufacturer
# responsibility of keeping the smart home device secure as a whole

dunn_A204_04 <-
  dunnTest(singleSourceOfTruthAppended$A204_04,
           `Current Country of Residence`,
           method = "bonferroni")
eps1 <-
  epsilonSquared(
    x = as.numeric(
      subset(
        singleSourceOfTruthAppended,
        `Current Country of Residence` == "DACH" |
          `Current Country of Residence` == "United Kingdom"
      )$A204_04
    ),
    g = subset(
      singleSourceOfTruthAppended,
      `Current Country of Residence` == "DACH" |
        `Current Country of Residence` == "United Kingdom"
    )$`Current Country of Residence`
  )
eps2 <-
  epsilonSquared(
    x = as.numeric(
      subset(
        singleSourceOfTruthAppended,
        `Current Country of Residence` == "DACH" |
          `Current Country of Residence` == "United States"
      )$A204_04
    ),
    g = subset(
      singleSourceOfTruthAppended,
      `Current Country of Residence` == "DACH" |
        `Current Country of Residence` == "United States"
    )$`Current Country of Residence`
  )

epsilonSquared <- c(eps1, eps2, "NA")
dunn_A204_04_LATEX <- cbind(dunn_A204_04$res, epsilonSquared)
dunn_A204_04_LATEX <- dunn_A204_04_LATEX[-c(3)]


countryPerception = select(singleSourceOfTruthAppended,
                           `Current Country of Residence`,
                           A204_04)
aggregate(countryPerception[, 2],
          list(countryPerception$`Current Country of Residence`),
          mean)

#check on non-users
countryPerception_N = select(subset(singleSourceOfTruthAppended, R101 < 1),
                           `Current Country of Residence`,
                           A204_04)
aggregate(countryPerception_N[, 2],
          list(countryPerception_N$`Current Country of Residence`),
          mean)

#check users
countryPerception_U = select(subset(singleSourceOfTruthAppended, R101 > 0),
                           `Current Country of Residence`,
                           A204_04)
aggregate(countryPerception_U[, 2],
          list(countryPerception_U$`Current Country of Residence`),
          mean)
# We find that germany sees smart home device security to lie more in their individual hands
# whereas the english speaking regions see it more in the manufacturers hands

#---


library(plyr)
mu <-
  ddply(countryPerception,
        "`Current Country of Residence`",
        summarise,
        grp.mean = mean(A204_04))


p <-
  ggplot(
    countryPerception,
    aes(
      x = countryPerception$A204_04,
      color = countryPerception$`Current Country of Residence`
    )
  ) +
  geom_density() + geom_vline(
    data = mu,
    aes(xintercept = grp.mean, color = `Current Country of Residence`),
    linetype = "dashed"
  ) + scale_color_brewer(palette = "Dark2") + labs(title = "Manufacturer responsibility - keeping smart home device secure",
                                                   x =
                                                     "Likert Scale from myself vs. Manufacturer", y = "density") + theme(legend.title = element_blank())
# barplot, show the whole scale, germans want to take more responsibility or no trust in responsibility. Protected by GDPR , different regions call to different conclusions, test for a206 global change, in all three regions there is a desire to place responsibility on the manufacturer
# english speaking countries nudged into Manufacturer domain
# No real trends add mean and
p

a <-
  ggplot(
    countryPerception,
    aes(x = A204_04, color = `Current Country of Residence`, fill = `Current Country of Residence`)
  ) +
  geom_histogram(aes(y = ..density..), alpha = 0.5,
                 position = "identity") +
  geom_density(alpha = .2)
a
# Group.1  A204_04
# 1           DACH 3.940741
# 2 United Kingdom 4.535484
# 3  United States 4.675862

# we are investigating the benefits of smart home device perceived benefits and if they change by country

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

increaseLeisure = select(singleSourceOfTruthAppended,
                         `Current Country of Residence`,
                         A307_04)

aggregate(increaseLeisure[, 2],
          list(increaseLeisure$`Current Country of Residence`),
          mean)

# we find that perceived benefits include enhancing leisure activities is significantly different in the different regions
# with germany / UK / US in rank for enhancing leisure activities

#---

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

providingComfort = select(singleSourceOfTruthAppended,
                          `Current Country of Residence`,
                          A307_06)

aggregate(providingComfort[, 2],
          list(providingComfort$`Current Country of Residence`),
          mean)

# we find that providing comfort is significantly different across regions
# rank: UK / US / DACH

#---

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

increasingSafety = select(singleSourceOfTruthAppended,
                          `Current Country of Residence`,
                          A307_07)
aggregate(increasingSafety[, 2],
          list(increasingSafety$`Current Country of Residence`),
          mean)

# we find that increasing safety is important to US
# rank: DACH / UK / US

#---

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
providingCare = select(singleSourceOfTruthAppended,
                       `Current Country of Residence`,
                       A307_08)
aggregate(providingCare[, 2],
          list(providingCare$`Current Country of Residence`),
          mean)

# providing care is different for DACH - US/UK
# rank DACH / US-UK (close)

#------------------------------------------------------------------------------------

#starting pairwise testing per country
# Country and adding to the property value


dunnTest(A307_10, as.factor(`Current Country of Residence`), method = "bonferroni")
epsilonSquared(x = as.numeric(A307_10), g = `Current Country of Residence`)
countryIncreaseProperty = select(singleSourceOfTruthAppended,
                                 `Current Country of Residence`,
                                 A307_10)
aggregate(
  countryIncreaseProperty[, 2],
  list(countryIncreaseProperty$`Current Country of Residence`),
  mean
)

# significant use to increase property value
# rank : DACH/ UK / US

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


E201_SMART_TV_RISK_CCR <-
  cbind(
    dunnTest(
      singleSourceOfTruthAppended$E201_16,
      singleSourceOfTruthAppended$`Current Country of Residence`,
      method = "bonferroni"
    )$res,
    c(
      epsilonSquared(
        x = subset(
          singleSourceOfTruthAppended,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United Kingdom"
        )$E201_16,
        g = subset(
          singleSourceOfTruthAppended,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United Kingdom"
        )$`Current Country of Residence`
      ),
      epsilonSquared(
        x = subset(
          singleSourceOfTruthAppended,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United States"
        )$E201_16,
        g = subset(
          singleSourceOfTruthAppended,
          `Current Country of Residence` == "DACH" |
            `Current Country of Residence` == "United States"
        )$`Current Country of Residence`
      ),
      "NA"
    )
  )
names(E201_SMART_TV_RISK_CCR)[5] = "Epsilon^2"
E201_SMART_TV_RISK_CCR <- E201_SMART_TV_RISK_CCR[-c(3)]

avg_smart_tv_risk = select(singleSourceOfTruthAppended,
                           E201_16,
                           `Current Country of Residence`)

aggregate(avg_smart_tv_risk[, 1],
          list(avg_smart_tv_risk$`Current Country of Residence`),
          mean)

# we find that the smart tv posed risk is significant by region
# rank UK / US /  - DACH


perceived_benefits <-
  select(
    singleSourceOfTruthAppended,
    `Current Country of Residence`,
    A307_04,
    A307_07,
    A307_08,
    A307_10
  )

A307_LATEX <-
  cbind(
    c(
      "A307_04",
      "A307_04",
      "A307_04",
      "A307_07",
      "A307_07",
      "A307_07",
      "A307_08",
      "A307_08",
      "A307_08",
      "A307_10",
      "A307_10",
      "A307_10"
    ),
    rbind(
      data.frame(
        dunnTest(
          singleSourceOfTruthAppended$A307_04,
          singleSourceOfTruthAppended$`Current Country of Residence`,
          method = "bonferroni"
        )$res
      ),
      data.frame(
        dunnTest(
          singleSourceOfTruthAppended$A307_07,
          singleSourceOfTruthAppended$`Current Country of Residence`,
          method = "bonferroni"
        )$res
      ),
      data.frame(
        dunnTest(
          singleSourceOfTruthAppended$A307_08,
          singleSourceOfTruthAppended$`Current Country of Residence`,
          method = "bonferroni"
        )$res
      ),
      data.frame(
        dunnTest(
          singleSourceOfTruthAppended$A307_10,
          singleSourceOfTruthAppended$`Current Country of Residence`,
          method = "bonferroni"
        )$res
      )
    )
  )

names(A307_LATEX)[1] <- "Code"
A307_LATEX <- A307_LATEX[-c(4)]

#smart lights actually smart home device (benefiting from smart capabilities)
CCR_Device_Smart_Benefit <- select(singleSourceOfTruthAppended, `Current Country of Residence`,A302_01:A302_19)
kruskal.test(CCR_Device_Smart_Benefit$`Current Country of Residence`, CCR_Device_Smart_Benefit$A302_11)
kruskal.test(CCR_Device_Smart_Benefit$`Current Country of Residence`, CCR_Device_Smart_Benefit$A302_14)
kruskal.test(CCR_Device_Smart_Benefit$`Current Country of Residence`, CCR_Device_Smart_Benefit$A302_16)
table(CCR_Device_Smart_Benefit$A302_11) #light
table(CCR_Device_Smart_Benefit$A302_14) #speaker
table(CCR_Device_Smart_Benefit$A302_16) #TV


#######ANOVA analyses##





detach(singleSourceOfTruthAppended)







