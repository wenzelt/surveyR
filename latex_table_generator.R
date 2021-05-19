#install dependencies for latex table

library(xtable)
####RQ1######
# export LA_Mean x E201 Latex
print(
  xtable(
    LA_E201_Latex,
    type = "latex",
    caption = "Summary statistic of correlation between Legislative Stance (LA) and Device Risk assessment (E201) measured on a 7-Point Likert scale",
    label = "RQ1_H3_DeviceRiskLA"
  ),
  file = "latex_tables/LA_MEANxE201.tex"
)

# export LA_Mean x E201 Latex Interesting
print(
  xtable(
    LA_E201_Latex_Interesting,
    type = "latex",
    caption = "Summary statistic of correlation between Legislative Stance (LA) and Device Risk assessment (E201) measured on a 7-Point Likert scale",
    label = "RQ1_H3_DeviceRiskLAInteresting"
  ),
  file = "latex_tables/LA_MEANxE201_Interesting.tex",digits = 2
)

# export LA_Mean x A204 Latex
print(
  xtable(
    LA_A204_Latex,
    type = "latex",
    caption = "Summary statistic of correlation between Legislative Stance (LA) and Perceived Responsibility measured on a 7-Point Likert scale between Oneself (1) and the Manufacturer (7)",
    label = "RQ1_H3_ResponsibilityLA"
  ),
  file = "latex_tables/LA_MEANxA204.tex", digits = 2
)

# export USAGE BY DEVICE
print(
  xtable(
    LA_MEAN_USAGE_DEVICE_INTERESTING,
    type = "latex",
    caption = "Effect of Legislative stance on the usage on a specific device." ,
    label = "RQ1_H2_UsageLA"
  ),
  file = "latex_tables/LA_MEAN_USAGE_DEVICE_INTERESTING.tex",digits = 2
)

####RQ2######

# export Usage x CCR Latex Overall Devices
print(
  xtable(
    Usage_CCR_LATEX,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart Home Devices Overall by Region of Residence",
    label = "RQ2_H2_UsageRegion"
  ),
  file = "latex_tables/USAGE_CCR_OVERALL.tex",digits = 2
)

# export Usage x CCR Latex Smart TV
print(
  xtable(
    Usage_CCR_LATEX_SMART_TV,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart TV's by Region of Residence",
    label = "RQ2_H2_UsageRegionSmartTV"
  ),
  file = "latex_tables/USAGE_CCR_SMART_TV.tex",digits = 2
)

# export Usage x CCR Latex Smart Speaker
print(
  xtable(
    Usage_CCR_LATEX_SMART_SPEAKER,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart Speakers by Region of Residence",
    label = "RQ2_H2_UsageRegionSmartSpeaker"
  ),
  file = "latex_tables/USAGE_CCR_SMART_SPEAKER.tex",digits = 2
)

# export Usage x CCR Latex Smart Lightbulb
print(
  xtable(
    Usage_CCR_LATEX_SMART_LIGHTBULB,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart Lightbulbs Overall by Region of Residence" ,
    label = "RQ2_H2_UsageRegionSmartLightbulbs"
  ),
  file = "latex_tables/USAGE_CCR_SMART_LIGHTBULB.tex",digits = 2
)

# export Usage x CCR Latex Other Devices
print(
  xtable(
    Usage_CCR_LATEX_OTHER,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart Devices Overall by Region of Residence",
    label = "RQ2_H2_UsageRegionOtherDevices"
  ),
  file = "latex_tables/USAGE_CCR_OTHER.tex",digits = 2
)

# export Usage x CCR Latex Other Devices
#print(
#  xtable(data.frame(cbind(DISABLED_FEATURES_COUNTRY_LATEX_CHI$p.value,DISABLED_FEATURES_COUNTRY_LATEX_CHI$method)), type = "latex", caption = "Chi-Squared Test for significant differences in disabling features in smart home devices by Region of residence."),
#  file = "latex_tables/DISABLED_FEATURES_CCR.tex"
#)

#Manufacturer Responsibility by country
print(
  xtable(dunn_A204_04_LATEX, type = "latex", caption = "DunnTest for participants opinion on Manufacturer responsibility on \"Keeping the Smart Home device secure\" by Region of Residence",label = "RQ2_H3_ResponsibilityA204_04")
  ,
  file = "latex_tables/A204_04_CCR.tex",digits = 2
)


#Device risk by country TV:
print(
  xtable(
    E201_SMART_TV_RISK_CCR,
    type = "latex",
    caption = "Device Risk assessment dunnTest by Current Region of residence.",
    label = "RQ2_H3_RiskPerceptionRegionSmartTV"
  ),
  file = "latex_tables/E201_SMART_TV_RISK_CCR.tex",digits = 2
)

#Perceived Benefits of Smart home devices by region of residence
print(
  xtable(
    A307_LATEX,
    type = "latex",
    caption = "Perceived benefits of smart home devices by region of residence. ",
    label = "RQ2_H3_BenefitSmartDevicesRegion"
  ),
  file = "latex_tables/A307_CCR_LATEX.tex",digits = 2,
  hline.after = c(-1, 0, 3, 6, 9, 12)
)


####RQ3####
print(
  xtable(
    USAGE_A005_LATEX,
    type = "latex",
    caption = "Household size correlation with the usage of most used devices.",
    label = "RQ3_H2_UsageChildren"
  ),
  file = "latex_tables/USAGE_A005_LATEX.tex",digits = 2
)

print(
  xtable(
    riskChildren_LATEX,
    type = "latex",
    caption = "Pereceived risk of Smart Home devices with children in the household.",
    label = "RQ3_H3_PerceivedRiskChildren"
  ),
  file = "latex_tables/riskChildren_LATEX.tex",digits = 2
)

print(
  xtable(
    responsibilityChildren_LATEX,
    type = "latex",
    caption = "Perceived responsibility regarding Smart Home Devices with children in the household.",
    label = "RQ3_H3_PerceivedResponsibilityChildren"
  ),
  file = "latex_tables/responsibilityChildren_LATEX.tex",digits = 2
)

print(
  xtable(
    USAGETYPE_Children_LATEX,
    type = "latex",
    caption = "Usage of Smart Home Devices with children in the household.",
    label = "RQ3_H3_UsageTypeChildren"
  ),
  file = "latex_tables/USAGETYPE_Children_LATEX.tex",digits = 2
)

print(
  xtable(
    demographic_overview_table,
    type = "latex",
    caption = "Summary of demographic information pertaining to our participants. ",
    label = "demographic_overview_table"
  ),
  file = "latex_tables/demographic_overview_table.tex"
)
