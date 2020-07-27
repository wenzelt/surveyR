#install dependencies for latex table

library(xtable)
####RQ1######
# export LA_Mean x E201 Latex
print(
  xtable(LA_E201_Latex, type = "latex", caption = "Summary statistic of correlation between Legislative Stance (LA) and Device Risk assessment (E201) measured on a 7-Point Likert scale"),
  file = "latex_tables/LA_MEANxE201.tex"
)

# export LA_Mean x E201 Latex Interesting
print(
  xtable(
    LA_E201_Latex_Interesting,
    type = "latex",
    caption = "Summary statistic of correlation between Legislative Stance (LA) and Device Risk assessment (E201) measured on a 7-Point Likert scale"
  ),
  file = "latex_tables/LA_MEANxE201_Interesting.tex"
)

# export LA_Mean x A204 Latex
print(
  xtable(LA_A204_Latex, type = "latex", caption = "Summary statistic of correlation between Legislative Stance (LA) and Perceived Responsibility measured on a 7-Point Likert scale between Oneself (1) and the Manufacturer (7)"),
  file = "latex_tables/LA_MEANxA204.tex"
)


####RQ2######

# export Usage x CCR Latex Overall Devices
print(
  xtable(Usage_CCR_LATEX, type = "latex", caption = "dunnTest comparison results for the usage of Smart Home Devices Overall by Region of Residence"),
  file = "latex_tables/USAGE_CCR_OVERALL.tex"
)

# export Usage x CCR Latex Smart TV
print(
  xtable(
    Usage_CCR_LATEX_SMART_TV,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart TV's by Region of Residence"
  ),
  file = "latex_tables/USAGE_CCR_SMART_TV.tex"
)

# export Usage x CCR Latex Smart Speaker
print(
  xtable(
    Usage_CCR_LATEX_SMART_SPEAKER,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart Speakers by Region of Residence"
  ),
  file = "latex_tables/USAGE_CCR_SMART_SPEAKER.tex"
)

# export Usage x CCR Latex Smart Lightbulb
print(
  xtable(
    Usage_CCR_LATEX_SMART_LIGHTBULB,
    type = "latex",
    caption = "dunnTest comparison results for the usage of Smart Lightbulbs Overall by Region of Residence"
  ),
  file = "latex_tables/USAGE_CCR_SMART_LIGHTBULB.tex"
)

# export Usage x CCR Latex Other Devices
print(
  xtable(Usage_CCR_LATEX_OTHER, type = "latex", caption = "dunnTest comparison results for the usage of Smart Lightbulbs Overall by Region of Residence"),
  file = "latex_tables/USAGE_CCR_OTHER.tex"
)


# export Usage x CCR Latex Other Devices
print(
  xtable(data.frame(cbind(DISABLED_FEATURES_COUNTRY_LATEX_CHI$p.value,DISABLED_FEATURES_COUNTRY_LATEX_CHI$method)), type = "latex", caption = "Chi-Squared Test for significant differences in disabling features in smart home devices by Region of residence."),
  file = "latex_tables/DISABLED_FEATURES_CCR.tex"
)


print(
  xtable(dunn_A204_04_LATEX, type = "latex", caption = "DunnTest for participants opinion on Manufacturer responsibility on \"Keeping the Smart Home device secure\" by Region of Residence"),
  file = "latex_tables/A204_04_CCR.tex"
)
