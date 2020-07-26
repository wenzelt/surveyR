#install dependencies for latex table 

library(xtable)

# export LA_Mean x E201 Latex
print(xtable(LA_E201_Latex, type = "latex",caption = "Summary statistic of correlation between Legislative Stance (LA) and Device Risk assessment (E201) measured on a 7-Point Likert scale"), file = "latex_tables/LA_MEANxE201.tex")

# export LA_Mean x E201 Latex Interesting
print(xtable(LA_E201_Latex_Interesting, type = "latex",caption = "Summary statistic of correlation between Legislative Stance (LA) and Device Risk assessment (E201) measured on a 7-Point Likert scale"), file = "latex_tables/LA_MEANxE201_Interesting.tex")

# export LA_Mean x A204 Latex
print(xtable(LA_A204_Latex, type = "latex",caption = "Summary statistic of correlation between Legislative Stance (LA) and Perceived Responsibility measured on a 7-Point Likert scale between Oneself (1) and the Manufacturer (7)"), file = "latex_tables/LA_MEANxA204.tex")
