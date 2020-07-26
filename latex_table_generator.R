#install dependencies for latex table 

library(xtable)

# export LA_Mean x E201 Latex
print(xtable(LA_E201_Latex, type = "latex",caption = "Summary statistic of correlation between Legislative Stance (LA) and Device Risk assessment (E201) measured on a 7-Point Likert scale"), file = "latex_tables/LA_MEANxE201.tex")


