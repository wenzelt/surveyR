#install dependencies for latex table 

library(xtable)

# export LA_Mean x E201 Latex
print(xtable(LA_E201_Latex, type = "latex"), file = "filename2.tex", include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = I, hline.after = c(0,1))


