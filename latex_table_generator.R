#install dependencies for latex table 

library(xtable)

print(xtable(
  select(Participants_DACH,participant_id,A004)
, type = "latex"), file = "filename
.tex")