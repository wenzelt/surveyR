# New investigation to age differences in participants 
library(stringr)

attach(singleSourceOfTruthAppended)


split_age = select(singleSourceOfTruthAppended, A001)
split_age = str_split_fixed(split_age$A001, "-", 2)
ages = cbind(select(singleSourceOfTruthAppended, age), split_age)
ages$flag = apply(ages, 1, function(x){ ifelse(x["age"] >= as.numeric(x["1"]) -1  &&x["age"] <= as.numeric(x["2"]) + 1 , 1, 2) })
