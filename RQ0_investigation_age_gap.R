# New investigation to age differences in participants 
library(stringr)

attach(singleSourceOfTruthAppended)


split_age = select(singleSourceOfTruthAppended, A001)
split_age = str_split_fixed(split_age$A001, "-", 2)
ages = cbind(select(singleSourceOfTruthAppended,participant_id, age ), split_age)
ages$flag = apply(ages, 1, function(x){ ifelse(x["age"] >= as.numeric(x["1"]) &&x["age"] <= as.numeric(x["2"]) , 1, 2) })
violations = subset(ages, flag == "2")

#view(ages)


detach(singleSourceOfTruthAppended)

# checking how many are non-binary
asd = select(singleSourceOfTruthAppended, Sex, A002)
qwe = subset(asd, A002 == "Non-binary")


table = select(singleSourceOfTruthAppended, `Current Country of Residence`, LA_Mean, sebis_avg)

dunnTest(table$LA_Mean,as.factor(table$`Current Country of Residence`))
dunnTest(table$sebis_avg,as.factor(table$`Current Country of Residence`))
