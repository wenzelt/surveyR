
prop.table(table(singleSourceOfTruthAppended$Sex)) # female 
prop.table(table(singleSourceOfTruthAppended$age)) # 32.14 years old
prop.table(table(singleSourceOfTruthAppended$A003))#Bachelor's degree
prop.table(table(singleSourceOfTruthAppended$A006))
prop.table(table(singleSourceOfTruthAppended$A007))

demographics <- select(singleSourceOfTruthAppended, Sex, age,A001, A003, A006, A007)
view(dfSummary(demographics))

mean(as.numeric(as.character(singleSourceOfTruthAppended$age)), na.rm = T)
max(prop.table(table(singleSourceOfTruthAppended$A003)))#edu
max(prop.table(table(singleSourceOfTruthAppended$A006)))#between 40k and 49,999k 
prop.table(table(singleSourceOfTruthAppended$A007))# rents house 


range(rowMeans(select(singleSourceOfTruthAppended, S101_01:S101_12)))
range(singleSourceOfTruthAppended$sebis_avg)
range(singleSourceOfTruthAppended$sebis_DeviceSecurement_avg)
range(singleSourceOfTruthAppended$sebis_ProactiveAwareness_avg)
range(singleSourceOfTruthAppended$sebis_UpdatingBehaviour_avg)
