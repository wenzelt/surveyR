device_calc <- function(device_name) {
  asd = subset(d, Device == device_name)
  asd = select(asd,`People in Household`,Device)
  number_households = nrow(asd)
  number_households_in_bin = table(d$`People in Household`)
  return(asd)
}


asd = subset(d, Device == "Smart Coffee Maker")
select(asd,`People in Household`,Device)
number_households_in_bin = table(d$`People in Household`)


devices = unique(d$Device)
range_do = 1: length(devices)
for (i in range_do){
 print(device_calculation(devices[i]))
}




device_calculation <- function(device_name) {
  number_households_in_bin = table(d$`People in Household`)
  print(device_name)
  data_list = matrix(ncol = 4, nrow = 6)
  my_range = 1:6
  for(i in my_range) {    
    hh<-number_households_in_bin[[i]]
    device_no<-nrow(subset(d, Device == device_name & `People in Household` == i))
    orange<-c(i,hh,device_no , device_no/hh * 100)
    data_list[i,] <- orange 
  }
  res = data.frame(data_list)
  print(cor.test(res$X1, res$X4))
 
  return(list(c(res, plot(res$X1, res$X4))))
}
