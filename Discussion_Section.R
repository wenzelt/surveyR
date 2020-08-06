location <- select(singleSourceOfTruthAppended, R528_01:R528_12, R529_01:R529_12,R530_01:R530_12, `Current Country of Residence`)

location1 <- select(singleSourceOfTruthAppended,R528_01:R528_12, `Current Country of Residence`)
location2 <- select(singleSourceOfTruthAppended,R529_01:R529_12, `Current Country of Residence`)
location3 <- select(singleSourceOfTruthAppended,R530_01:R530_12, `Current Country of Residence`)

colnames(location1) <- c("Balcony","Basement", "Children's Room", "Dining Room", "Garage", "Guest Bedroom", "Hallway","Kitchen","Living Room" , "Master Bedroom","Patio", "Yard","Current Country of Residence")
colnames(location2) <- c("Balcony","Basement", "Children's Room", "Dining Room", "Garage", "Guest Bedroom", "Hallway","Kitchen","Living Room" , "Master Bedroom","Patio", "Yard","Current Country of Residence")
colnames(location3) <- c("Balcony","Basement", "Children's Room", "Dining Room", "Garage", "Guest Bedroom", "Hallway","Kitchen","Living Room" , "Master Bedroom","Patio", "Yard","Current Country of Residence")

location <- rbind(location1,location2,location3)
stack(location[1:12])
location_bool <- select(location, Balcony:Yard)

location_stacked <- subset(cbind(A=location[,13],stack(location[1:12])),values==TRUE,-2)
table(location_stacked)

ggplot(iris, aes(x=location_stacked$ind, fill=location_stacked$`Current Country of Residence`)) +
  geom_histogram()

install.packages("vcd")
library(vcd)
mosaic(table(location_stacked), shade = TRUE)


