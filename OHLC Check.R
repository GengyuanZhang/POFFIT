## This is to check for data consistency in terms of Open, High, Low, Close
## High > Low
## Open <= High
## Low <= Close

min_data<-min_data[which(min_data$HIGH1 > min_data$LOW1),]
min_data<-min_data[which(min_data$OPEN1 <= min_data$HIGH1),]
min_data<-min_data[which(min_data$LOW1 <= min_data$CLOSE1),]
min_data<-min_data[which(min_data$HIGH1 > min_data$LOW1),]
