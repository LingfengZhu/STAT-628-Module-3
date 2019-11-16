##########load data###########
library("factoextra")

data <- read.csv("att_data_3.csv")
head(data)

names <- (names(data)[!names(data) %in% c("X", "address", "city", "state", "postal_code", 
                                          "attributes", "categories", "hours")])
data <- data[, names]
head(data)

data$Alcohol[which(as.character(data$Alcohol)=="'none'")] <- ''
data$Alcohol <- droplevels(data$Alcohol)
unique(data$Alcohol)

data$is_open[which(data$is_open==0)] <- "0"
data$is_open[which(data$is_open==1)] <- "1"

data$park[which(data$park==0)] <- "0"
data$park[which(data$park==1)] <- "1"