##########load data###########
library("factoextra")

setwd("~/Downloads/2019Fall/628/Module3/data")

data <- read.csv("att_data_3")
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

##########Drop variables with more than 90% of missing###########
missing <- function(x){
  return(length(which(is.na(x)==TRUE | x==''))/dim(data)[1])
}

missing_per <- apply(data, 2, missing)
keep_var <- which(missing_per<0.9)
colnames(data)[keep_var]
data <- data[, keep_var]
data$Unnamed..0 <- NULL
missing_per <- apply(data, 2, missing)
missing_per

##########Impute the rest of data###########
library("VIM")
library("dplyr")

data_impute <- data

data_impute <- na_if(data_impute, "") #"dplyr" to replace "" with NA

data_impute <- kNN(data_impute, k=5) # "VIM" to use knn

apply(data_impute, 2, missing)


write.csv(data_impute, "att_data_clean.csv")

