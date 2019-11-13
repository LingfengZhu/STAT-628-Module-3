##########data###########
setwd("~/Downloads/2019Fall/628/Module3")

# load data
att_df <- read.csv('data/att_data.csv')
att_df <- att_df[, -1]
str(att_df)

##########attributes names###########
# colnames of nonsense attributes and attribute with a bigger category
noInput <- c("NoInput", "NoInput.1", "NoInput.2", "NoInput.3", "NoInput.4", "NoInput.5", "NoInput.6", "NoInput.7")
att_combo <- c('GoodForMeal', 'BusinessParking', 'Ambience', 'Music', 'BestNights', 'HairSpecializesIn', 
               'DietaryRestrictions')

# colnames of subset of attributes with a bigger category
colname_combo <- c('dessert', 'latenight', 'lunch', 'dinner', 'brunch', 'breakfast', 
                   'garage', 'street', 'validated', 'lot', 'valet',
                   'romantic', 'intimate', 'classy', 'hipster', 'divey', 'touristy', 'trendy', 'upscale', 'casual',
                   'dj', 'background_music', 'no_music', 'jukebox', 'live', 'video', 'karaoke',
                   'monday', 'tuesday', 'friday', 'wednesday', 'thursday', 'sunday', 'saturday',
                   'straightperms', 'coloring', 'extensions', 'africanamerican', 'curly', 'kids', 'perms', 'asian',
                   'dairy.free', 'gluten.free', 'vegan', 'kosher', 'halal', 'soy.free', 'vegetarian')
colname_index <- c(rep(1, 6), rep(2, 5), rep(3, 9), rep(4, 7), rep(5, 7), rep(6, 8), rep(7, 7))

##########data to be cleaned###########
# the data that ready to be cleaned
names <- (names(att_df)[!names(att_df) %in% c(noInput, att_combo)])
att_clean_df <- att_df[, names]
# str(att_clean_df)

##########clean parking information###########
# combine the five attributes of parking into one attributes
sub_data <- att_clean_df[, colname_combo[which(colname_index==2)]]
head(sub_data)

conv_fact <- function(x){
  if (x[1]=="" & x[2]=="" & x[3]=="" & x[4]=="" & x[5]==""){
    result <- ""
  } else if (x[1]=="False" & x[2]=="False" & x[3]=="False" & x[4]=="False" & x[5]=="False"){
    result <- 0
  } else {
    result <- 1
  }
}
park <- apply(sub_data, 1, conv_fact)

att_clean_df <- cbind(att_clean_df, "park"=park)
names <- (names(att_clean_df)[!names(att_clean_df) %in% colname_combo[which(colname_index==2)]])
att_clean_df <- att_clean_df[, names]


##########clean missing information###########
# combine the 'missing' and 'none' to be one level in all variable
for (i in 1:dim(att_clean_df)[2]) {
  att_clean_df[which(att_clean_df[,i]=="None"), i] <- ""
  att_clean_df[,i] <- droplevels(att_clean_df[,i])
}

str(att_clean_df)


##########clean factors value information###########
att_clean_df <- att_clean_df1

names <- c('NoiseLevel', 'WiFi', 'Alcohol', 'RestaurantsAttire', 'BYOBCorkage', 'Smoking')

for (i in 1:6) {
  #unique(att_clean_df$NoiseLevel)
  temp <- att_clean_df[, names[i]]
  
  first <- substr(temp, 1, 1)
  index <- which(first=="u")
  temp[index] <- substr(temp[index], 2, 20)
  att_clean_df[names[i]] <- as.factor(temp)
  
  att_clean_df[,names[i]] <- droplevels(att_clean_df[,names[i]])
}


write.csv(att_clean_df, "data/att_data_2.csv")


