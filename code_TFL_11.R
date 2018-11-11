load(file="C:/Users/YKHAN/Documents/Technology for Learning/Project/Data.Rdata")
colnames(dat)
head(dat)
dat <- as.data.frame(dat)
dat <- dat[order(dat$created),]
summary(dat)
#to convert the created date in seconds into the date format
library(lubridate)
seconds <- dat$created
dat$created <- as.POSIXct("09/01/2017 01:37:33", format = "%d/%m/%Y %H:%M:%S") + seconds
weekdays <- weekdays(as.Date(dat$created))
dat <- cbind(dat, weekdays)
head(dat)

#split the date into year, month, day
hours <- format(as.POSIXct(strptime(dat$created,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
dates <- format(as.POSIXct(strptime(dat$created,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")
months <- format(as.POSIXct(strptime(dat$created,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m")
only_hours <- format(as.POSIXct(strptime(dat$created,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
dat$hours <- hours
dat$months <- months
dat$dates <- dates
dat$only_hours <- only_hours

#This function is for deciding if a response belongs to a session

session_user <- function(directory = "user1", user_id = 1:length(user1)) {
  
  empty_f <- data.frame() # creates an empty data frame
  v=0
  
  for (i in 2:nrow(user1)) {
    
    session_yn <- if(user1[i,7] == user1[i-1,7] && user1[i,1] == user1[i-1,1] && user1[i,5] - user1[i-1,5] <=20) {
      v
    } else {
      v = v + 1
    }
    
    
    
    data_fr <- data.frame("Response" = i,
                          session_yn) # here I want to store all the outputs of the values in a dataframe
    
    empty_f <- rbind(empty_f, data_fr) # here the data_frame should be added to an existing data frame
  }
  
  print(empty_f)
  
}

session_user()

#Table cleaning
na.omit(dat)

#create matrix used to remove people with 0-9 responses in the whole data
IV = unique(dat$user_id)
SomeMatrix_alldat = matrix(ncol = 2)

for (i in 1:length(IV)) {
  tempdata = subset(dat, user_id == IdentifierVector[i])
  var1 = nrow(tempdata)
  SomeMatrix_alldat = rbind(SomeMatrix_alldat, c(IV[i], var1))
  i = i + 1
}
SomeMatrix_alldat = SomeMatrix_alldat[-1,]

#remove people with 0-9 responses
SomeMatrix_alldat = subset(SomeMatrix_alldat, SomeMatrix_alldat[,2] >= 10)
CleanIV = unique(SomeMatrix_alldat[,1])
cleandat = subset(cleandat, user_id %in% CleanIV)
#write.csv(cleandat_dif0, file = "cleandat_dif0.csv")



#Create matrixes which are used to remove people with 0-9 responses per difficulty
IdentifierVector = unique(cleandat$user_id)

SomeMatrix_dif0 = matrix(ncol = 2)
SomeMatrix_dif1 = matrix(ncol = 2)
SomeMatrix_dif2 = matrix(ncol = 2)

for (i in 1:length(IdentifierVector)) {
  tempdata = subset(cleandat, user_id == IdentifierVector[i])
  var1 = nrow(tempdata)
  SomeMatrix_dif0 = rbind(SomeMatrix_dif0, c(IdentifierVector[i], var1))
  i = i + 1
}
SomeMatrix_dif0 = SomeMatrix_dif0[-1,]

for (i in 1:length(IdentifierVector)) {
  tempdata = subset(cleandat, user_id == IdentifierVector[i])
  var1 = nrow(tempdata)
  SomeMatrix_dif1 = rbind(SomeMatrix_dif1, c(IdentifierVector[i], var1))
  i = i + 1
}
SomeMatrix_dif1 = SomeMatrix_dif1[-1,]

for (i in 1:length(IdentifierVector)) {
  tempdata = subset(cleandat, user_id == IdentifierVector[i])
  var1 = nrow(tempdata)
  SomeMatrix_dif2 = rbind(SomeMatrix_dif2, c(IdentifierVector[i], var1))
  i = i + 1
}
SomeMatrix_dif2 = SomeMatrix_dif2[-1,]



#Select all ID's of learners with 10 or more questions completed for difficulty 0
SomeMatrix2_dif0 = subset(SomeMatrix_dif0, SomeMatrix_dif0[,2] >= 10)
CleanedIdentifierVector_dif0 = unique(SomeMatrix2_dif0[,1])
cleandat_dif0 = subset(cleandat, user_id %in% CleanedIdentifierVector_dif0)
#write.csv(cleandat_dif0, file = "cleandat_dif0.csv")

SomeMatrix2_dif1 = subset(SomeMatrix_dif1, SomeMatrix_dif1[,2] >= 10)
CleanedIdentifierVector_dif1 = unique(SomeMatrix2_dif1[,1])
cleandat_dif1 = subset(cleandat, user_id %in% CleanedIdentifierVector_dif1)
#write.csv(cleandat_dif0, file = "cleandat_dif0.csv")

SomeMatrix2_dif2 = subset(SomeMatrix_dif2, SomeMatrix_dif2[,2] >= 10)
CleanedIdentifierVector_dif2 = unique(SomeMatrix2_dif2[,1])
cleandat_dif2 = subset(cleandat, user_id %in% CleanedIdentifierVector_dif2)
#write.csv(cleandat_dif0, file = "cleandat_dif0.csv")



#Part of data with row 11 to 40
SomeMatrix11_40 = subset(SomeMatrix, SomeMatrix[,2] >= 10)
SomeMatrix11_40 = subset(SomeMatrix11_40, SomeMatrix11_40[,2] <= 40)
CIV11_40 = unique(SomeMatrix11_40[,1])
cleandat11_40 = subset(cleandat, user_id %in% CIV11_40)
write.csv(cleandat11_40, file = "cleandat11_40.csv")

#41 to 100
SomeMatrix41_100 = subset(SomeMatrix, SomeMatrix[,2] >= 41)
SomeMatrix41_100 = subset(SomeMatrix41_100, SomeMatrix41_100[,2] <= 100)
CIV41_100 = unique(SomeMatrix41_100[,1])
cleandat41_100 = subset(cleandat, user_id %in% CIV41_100)
write.csv(cleandat41_100, file = "cleandat41_100.csv")

#101 to 500
SomeMatrix101_500 = subset(SomeMatrix, SomeMatrix[,2] >= 101)
SomeMatrix101_500 = subset(SomeMatrix101_500, SomeMatrix101_500[,2] <= 500)
CIV101_500 = unique(SomeMatrix101_500[,1])
cleandat101_500 = subset(cleandat, user_id %in% CIV101_500)
write.csv(cleandat101_500, file = "cleandat101_500.csv")

#501+
SomeMatrix500plus = subset(SomeMatrix, SomeMatrix[,2] >= 501)
CIV500plus = unique(SomeMatrix500plus[,1])
cleandat500plus = subset(cleandat, user_id %in% CIV500plus)
write.csv(cleandat500plus, file = "cleandat500plus.csv")


#### Matrix: ID - Ratio - Nrows - Delta 
#### % correct per difficulty over all cleaned data 

#Difficulty 0
dif0table = subset(cleandat, cleandat$difficulty == 0)
dif0table = na.omit(dif0table)

CleanerMatrix_dif0 = matrix(ncol = 2)
IdentifierVector_dif0table = unique(dif0table$user_id)
for (i in 1:length(IdentifierVector_dif0table)) {
  tempdata = subset(dif0table, user_id == IdentifierVector_dif0table[i])
  var1 = nrow(tempdata)
  CleanerMatrix_dif0 = rbind(CleanerMatrix_dif0, c(IdentifierVector_dif0table[i], var1))
  i = i + 1
}
CleanerMatrix_dif0 = CleanerMatrix_dif0[-1,]
CleanerMatrix_dif0_2 = subset(CleanerMatrix_dif0, CleanerMatrix_dif0[,2] >= 10)
CleanerMatrixIdentifierVector_dif0 = unique(CleanerMatrix_dif0_2[,1])
dif0table = subset(dif0table, user_id %in% CleanerMatrixIdentifierVector_dif0)

var1 = sum(dif0table$correct_answered)
var2 = nrow(dif0table)
Difficulty0percentage = var1/var2

#Difficulty 1
dif1table = subset(cleandat, cleandat$difficulty == 1)
dif1table = na.omit(dif1table)
var1 = sum(dif1table$correct_answered)
var2 = nrow(dif1table)
Difficulty1percentage = var1/var2

#Difficulty 2
dif2table = subset(cleandat, cleandat$difficulty == 2)
dif2table = na.omit(dif2table)
var1 = sum(dif2table$correct_answered)
var2 = nrow(dif2table)
Difficulty2percentage = var1/var2

#Table for difficulty distributions
Difficulty0DistributionMatrix = matrix(ncol = 4)
Difficulty1DistributionMatrix = matrix(ncol = 4)
Difficulty2DistributionMatrix = matrix(ncol = 4)

#Difficulty 0 Distribution
uniqueid0 = unique(dif0table$user_id)
uniqueid1 = unique(dif1table$user_id)
uniqueid2 = unique(dif2table$user_id)

#Dif 0 - 90%
for (i in 1:length(uniqueid0)) {
  tempdata = subset(dif0table, user_id == uniqueid0[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.9
  Difficulty0DistributionMatrix = rbind(Difficulty0DistributionMatrix, c(uniqueid0[i], var3, var2, delta))
  i = i + 1
}

#Dif 1 - 75%
for (i in 1:length(uniqueid1)) {
  tempdata = subset(dif1table, user_id == uniqueid1[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2 
  delta = var3 - 0.75
  Difficulty1DistributionMatrix = rbind(Difficulty1DistributionMatrix, c(uniqueid1[i], var3, var2, delta))
  i = i + 1
}

#Dif 2 - 60%
for (i in 1:length(uniqueid2)) {
  tempdata = subset(dif2table, user_id == uniqueid2[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.6
  Difficulty2DistributionMatrix = rbind(Difficulty2DistributionMatrix, c(uniqueid2[i], var3, var2, delta))
  i = i + 1
}

Difficulty0DistributionMatrix = Difficulty0DistributionMatrix[-1,]
Difficulty1DistributionMatrix = Difficulty1DistributionMatrix[-1,]
Difficulty2DistributionMatrix = Difficulty2DistributionMatrix[-1,]





#### % correct per difficulty over 11_40 responses data subset
#Difficulty 0
z11_40_dif0table = subset(cleandat11_40, cleandat11_40$difficulty == 0)
z11_40_dif0table = na.omit(z11_40_dif0table)
var1 = sum(z11_40_dif0table$correct_answered)
var2 = nrow(z11_40_dif0table)
z11_40_Dif0_perc = var1/var2

#Difficulty 1
z11_40_dif1table = subset(cleandat11_40, cleandat11_40$difficulty == 1)
z11_40_dif1table = na.omit(z11_40_dif1table)
var1 = sum(z11_40_dif1table$correct_answered)
var2 = nrow(z11_40_dif1table)
z11_40_dif1_perc = var1/var2

#Difficulty 2
z11_40_dif2table = subset(cleandat11_40, cleandat11_40$difficulty == 2)
z11_40_dif2table = na.omit(z11_40_dif2table)
var1 = sum(z11_40_dif2table$correct_answered)
var2 = nrow(z11_40_dif2table)
z11_40_dif2_perc = var1/var2

#Matrixes
z11_40_dif0_matrix = matrix(ncol = 4)
z11_40_dif1_matrix = matrix(ncol = 4)
z11_40_dif2_matrix = matrix(ncol = 4)

#Vectors with unique ID
z11_40_uniqueid0 = unique(z11_40_dif0table$user_id)
z11_40_uniqueid1 = unique(z11_40_dif1table$user_id)
z11_40_uniqueid2 = unique(z11_40_dif2table$user_id)

#Dif 0 - 90%
for (i in 1:length(z11_40_uniqueid0)) {
  tempdata = subset(z11_40_dif0table, user_id == z11_40_uniqueid0[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.9
  z11_40_dif0_matrix = rbind(z11_40_dif0_matrix, c(z11_40_uniqueid0[i], var3, var2, delta))
  i = i + 1
}

#Dif 1 - 75%
for (i in 1:length(z11_40_uniqueid1)) {
  tempdata = subset(z11_40_dif1table, user_id == z11_40_uniqueid1[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2 
  delta = var3 - 0.75
  z11_40_dif1_matrix = rbind(z11_40_dif1_matrix, c(z11_40_uniqueid1[i], var3, var2, delta))
  i = i + 1
}

#Dif 2 - 60%
for (i in 1:length(z11_40_uniqueid2)) {
  tempdata = subset(z11_40_dif2table, user_id == z11_40_uniqueid2[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.6
  z11_40_dif2_matrix = rbind(z11_40_dif2_matrix, c(z11_40_uniqueid2[i], var3, var2, delta))
  i = i + 1
}

#Remove first rows
z11_40_dif0_matrix = z11_40_dif0_matrix[-1,]
z11_40_dif1_matrix = z11_40_dif1_matrix[-1,]
z11_40_dif2_matrix = z11_40_dif2_matrix[-1,]






#### % correct per difficulty over 41_100 responses data subset
#Difficulty 0
z41_100_dif0table = subset(cleandat41_100, cleandat41_100$difficulty == 0)
z41_100_dif0table = na.omit(z41_100_dif0table)
var1 = sum(z41_100_dif0table$correct_answered)
var2 = nrow(z41_100_dif0table)
z41_100_Dif0_perc = var1/var2

#Difficulty 1
z41_100_dif1table = subset(cleandat41_100, cleandat41_100$difficulty == 1)
z41_100_dif1table = na.omit(z41_100_dif1table)
var1 = sum(z41_100_dif1table$correct_answered)
var2 = nrow(z41_100_dif1table)
z41_100_dif1_perc = var1/var2

#Difficulty 2
z41_100_dif2table = subset(cleandat41_100, cleandat41_100$difficulty == 2)
z41_100_dif2table = na.omit(z41_100_dif2table)
var1 = sum(z41_100_dif2table$correct_answered)
var2 = nrow(z41_100_dif2table)
z41_100_dif2_perc = var1/var2

#Matrixes
z41_100_dif0_matrix = matrix(ncol = 4)
z41_100_dif1_matrix = matrix(ncol = 4)
z41_100_dif2_matrix = matrix(ncol = 4)

#Vectors with unique ID
z41_100_uniqueid0 = unique(z41_100_dif0table$user_id)
z41_100_uniqueid1 = unique(z41_100_dif1table$user_id)
z41_100_uniqueid2 = unique(z41_100_dif2table$user_id)

#Dif 0 - 90%
for (i in 1:length(z41_100_uniqueid0)) {
  tempdata = subset(z41_100_dif0table, user_id == z41_100_uniqueid0[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.9
  z41_100_dif0_matrix = rbind(z41_100_dif0_matrix, c(z41_100_uniqueid0[i], var3, var2, delta))
  i = i + 1
}

#Dif 1 - 75%
for (i in 1:length(z41_100_uniqueid1)) {
  tempdata = subset(z41_100_dif1table, user_id == z41_100_uniqueid1[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2 
  delta = var3 - 0.75
  z41_100_dif1_matrix = rbind(z41_100_dif1_matrix, c(z41_100_uniqueid1[i], var3, var2, delta))
  i = i + 1
  print(i)
}

#Dif 2 - 60%
for (i in 1:length(z41_100_uniqueid2)) {
  tempdata = subset(z41_100_dif2table, user_id == z41_100_uniqueid2[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.6
  z41_100_dif2_matrix = rbind(z41_100_dif2_matrix, c(z41_100_uniqueid2[i], var3, var2, delta))
  i = i + 1
}

#Remove first rows
z41_100_dif0_matrix = z41_100_dif0_matrix[-1,]
z41_100_dif1_matrix = z41_100_dif1_matrix[-1,]
z41_100_dif2_matrix = z41_100_dif2_matrix[-1,]






#### % correct per difficulty over 101_500 responses data subset
#Difficulty 0
z101_500_dif0table = subset(cleandat101_500, cleandat101_500$difficulty == 0)
z101_500_dif0table = na.omit(z101_500_dif0table)
var1 = sum(z101_500_dif0table$correct_answered)
var2 = nrow(z101_500_dif0table)
z101_500_Dif0_perc = var1/var2

#Difficulty 1
z101_500_dif1table = subset(cleandat101_500, cleandat101_500$difficulty == 1)
z101_500_dif1table = na.omit(z101_500_dif1table)
var1 = sum(z101_500_dif1table$correct_answered)
var2 = nrow(z101_500_dif1table)
z101_500_dif1_perc = var1/var2

#Difficulty 2
z101_500_dif2table = subset(cleandat101_500, cleandat101_500$difficulty == 2)
z101_500_dif2table = na.omit(z101_500_dif2table)
var1 = sum(z101_500_dif2table$correct_answered)
var2 = nrow(z101_500_dif2table)
z101_500_dif2_perc = var1/var2

#Matrixes
z101_500_dif0_matrix = matrix(ncol = 4)
z101_500_dif1_matrix = matrix(ncol = 4)
z101_500_dif2_matrix = matrix(ncol = 4)

#Vectors with unique ID
z101_500_uniqueid0 = unique(z101_500_dif0table$user_id)
z101_500_uniqueid1 = unique(z101_500_dif1table$user_id)
z101_500_uniqueid2 = unique(z101_500_dif2table$user_id)

#Dif 0 - 90%
for (i in 1:length(z101_500_uniqueid0)) {
  tempdata = subset(z101_500_dif0table, user_id == z101_500_uniqueid0[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.9
  z101_500_dif0_matrix = rbind(z101_500_dif0_matrix, c(z101_500_uniqueid0[i], var3, var2, delta))
  i = i + 1
}

#Dif 1 - 75%
for (i in 1:length(z101_500_uniqueid1)) {
  tempdata = subset(z101_500_dif1table, user_id == z101_500_uniqueid1[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2 
  delta = var3 - 0.75
  z101_500_dif1_matrix = rbind(z101_500_dif1_matrix, c(z101_500_uniqueid1[i], var3, var2, delta))
  i = i + 1
}

#Dif 2 - 60%
for (i in 1:length(z101_500_uniqueid2)) {
  tempdata = subset(z101_500_dif2table, user_id == z101_500_uniqueid2[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.6
  z101_500_dif2_matrix = rbind(z101_500_dif2_matrix, c(z101_500_uniqueid2[i], var3, var2, delta))
  i = i + 1
}

#Remove first rows
z101_500_dif0_matrix = z101_500_dif0_matrix[-1,]
z101_500_dif1_matrix = z101_500_dif1_matrix[-1,]
z101_500_dif2_matrix = z101_500_dif2_matrix[-1,]





#### % correct per difficulty over 500plus responses data subset
#Difficulty 0
z500p_dif0table = subset(cleandat500plus, cleandat500plus$difficulty == 0)
z500p_dif0table = na.omit(z500p_dif0table)
var1 = sum(z500p_dif0table$correct_answered)
var2 = nrow(z500p_dif0table)
z500p_Dif0_perc = var1/var2

#Difficulty 1
z500p_dif1table = subset(cleandat500plus, cleandat500plus$difficulty == 1)
z500p_dif1table = na.omit(z500p_dif1table)
var1 = sum(z500p_dif1table$correct_answered)
var2 = nrow(z500p_dif1table)
z500p_dif1_perc = var1/var2

#Difficulty 2
z500p_dif2table = subset(cleandat500plus, cleandat500plus$difficulty == 2)
z500p_dif2table = na.omit(z500p_dif2table)
var1 = sum(z500p_dif2table$correct_answered)
var2 = nrow(z500p_dif2table)
z500p_dif2_perc = var1/var2

#Matrixes
z500p_dif0_matrix = matrix(ncol = 4)
z500p_dif1_matrix = matrix(ncol = 4)
z500p_dif2_matrix = matrix(ncol = 4)

#Vectors with unique ID
z500p_uniqueid0 = unique(z500p_dif0table$user_id)
z500p_uniqueid1 = unique(z500p_dif1table$user_id)
z500p_uniqueid2 = unique(z500p_dif2table$user_id)

#Dif 0 - 90%
for (i in 1:length(z500p_uniqueid0)) {
  tempdata = subset(z500p_dif0table, user_id == z500p_uniqueid0[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.9
  z500p_dif0_matrix = rbind(z500p_dif0_matrix, c(z500p_uniqueid0[i], var3, var2, delta))
  i = i + 1
}

#Dif 1 - 75%
for (i in 1:length(z500p_uniqueid1)) {
  tempdata = subset(z500p_dif1table, user_id == z500p_uniqueid1[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2 
  delta = var3 - 0.75
  z500p_dif1_matrix = rbind(z500p_dif1_matrix, c(z500p_uniqueid1[i], var3, var2, delta))
  i = i + 1
}

#Dif 2 - 60%
for (i in 1:length(z500p_uniqueid2)) {
  tempdata = subset(z500p_dif2table, user_id == z500p_uniqueid2[i])
  var1 = sum(tempdata$correct_answered)
  var2 = nrow(tempdata)
  var3 = var1/var2
  delta = var3 - 0.6
  z500p_dif2_matrix = rbind(z500p_dif2_matrix, c(z500p_uniqueid2[i], var3, var2, delta))
  i = i + 1
}

#Remove first rows
z500p_dif0_matrix = z500p_dif0_matrix[-1,]
z500p_dif1_matrix = z500p_dif1_matrix[-1,]
z500p_dif2_matrix = z500p_dif2_matrix[-1,]


#plots
plot(density(z500p_dif0_matrix[,4]))
plot(density(z500p_dif1_matrix[,4]))
plot(density(z500p_dif2_matrix[,4]))


tempdata = subset(cleandat, cleandat$user_id == 5588)
tempdata = subset(items, items$id == 894)



#Great graphs require shitty code
mean(cleandat$user_domain_rating)
mean(cleandat$item_rating)

tempdata = subset(Difficulty0DistributionMatrix, Difficulty0DistributionMatrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 0, 4432 unique users with at least 10 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = Difficulty0DistributionMatrix[,1]
tempdata = subset(dif0table, dif0table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(Difficulty1DistributionMatrix, Difficulty1DistributionMatrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 1, 4371 unique users with at least 10 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = Difficulty1DistributionMatrix[,1]
tempdata = subset(dif1table, dif1table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(Difficulty2DistributionMatrix, Difficulty2DistributionMatrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 2, 6661 unique users with at least 10 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = Difficulty2DistributionMatrix[,1]
tempdata = subset(dif2table, dif2table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])


tempdata = subset(z11_40_dif0_matrix, z11_40_dif0_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 0, 314 unique users with 10 to 40 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z11_40_dif0_matrix[,1]
tempdata = subset(dif0table, dif0table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z11_40_dif1_matrix, z11_40_dif1_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 1, 326 unique users with 10 to 40 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z11_40_dif1_matrix[,1]
tempdata = subset(dif1table, dif1table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z11_40_dif2_matrix, z11_40_dif2_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 2, 385 unique users with 10 to 40 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z11_40_dif2_matrix[,1]
tempdata = subset(dif2table, dif2table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])


tempdata = subset(z41_100_dif0_matrix, z41_100_dif0_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 0, 1080 unique users with 41 to 100 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z41_100_dif0_matrix[,1]
tempdata = subset(dif0table, dif0table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z41_100_dif1_matrix, z41_100_dif1_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 1, 1178 unique users with 41 to 100 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z41_100_dif1_matrix[,1]
tempdata = subset(dif1table, dif1table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z41_100_dif2_matrix, z41_100_dif2_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 2, 1549 unique users with 41 to 100 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z41_100_dif2_matrix[,1]
tempdata = subset(dif2table, dif2table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])


tempdata = subset(z101_500_dif0_matrix, z101_500_dif0_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 0, 3683 unique users with 101 to 500 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z101_500_dif0_matrix[,1]
tempdata = subset(dif0table, dif0table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z101_500_dif1_matrix, z101_500_dif1_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 1, 3702 unique users with 101 to 500 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z101_500_dif1_matrix[,1]
tempdata = subset(dif1table, dif1table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z101_500_dif2_matrix, z101_500_dif2_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 2, 5618 unique users with 101 to 500 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z101_500_dif2_matrix[,1]
tempdata = subset(dif2table, dif2table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])


tempdata = subset(z500p_dif0_matrix, z500p_dif0_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 0, 1012 unique users with over 500 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z500p_dif0_matrix[,1]
tempdata = subset(dif0table, dif0table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z500p_dif1_matrix, z500p_dif1_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 1, 964 unique users with over 500 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z500p_dif1_matrix[,1]
tempdata = subset(dif1table, dif1table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])

tempdata = subset(z500p_dif2_matrix, z500p_dif2_matrix[,3] >= 10)
mean(tempdata[,2])
hist(tempdata[,4], breaks = 100, probability = TRUE, main = "Difficulty 2, 1315 unique users with over 500 responses", xlab = "delta", ylab = "relative frequency")
lines(density(tempdata[,4]), lwd = 2, col = "red")
length(unique(tempdata[,4]))
tempvector = z500p_dif2_matrix[,1]
tempdata = subset(dif2table, dif2table$user_id %in% tempvector)
mean(tempdata[,8])
mean(tempdata[,9])


mean(cleandat500plus$user_domain_rating)
hist(cleandat500plus$user_domain_rating)

mean(cleandat41_100$user_domain_rating)
hist(cleandat41_100$user_domain_rating)
mean(cleandat$user_domain_rating)

MemeVector = z500p_dif0_matrix[,1]
MemeData = subset(dif0table, dif0table$user_id %in% MemeVector)
hist(MemeData$user_domain_rating)

MemeVector = z500p_dif1_matrix[,1]
MemeData = subset(dif1table[dif1table$user_id %in% MemeVector,])
hist(MemeData$user_domain_rating)

MemeVector = z500p_dif2_matrix[,1]
MemeData = subset(dif2table[dif2table$user_id %in% MemeVector,])
hist(MemeData$user_domain_rating)

mean(MemeData$user_domain_rating)
hist(cleandat$user_domain_rating)
max(cleandat500plus$user_domain_rating)

MemeData = subset(cleandat500plus, cleandat500plus$user_id == 100331)
MemeData = subset(MemeData, MemeData$difficulty == 2)
#plot(x = MemeData$item_rating, y = MemeData$user_domain_rating)
MemeVector = matrix(nrow = nrow(MemeData), ncol = 1)  
for (i in 1:nrow(MemeVector)) {
  MemeVector[i,1] = MemeData[i,8] - MemeData[i,9]
  i = i + 1
}
max(MemeVector[,1])
mean(MemeVector[,1])
plot(x = MemeVector[,1], y = MemeData$user_domain_rating)

plot(x = cleandat$item_rating, y = cleandat$user_domain_rating)

plot(x = MemeData$created, y = MemeData$user_domain_rating)
Memevar1 = nrow(MemeData)
Memevar2 = sum(MemeData$correct_answered)
Memevar3 = Memevar2 / Memevar1
print(Memevar3)


#Writing tables
write.table(z11_40_dif0_matrix, file="11_40_dif0.txt", row.names=FALSE, col.names=FALSE)
write.table(z11_40_dif1_matrix, file="11_40_dif1.txt", row.names=FALSE, col.names=FALSE)
write.table(z11_40_dif2_matrix, file="11_40_dif2.txt", row.names=FALSE, col.names=FALSE)

write.table(z41_100_dif0_matrix, file="41_100_dif0.txt", row.names=FALSE, col.names=FALSE)
write.table(z41_100_dif1_matrix, file="41_100_dif1.txt", row.names=FALSE, col.names=FALSE)
write.table(z41_100_dif2_matrix, file="41_100_dif2.txt", row.names=FALSE, col.names=FALSE)

write.table(z101_500_dif0_matrix, file="101_500_dif0.txt", row.names=FALSE, col.names=FALSE)
write.table(z101_500_dif1_matrix, file="101_500_dif1.txt", row.names=FALSE, col.names=FALSE)
write.table(z101_500_dif2_matrix, file="101_500_dif2.txt", row.names=FALSE, col.names=FALSE)

write.table(z500p_dif0_matrix, file="500plus_dif0.txt", row.names=FALSE, col.names=FALSE)
write.table(z500p_dif1_matrix, file="500plus_dif1.txt", row.names=FALSE, col.names=FALSE)
write.table(z500p_dif2_matrix, file="500plus_dif2.txt", row.names=FALSE, col.names=FALSE)

#to find all rows with certain ID
dat_user_id <- subset(dat, dat$user_id %in% 74)
dat

#to find all rows with correct answered = 0
dat_0 <- subset(dat, dat$correct_answered %in% 0)
dat_0

#to find all rows with correct answered = 1
dat_1 <- subset(dat, dat$correct_answered %in% 1)
dat_1

library(dplyr)
#total answers per item_id
item_id_count <- tally(group_by(dat, item_id))
write.csv(item_id_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/item_id_count.csv")

#total answers per weekday
weekdays_count <- tally(group_by(dat$user_id, weekdays))
write.csv(weekdays_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/weekdays_count.csv")

#total users per month
month_count <- tally(group_by(dat$user_id, as.numeric(months)))
write.csv(month_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/month_count.csv")

#total users per month
hours_count <- tally(group_by(dat$user_id, as.numeric(only_hours)))
write.csv(hours_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/hours_count.csv")


library(plyr)
#number of distinct user per weekday
ddply(dat,~weekdays,summarise,number_of_distinct_users=length(unique(user_id)))
write.csv(weekdays_count_users, "C:/Users/YKHAN/Documents/Technology for Learning/Project/weekdays_count_users.csv")

#number of distinct users per month
months_users_count <- ddply(dat,~months,summarise,number_of_distinct_users=length(unique(user_id)))
write.csv(months_users_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/months_count_users.csv")

#number of distinct users per day
date_users_count <- ddply(dat,~dates,summarise,number_of_distinct_users=length(unique(user_id)))
write.csv(months_users_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/dates_count_users.csv")
plotly(date_users_count$dates, date_users_count$number_of_distinct_users)

#number of distinct users per hour
hours_users_count <- ddply(dat,~only_hours,summarise,number_of_distinct_users=length(unique(user_id)))
write.csv(hours_users_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/hours_count_users.csv")

#number of responses per hour
hours_responses_count <- ddply(dat,~only_hours,summarise,number_of_distinct_users=length(user_id))
write.csv(hours_responses_count, "C:/Users/YKHAN/Documents/Technology for Learning/Project/dates_count_responses.csv")

#number of incorrect responses per hour
hours_responses_count_0 <- ddply(dat_0,~only_hours,summarise,number_of_distinct_users=length(user_id))

#number of correct responses per hour
hours_responses_count_1 <- ddply(dat_1,~only_hours,summarise,number_of_distinct_users=length(user_id))

#plot number of incorrect responses per hour
library(plotly)
f <- list(
  family = "Courier New",
  size = 18,
  color = "#7f7f7f"
)
xas <- list(
  title = "Hour of the day",
  titlefont = f
)
yas <- list(
  title = "Number of incorrect responses",
  titlefont = f
)
number_responses_hours_plot_0 <- plot_ly(hours_responses_count_0, x = ~ hours_responses_count_0$only_hours, y = ~ hours_responses_count_0$number_of_distinct_users, type = 'bar', name = 'Number of incorrect responses for each hour of the day') %>%
  layout(title = 'Number of incorrect responses for each hour of the day', xaxis = xas, yaxis = yas)
number_responses_hours_plot_0

#plot number of correct responses per hour
library(plotly)
f <- list(
  family = "Courier New",
  size = 18,
  color = "#7f7f7f"
)
xas <- list(
  title = "Hour of the day",
  titlefont = f
)
yas <- list(
  title = "Number of correct responses",
  titlefont = f
)
number_responses_hours_plot_1 <- plot_ly(hours_responses_count_1, x = ~ hours_responses_count_1$only_hours, y = ~ hours_responses_count_1$number_of_distinct_users, type = 'bar', name = 'Number of correct responses for each hour of the day') %>%
  layout(title = 'Number of correct responses for each hour of the day', xaxis = xas, yaxis = yas)
number_responses_hours_plot_1

#clock plot
x <- hours_users_count$number_of_distinct_users
clock.plot <- function (x, col = rainbow(n), ...) {
  if( min(x)<0 ) x <- x - min(x)
  if( max(x)>1 ) x <- x/max(x)
  n <- length(x)
  if(is.null(names(x))) names(x) <- 0:(n-1)
  m <- 1.05
  plot(0, 
       type = 'n', # do not plot anything
       xlim = c(-m,m), ylim = c(-m,m), 
       axes = F, xlab = '', ylab = '', ...)
  a <- pi/2 - 2*pi/200*0:200
  polygon( cos(a), sin(a) )
  v <- .02
  a <- pi/2 - 2*pi/n*0:n
  segments( (1+v)*cos(a), (1+v)*sin(a), 
            (1-v)*cos(a), (1-v)*sin(a) )
  segments( cos(a), sin(a), 
            0, 0, 
            col = 'light grey', lty = 3) 
  ca <- -2*pi/n*(0:50)/50
  for (i in 1:n) {
    a <- pi/2 - 2*pi/n*(i-1)
    b <- pi/2 - 2*pi/n*i
    polygon( c(0, x[i]*cos(a+ca), 0),
             c(0, x[i]*sin(a+ca), 0),
             col=col[i] )
    v <- .1
    text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
  }
}
clock.plot(x, 
           main = "Total number of distinct visitors to the Math Garden for each hour of the day")



#plot number of visitors per weekday
library(plotly)
f <- list(
  family = "Courier New",
  size = 18,
  color = "#7f7f7f"
)
xas <- list(
  title = "Weekday",
  titlefont = f
)
yas <- list(
  title = "Number of distinct users",
  titlefont = f
)
months_users_count_plot <- plot_ly(months_users_count, x = ~ months_users_count$months, y = ~ months_users_count$number_of_distinct_users, type = 'bar', name = 'Number of distinct users per month') %>%
  layout(title = 'Number of distinct users per month', xaxis = xas, yaxis = yas)
api_create(months_users_count_plot, filename = "r-months_users_count_plot")

#to publish plotly plots
Sys.setenv("plotly_username"="y.khan")
Sys.setenv("plotly_api_key"="n1QrWm27JOgkf9TfD0Zj")

#plot number of visitors per month
library(plotly)
f <- list(
  family = "Courier New",
  size = 18,
  color = "#7f7f7f"
)
xas <- list(
  title = "Month",
  titlefont = f
)
yas <- list(
  title = "Number of distinct users",
  titlefont = f
)
months_users_count_plot <- plot_ly(months_users_count, x = ~ months_users_count$months, y = ~ months_users_count$number_of_distinct_users, type = 'bar', name = 'Number of distinct users per month') %>%
  layout(title = 'Number of distinct users per month', xaxis = xas, yaxis = yas)
api_create(months_users_count_plot, filename = "r-months_users_count_plot")

#plot number of visitors per hour
library(plotly)
f <- list(
  family = "Courier New",
  size = 18,
  color = "#7f7f7f"
)
xas <- list(
  title = "Hour of the day",
  titlefont = f
)
yas <- list(
  title = "Number of distinct users",
  titlefont = f
)
number_visitors_hours_plot <- plot_ly(hours_users_count, x = ~ hours_users_count$only_hours, y = ~ hours_users_count$number_of_distinct_users, type = 'bar', name = 'Number of distinct users for each hour of the day') %>%
  layout(title = 'Number of distinct users for each hour of the day', xaxis = xas, yaxis = yas)
api_create(number_visitors_hours_plot, filename = "r-number_visitors_hours_plot")


#plot number of responses per hour
library(plotly)
f <- list(
  family = "Courier New",
  size = 18,
  color = "#7f7f7f"
)
xas <- list(
  title = "Hour of the day",
  titlefont = f
)
yas <- list(
  title = "Number of responses",
  titlefont = f
)
number_responses_hours_plot <- plot_ly(hours_responses_count, x = ~ hours_responses_count$only_hours, y = ~ hours_responses_count$number_of_distinct_users, type = 'bar', name = 'Number of responses for each hour of the day') %>%
  layout(title = 'Number of responses for each hour of the day', xaxis = xas, yaxis = yas)
api_create(number_responses_hours_plot, filename = "r-number_responses_hours_plot")


#plot number of distinct users per day
library(plotly)
f <- list(
  family = "Courier New",
  size = 18,
  color = "#7f7f7f"
)
xas <- list(
  title = "Day",
  titlefont = f
)
yas <- list(
  title = "Number of responses",
  titlefont = f
)
date_users_count_plot <- plot_ly(date_users_count, x = ~ date_users_count$dates, y = ~ date_users_count$number_of_distinct_users, mode = 'lines', name = 'Number of distinct users per day') %>%
  layout(title = 'Number of distinct users per day', xaxis = xas, yaxis = yas)
api_create(date_users_count_plot, filename = "r-date_users_count_plot")




#average correct answer for each unique user
mean_user_id_correct <- aggregate(dat$correct ~ user_id, dat, mean)

#average correct answer and average response time for each unique user
mean_item_id_correct <- aggregate(cbind(dat$correct_answered, dat$response_in_milliseconds/1000) ~ item_id, dat, mean)

#average correct answer and average response time for each unique user
mean_weekdays_correct_response <- aggregate(cbind(dat$correct_answered, dat$response_in_milliseconds/1000) ~ user_id, dat, mean)

#average correct answer and average response time for weekdays
mean_weekdays_correct_response <- aggregate(cbind(dat$correct_answered, dat$response_in_milliseconds/1000) ~ weekdays, dat, mean)
write.csv(mean_weekdays_correct_response, "C:/Users/YKHAN/Documents/Technology for Learning/Project/mean_weekdays_correct_response.csv")

#average correct answer, difficulty and average response time for each unique user
mean_item_id_correct_dif <- aggregate(cbind(dat$correct_answered, dat$response_in_milliseconds/1000,dat$difficulty) ~ item_id, dat, mean)
write.csv(mean_item_id_correct_dif, "C:/Users/YKHAN/Documents/Technology for Learning/Project/mean_item_id_correct_dif.csv")


mean_user_id_response <- aggregate(cbind(dat$correct_answered, dat$response_in_milliseconds/1000) ~ user_id, dat, mean)
mode_difficulty <- aggregate(dat$co)
write.csv(mean_user_id_response, "C:/Users/YKHAN/Documents/Technology for Learning/Project/mean_user_id_response.csv")
mean_user_id_response <- aggregate(cbind(dat$difficulty, dat$response_in_milliseconds/1000) ~ user_id, dat, mean)
write.csv(mean_user_id_response, "C:/Users/YKHAN/Documents/Technology for Learning/Project/mean_user_id_response.csv")


install.packages("car")
library(car)
scatterplot(mean_user_id_response$V1 ~ mean_user_id_response$V2, data=mean_user_id_response,
            xlab="Average response time", ylab="Average correctness of answers", 
            main="Scatter Plot")
library(RColorBrewer)
my_colors <- brewer.pal(nlevels(mean_user_id_response$V3), "Correct answered")
scatterplot(mean_user_id_response$V2 ~ mean_user_id_response$V3, data=mean_user_id_response,
            xlab="Average difficulty level", ylab="Average response time", col = mean_user_id_response$V1,
            main="Scatter Plot")

library(ggplot2)
ggplot2.scatterplot(data=mean_user_id_response, xName='wt',yName='mpg', 
                    addRegLine=TRUE, regLineColor="blue")
#for sessions sample
colnames(sample_sessions)
head(sample_sessions)
sample_sessions <- as.data.frame(sample_sessions)
sample_sessions <- sample_sessions[order(sample_sessions$created),]
summary(sample_sessions)
library(lubridate)
seconds <- sample_sessions$created
sample_sessions$created <- as.POSIXct("09/01/2017 01:37:33", format = "%d/%m/%Y %H:%M:%S") + seconds
weekdays <- weekdays(as.Date(sample_sessions$created))
sample_sessions <- cbind(sample_sessions, weekdays)
head(sample_sessions)

#new_user_domain_modified - counter of domain method if they new
#user domain rating - ELO rating of the user (item - of the item)
head(dat,100)
#analyse clean data 101-500
dat101 <- read.csv(file = "C:/Users/YKHAN/Documents/Technology for Learning/Project/cleandat101_500.csv",header = TRUE)
head(dat101)
dat101 <- dat101[order(dat101$created),]
library(lubridate)
seconds_101 <- dat101$created
dat101$created <- as.POSIXct("09/01/2017 01:37:33", format = "%d/%m/%Y %H:%M:%S") + seconds_101
weekdays101 <- weekdays(as.Date(dat101$created))
dat101 <- cbind(dat101, weekdays101)
head(dat101)
library(ggplot2)
ggplot(data = dat101, mapping = aes(dat101$response_in_milliseconds, dat101$item_id, group = dat101$correct_answered)) + geom_line(aes(col = dat101$correct_answered)) + geom_point(aes(col = dat101$correct_answered)) + labs(title="Item rating per Item ID",x="Item ID", y = "Item rating", colour = "Difficulty")
sample
sample <- dat[sample(1:nrow(dat), 1000, replace=FALSE),]
newdata <- subset(mydata, age >= 20 | age < 10, 
                  select=c(ID, Weight))
as.POSIXct(dat$created = 0, origin = "1970-01-01")
install.packages("lubridate")
boxplot(difficulty~1,data=dat, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")
sapply(dat, function(x) length(unique(x)))
x <- ymd_hms("2017/01/09 01:13:37")

#classification tree
require(rpart)
dat101$response_in_milliseconds <- dat101$response_in_milliseconds/1000
tree101 <- rpart(dat101$correct_answered ~ dat101$response_in_milliseconds, data = dat101, method = "class")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree101)

require(rattle)
# Library
install.packages("dygraphs")
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)


# Read the data
data=dat %>% head(100000)
str(dat)

# Since my time is currently a factor, I have to convert it to a date-time format!
dat101$created = ymd_hms(dat101$created)

# Then you can create the xts format, and thus use dygraph
don <- xts(x = data$response_in_milliseconds, order.by = data$created)
dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

# Dygraph for 101-500
don101 <- xts(x = data101$response_in_milliseconds, order.by = data101$created)
dygraph(don101) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)


#scatter plot

# library
library(ggplot2)

# The dataset is proposed in R
data <- dat %>% head(100000)
str(data)

# Color and shape depend on factor (categorical variable)
ggplot(sample_sessions, aes(x=sample_sessions$item_id, y=sample_sessions$response_in_milliseconds, color=sample_sessions$difficulty)) + geom_point(size=6, alpha=0.6)

#using dplyr - tibble 
library(dplyr)
dat <- as_tibble(dat)

#stacked area chart
library(ggplot2)
ggplot(sample_sessions, aes(x=sample_sessions$correct_answered, y=mean(sample_sessions$correct_answered), fill=sample_sessions$difficulty)) + geom_area()
ggplot(data, aes(x=Year, y=Value, fill=Sector)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Greens", breaks=rev(levels(data$Sector)))

gc()


