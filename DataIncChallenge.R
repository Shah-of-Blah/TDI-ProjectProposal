
#We first read in the data. 
data = read.csv("Arrest_Data_from_2010_to_Present.csv")
data = data.frame(data)

#I noticed the dates are not in the easiest format, so we change them. 
Arrest.Date = data$Arrest.Date
Arrest.Date = sapply(Arrest.Date, toString)

Arrest.Date = as.Date(c(Arrest.Date),"%m/%d/%Y") #converts to yyyy-mm-dd

#We drop the old Arrest.Date column and add our new one. 
drops = c("Arrest.Date")
data = data[ , !(names(data) %in% drops)]
Arrest.Date = data.frame(Arrest.Date)
data = cbind(data, Arrest.Date)

#Now we can get the data strictly from 2018. 
data_2018  = data[data$Arrest.Date >= "2018-01-01" & data$Arrest.Date <= "2018-12-31",]

#################################################################################################

#We now have the answer to Question #1 How many bookings were there in 2018?
#answer:104277
dim(data_2018)[1]

#################################################################################################

#Question 2: How many bookings were made in the area with the most arrests?
#answer: 10951 in Area Central
summary(data_2018$Area.Name)

#################################################################################################
#Question 3: get the 95% for age for vehicle theft, robbery, burglery, and Receive
#Stolen Property for 2018.

data_2018_Q3 = data_2018[data_2018$Charge.Group.Description == "Vehicle Theft" | data_2018$Charge.Group.Description == "Robbery" | data_2018$Charge.Group.Description == "Burglary" | data_2018$Charge.Group.Description == "Receive Stolen Property",]
#answer:52
quantile(data_2018_Q3$Age,.95)

data_2018_Q3 = NULL

#Question 4: Z-scores for age.###################################################################
Age = data_2018$Age
Age = data.frame(Age)

Charge.Group.Description = data_2018$Charge.Group.Description
Charge.Group.Description = data.frame(Charge.Group.Description)

data_2018_Q4 = cbind(Age, Charge.Group.Description)
data_2018_Q4 = data_2018_Q4[data_2018_Q4$Charge.Group.Description != "Pre-Delinquency" & data_2018_Q4$Charge.Group.Description != "Non-Criminal Detention" & data_2018_Q4$Charge.Group.Description != "",]

Age.Against = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Against Family/Child",][,1]
Age.Assault = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Aggravated Assault",][,1]
Age.Burglary = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Burglary",][,1]
Age.Disorderly = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Disorderly Conduct",][,1]
Age.Disturbing = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Disturbing the Peace",][,1]
Age.Driving = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Driving Under Influence",][,1]
Age.Drunkeness = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Drunkeness",][,1]
Age.Federal = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Federal Offenses",][,1]
Age.Forgery = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Forgery/Counterfeit",][,1]
Age.Fraud = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Fraud/Embezzlement",][,1]
Age.Gambling = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Gambling",][,1]
Age.Homicide = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Homicide",][,1]
Age.Larceny = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Larceny",][,1]
Age.Liquor = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Liquor Laws",][,1]
Age.Misc = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Miscellaneous Other Violations",][,1]
Age.Moving = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Moving Traffic Violations",][,1]
Age.Narcotic = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Narcotic Drug Laws",][,1]
Age.Other = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Other Assaults",][,1]
Age.Prostitution = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Prostitution/Allied",][,1]
Age.Rape = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Rape",][,1]
Age.Receive = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Receive Stolen Property",][,1]
Age.Robbery = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Robbery",][,1]
Age.Sex = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Sex (except rape/prst)",][,1]
Age.Vehicle = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Vehicle Theft",][,1]
Age.Weapon = data_2018_Q4[data_2018_Q4$Charge.Group.Description == "Weapon (carry/poss)",][,1]

ave.ages = c(mean(Age.Against), mean(Age.Assault), mean(Age.Burglary), mean(Age.Disorderly),
             mean(Age.Disturbing), mean(Age.Driving), mean(Age.Drunkeness), mean(Age.Federal),
             mean(Age.Forgery), mean(Age.Fraud), mean(Age.Gambling), mean(Age.Homicide),
             mean(Age.Larceny), mean(Age.Liquor), mean(Age.Misc), mean(Age.Moving), mean(Age.Narcotic),
             mean(Age.Other), mean(Age.Prostitution), mean(Age.Rape), mean(Age.Receive),
             mean(Age.Robbery), mean(Age.Sex), mean(Age.Vehicle), mean(Age.Weapon))

z_scores = scale(ave.ages)

#Finally we have the answer to question #4
#answer = 2.12786993443536
print(max(abs(min(z_scores)),max(z_scores)),digits=15)

data_2018_Q4 = NULL

#Question #5.########################################################################## 

#remove the data with unknown location. 
data_2018_Q5 = data_2018[data_2018$Location != "(0.0, 0.0)", ]

Location = data_2018_Q5$Location
Location = sapply(Location, toString)

Latitude = rep(0,length(Location))
for (i in 1:length(Location))
{
  end = gregexpr(pattern=',',Location[i])[[1]][1]
  Latitude[i] = as.double(substr(Location[i], 2, end-1))
}

Longitude =  rep(0,length(Location))
for (i in 1:length(Location))
{
  start = gregexpr(pattern=',',Location[i])[[1]][1]
  end = nchar(Location[i])
  Longitude[i] = as.double(substr(Location[i], start+2, end-2))
}

DistanceFromBradbury = rep(0,length(Location))
for (i in 1:length(Location))
{
  phi_m = mean(Latitude[i],34.050536)*(pi/180)
  DelPhi = (Latitude[i]-34.050536)*(pi/180)
  DelLambda = (Longitude[i]+118.247861)*(pi/180)
  DistanceFromBradbury[i] = 6371*sqrt(DelPhi^2 + (cos(phi_m)*DelLambda)^2)
}

#So we have a solution to question #5
#answer = 11494
length(DistanceFromBradbury[DistanceFromBradbury <= 2])

data_2018_Q5 = NULL

############################################################################################
# Question #6

#We'll need more Lat/Long data for this problem, so let's include it in our data set. 

Location = data_2018$Location
Location = sapply(Location, toString)

Latitude = rep(0,length(Location))
for (i in 1:length(Location))
{
  end = gregexpr(pattern=',',Location[i])[[1]][1]
  Latitude[i] = as.double(substr(Location[i], 2, end-1))
}

Longitude =  rep(0,length(Location))
for (i in 1:length(Location))
{
  start = gregexpr(pattern=',',Location[i])[[1]][1]
  end = nchar(Location[i])
  Longitude[i] = as.double(substr(Location[i], start+2, end-2))
}

Address = data_2018$Address
Address = sapply(Address, toString)

Pico = rep(FALSE, length(Address))
for (i in 1:length(Address))
{
  start = gregexpr(pattern='PICO',Address[i])[[1]][1]
  if (start != -1) {
    Pico[i] = TRUE
  }
}

Address = data.frame(Address)
Latitude = data.frame(Latitude)
Longitude = data.frame(Longitude)
Pico = data.frame(Pico)
data_2018_Q6 = cbind(Address,Latitude,Longitude,Pico)

#We now have a data set with just the Pico arrests.
data_2018_Q6 =  data_2018_Q6[data_2018_Q6$Pico == TRUE,]

#We now filter out outliers that beyond 2 stdev of the mean in both lat and long. 
scaled_latitude = scale(data_2018_Q6$Latitude)
scaled_longitude = scale(data_2018_Q6$Longitude)

scaled_latitude = data.frame(scaled_latitude)
scaled_longitude = data.frame(scaled_longitude)

data_2018_Q6 = cbind(data_2018_Q6, scaled_latitude, scaled_longitude)
data_2018_Q6 =  data_2018_Q6[data_2018_Q6$scaled_latitude <= 2 & data_2018_Q6$scaled_latitude >= -2,]
data_2018_Q6 =  data_2018_Q6[data_2018_Q6$scaled_longitude <= 2 & data_2018_Q6$scaled_longitude >= -2,]

#Now the data is we need to calculate the length of Pico Boulevard. The western most point will 
#have the minimum of longitude, and the eastern most is the maximum of longitute. 
West =  data_2018_Q6[data_2018_Q6$Longitude == min(data_2018_Q6$Longitude),]
East = data_2018_Q6[data_2018_Q6$Longitude == max(data_2018_Q6$Longitude),]

phi_m = mean(West$Latitude[1],East$Latitude[1])*(pi/180)
DelPhi = (West$Latitude[1]-East$Latitude[1])*(pi/180)
DelLambda = (West$Longitude[1]-East$Longitude[1])*(pi/180)
Length_of_Pico = 6371*sqrt(DelPhi^2 + (cos(phi_m)*DelLambda)^2)
Arrests_per_km_Pico = (dim(data_2018_Q6)[1])/Length_of_Pico
print(Arrests_per_km_Pico, digits = 15)

data_2018_Q6 = NULL

############################################################################################
# Question 7#

data_Q7 =  data[data$Arrest.Date <= "2018-12-31" & data$Charge.Group.Code != 99 & is.na(data$Charge.Group.Code) == FALSE ,]

#We now find the proportion of each charge group across the entire city. 
max(data_Q7$Charge.Group.Code)

citywide_dist = rep(0,29)
total = dim(data_Q7)[1]
for (i in 1:29)
{
  citywide_dist[i] = (dim(data_Q7[data_Q7$Charge.Group.Code == i,])[1])/total
  
}

#now i build a matrix where each row corresponds to an areaID and each column
#corresponds to a Charge.Group.Code
max(data_Q7$Area.ID)
Area_by_Charge = matrix(0,21,29)

for (i in 1:21)
{
  total = dim(data_Q7[data_Q7$Area.ID == i ,])[1]
  for (j in 1:29)
  {
    Area_by_Charge[i,j] = ((dim(data_Q7[data_Q7$Charge.Group.Code == j & data_Q7$Area.ID == i ,])[1])/total)/citywide_dist[j]
  }
}

#We now have a solution to Question 7
#answer = 3.5150763798652
print(mean(sort(as.vector(Area_by_Charge), decreasing = TRUE)[1:5]), digits = 15)

data_2018_Q7 = NULL

############################################################################################
# Question 8#
data_Q8 =  data[data$Arrest.Date <= "2018-12-31" & data$Arrest.Type.Code == "F",]
Felony = c(rep(0,9))
Felony[1] = dim(data_Q8[data_Q8$Arrest.Date > "2009-12-31" & data_Q8$Arrest.Date <= "2010-12-31",])[1]
Felony[2] = dim(data_Q8[data_Q8$Arrest.Date > "2010-12-31" & data_Q8$Arrest.Date <= "2011-12-31",])[1]
Felony[3] = dim(data_Q8[data_Q8$Arrest.Date > "2011-12-31" & data_Q8$Arrest.Date <= "2012-12-31",])[1]
Felony[4] = dim(data_Q8[data_Q8$Arrest.Date > "2012-12-31" & data_Q8$Arrest.Date <= "2013-12-31",])[1]
Felony[5] = dim(data_Q8[data_Q8$Arrest.Date > "2013-12-31" & data_Q8$Arrest.Date <= "2014-12-31",])[1]
Felony[6] = dim(data_Q8[data_Q8$Arrest.Date > "2014-12-31" & data_Q8$Arrest.Date <= "2015-12-31",])[1]
Felony[7] = dim(data_Q8[data_Q8$Arrest.Date > "2015-12-31" & data_Q8$Arrest.Date <= "2016-12-31",])[1]
Felony[8] = dim(data_Q8[data_Q8$Arrest.Date > "2016-12-31" & data_Q8$Arrest.Date <= "2017-12-31",])[1]
Felony[9] = dim(data_Q8[data_Q8$Arrest.Date > "2017-12-31" & data_Q8$Arrest.Date <= "2018-12-31",])[1]

Year = c(2010:2018)

summary(lm(Felony~Year))
regression = lm(Felony~Year)

plot(Year, Felony, xlab ="Year", ylab = "Felony Arrests")
abline(regression)

#We see in the summary of the linear model that the y-int is 4945182.86 and the slope is -243395. 
#Thus our prediction for 2019 felony crimes is 31037.81
felony_pred = -2433.95*2019 + 4945182.86
felony_pred

data_Q8 = NULL
