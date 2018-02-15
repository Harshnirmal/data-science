# Project Title:  Hotel room pricing in the Indian market
# NAME: Harsh Nirmal
# EMAIL: harshn52@gmail.com
# COLLEGE / COMPANY: DTU 


#reading the dataset into R
hotels.df <- read.csv(paste("Cities42.csv",sep=""))
View(hotels.df)

#summarizing the data
summary(hotels.df)

#describing data
library(psych)
describe(hotels.df)
attach(hotels.df)
str(hotels.df)

#I choose room rent as Y and starrating, airport , istouristloaction as various independent x

#visualising room rent variable
boxplot(hotels.df$RoomRent,horizontal = TRUE,col=c("red"),main="room rent")

#visualising star rating variable 
table(hotels.df$StarRating)

#visualising airport variable
boxplot(hotels.df$Airport,horizontal = TRUE,col=c("green"))

#visualising the istouristloaction variable
table(hotels.df$IsTouristDestination)

#rent vs rating
plot(hotels.df$StarRating,hotels.df$RoomRent,main="rent vs rating",col=c("green"),xlab = "rating",ylab = "room rent")


#rent vs istouristdestination
plot(hotels.df$IsTouristDestination,hotels.df$RoomRent,main="rent vs is destination",col=c("red"),xlab = "destination",ylab = "room rent")

#rent vs airport
plot(hotels.df$Airport,hotels.df$RoomRent,main="rent vs airport",col=c("purple"),xlab = "airport distance",ylab = "room rent")

#creating corrgram for these 4 variables
myvars <- c("RoomRent", "Airport", "IsTouristDestination","StarRating")
newdata.df <- hotels.df[myvars]
View(newdata.df)
library(corrgram)
corrgram(newdata.df)
corrgram(newdata.df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie,text.panel = panel.txt, main = "Hotel pricing")


#drawing covariance matrix
a <-newdata.df$RoomRent
b <- newdata.df$Airport
c <- newdata.df$IsTouristDestination
d<- newdata.df$StarRating
M <- cbind(a,b,c,d)
cov(M)
var(M)

#checking relation of room rent with if the city is metro or not
plot(hotels.df$IsMetroCity,hotels.df$RoomRent,main="rent vs metro city",col=c("purple"),xlab = "is a metro or not",ylab = "room rent")

#hypothesis H0: the room rent of hotels are not affected by tourist destination.
#H1: the room rent of hotels at tourist destination are higher than not at a tourist destination.
t.test(RoomRent ~ IsTouristDestination)
# as p vale < 0.05 ,we cant reject the null hypothesis and accept our alternative hypothesis.

#H0: the room rent and airport distance have not relation.
#H1: room rent is inversely proportional to airport distance
t.test(RoomRent,Airport)
# as p vale < 0.05 ,we  can't reject the null hypothesis and accept our alternative hypothesis.

#building regression model to do further analysis
library(UsingR)
library(ggplot2)

g = ggplot(hotels.df, aes(x = RoomRent, y = Airport))
g = g + xlab(" rent")
g = g + ylab("airport distance")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g
g + xlim(0, 250000)


#scatterplot matrices along diff parameters
scatterplotMatrix( ~ RoomRent+IsTouristDestination+Airport+ StarRating,data=hotels.df,main="roomrent on diff parameters")

#Analyzing diff parameters of expensive hotels by subsetting the data set by taking roomrent>= median
expensiverooms.df <- subset(hotels.df, RoomRent>= 4000)
View(expensiverooms.df)

#plot to check tourist destination relation ship with room rent
plot(expensiverooms.df$RoomRent,expensiverooms.df$IsTouristDestination)


#plot to check relation between airport distance and room rent of expensive hotels
plot(expensiverooms.df$RoomRent,expensiverooms.df$Airport)

#plot to check relation between star rating and room rent of expensive hotels
plot(expensiverooms.df$RoomRent,expensiverooms.df$StarRating)



model <- lm(RoomRent~IsTouristDestination+Airport+StarRating, data = hotels.df)
print(model)

#getting our regression equation for predicting values
RoomRent = -8942.50+(2051.47)*IsTouristDestination+(15.35)*Airport+(3660.48 )*StarRating

#summary of our model
summary(model)


#creating a model2 with more predictor variables as compared to model
model2 <- lm(RoomRent~IsTouristDestination+Airport+StarRating + HotelCapacity +IsMetroCity + HasSwimmingPool, data = hotels.df)
print(model2)

#regression equation from model 2
RoomRent = -8260.01+(2073.31)*IsTouristDestination+( 11.07)*Airport+( 3584.22 )*StarRating +(-11.25 )* HotelCapacity + (-1502.01)*IsMetroCity + (2211.90 )*HasSwimmingPool


#comparing two models
anova(model,model2)

#found out that model is better than model2