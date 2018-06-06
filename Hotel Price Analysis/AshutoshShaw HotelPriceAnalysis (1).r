# Project Title:  <Hotel Room Pricing In The Indian Market>
# NAME: <ASHUTOSH SHAW>
# EMAIL: <ashu.shaw09@gmail.com>
# COLLEGE: <DTU



setwd("F:/studies/Data Analytics Internship/Lecture Datasets")
hotel.df <-read.csv(paste("Cities42.csv",sep=""),)
summary(hotel.df)

#Omitting the not available data
hotel1.df<-na.omit(hotel.df)

cor.test(hotel.df$RoomRent,hotel.df$IsNewYearEve)   #low-co-relation

cor.test(hotel.df$RoomRent,hotel.df$IsMetroCity)    #low-co-relation

cor.test(hotel.df$RoomRent,hotel.df$HasSwimmingPool) #highly co-related

cor.test(hotel.df$RoomRent,hotel.df$StarRating)  #highly co-related

cor.test(hotel.df$RoomRent,hotel.df$HotelCapacity) #highly co-related

#Co-relation matrix 

my_data <- hotel1.df[, c(3:7,10:12,16:19)]
> res<-cor(my_data)
> round(res,2)

library(Boruta)
set.seed(123)
response<-hotel.df$RoomRent
bor.results<-Boruta(RoomRent~.,data=hotel1.df)
city1.df<-na.omit(hotel.df)

# 3 Rejected and 13 termed as significant
# HasSwimmingpool,StarRating and HotelCapacity are amongst the significant terms

#Identify the Dependent Variable(s) (i.e. the Y in the Y = F(x)) in your dataset
### Y= RoomRent

#Identify the three most important Independent variables (i.e. x1, x2, x3) in your dataset.
### X1=HasSwimmingPool, X2=StarRating ,X3=HotelCapacity

#Visualize Y, x1, x2, x3 individually. Ignore other variables for now.

# X1 categorical,  create a table() for it

mytable<-xtabs(~RoomRent+HasSwimmingPool , data =hotel1.df)      # Does affect the roomrent
mytable


#x2 , x3 is continuous, draw a Box Plot for it.
library(lattice)
bwplot(hotel1.df$RoomRent~hotel1.df$StarRating                   #Surely an important factor
bwplot(hotel1.df$RoomRent~hotel1.df$HotelCapacity)               #As room increase the price decreases


scatterplot(hotel1.df$RoomRent~hotel1.df$HotelCapacity)
scatterplot(hotel1.df$RoomRent~hotel1.df$HasSwimmingPool)
scatterplot(hotel1.df$RoomRent~hotel1.df$StarRating)


#Draw Scatter Plots to understand how are the variables correlated pair-wise

scatterplotMatrix(
     hotel1.df[
         ,c("RoomRent","HasSwimmingPool","StarRating","HotelCapacity")], 
     spread=TRUE, smoother.args=list(lty=2),
     main="Scatter Plot Matrix")


#Draw a Corrgram of Y, x1, x2, x3  (Ignore other variables for now) 

hotel2 <- c("RoomRent","HasSwimmingPool","StarRating","HotelCapacity")
 corrgram( hotel1.df[,hotelpricing2], order=TRUE,
           main="Hotel Pricing in India",
           lower.panel=panel.pts, upper.panel=panel.pie,
           diag.panel=panel.minmax, text.panel=panel.txt)


#Create a Variance-Covariance Matrix for Y, x1, x2, x3
my_data1 <- hotel1.df[, c(10,11 , 18 ,19)]
res<-cor(my_data1)
round(res,2)


