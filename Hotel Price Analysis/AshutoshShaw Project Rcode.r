# Title:  <Indian Hotel Industry>
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



#HYPOTHESIS 1

#The Internal Factors have much greater influence in deciding Room Rent of Hotels than External factors.

#Run t.test to check validity of hypothesis

reg1<-cbind(RoomRent,HasSwimmingPool,HotelCapacity,StarRating) #Internal factors
reg2<-cbind(RoomRent,IsNewYearEve,IsMetroCity,IsTouristDestination,CityRank) #External Factors

#For H1
t.test(reg1,reg2)

#The p-value is less than <0.05 and hence the difference between the variables is significant and hence the null hypothesis can be rejected.
#Formulate  Regression Model: 

# PREDICTING Room Rent FROM Internal Factors
#  Room Rent: Dependent variable 
#  StarRating , HotelCapacity ,HasSwimmingPool: Independent variables
#  Model:    RoomRent = b0 + b1*StarRating + b2*HotelCapacity +b3*HasSwimmingPool
# The lm() function in R gets (RoomRent,  StarRating , HotelCapacity ,HasSwimmingPool) as input
#      and returns beta coefficients {b0, b1,b2,b3} as output 


Model1<-RoomRent ~ StarRating + HotelCapacity +HasSwimmingPool
fit1 <- lm(Model1, data = hotel1.df)
summary(fit1)

# b0 = -6896,  b1 = 3597 ,b2= -15 ,b3=2528
# Model:    RoomRent = b0 + b1*StarRating + b2*HotelCapacity +b3*HasSwimmingPool

#BETA COEFFICIENTS coefficients()
fit1$coefficients

# CONFIDENCE INTERVALS (95%)
confint(fit1)

# PREDICTING Room Rent FROM External Factors
#  Room Rent: Dependent variable 
#  IsNewYearEve,IsMetroCity,IsTouristDestination: Independent variables
#  Model:    RoomRent = b0 + b1*IsNewYearEve+b2*IsMetroCity+b3*IsTouristDestination
# The lm() function in R gets (RoomRent,  IsNewYearEve,IsMetroCity,IsTouristDestination) as input
#      and returns beta coefficients {b0, b1,b2,b3} as output 

Model2<-RoomRent~IsNewYearEve+IsMetroCity+IsTouristDestination
fit2 <- lm(Model2, data = hotel1.df)
summary(fit2)

# Model:    RoomRent = b0 + b1*IsNewYearEve+b2*IsMetroCity+b3*IsTouristDestination
# b0 = 4201,  b1 = 863 ,b2= -1416 ,b3=2171 

#BETA COEFFICIENTS coefficients()
fit2$coefficients

# CONFIDENCE INTERVALS (95%)
confint(fit2)

# MODEL 1 fits better than MODEL 2, as indicated by AIC values

#For Model 1 
AIC(fit1)
#For Model 2
AIC(fit2)

#Thus, Model 1 is our 'best' ordinary least squares model.


#HYPOTHESIS 2
#H-2 -> The average RoomRent for toursit destination is more as comapared to non-tourist destination places 
t.test(hotel1.df$RoomRent[hotel1.df$IsTouristDestination==1],hotel1.df$RoomRent[hotel1.df$IsTouristDestination==0])
#The p-value is less than <0.05 and hence the difference between the variables is significant and hence the null hypothesis can be rejected.


#Regression Model
fit3<-lm(RoomRent~IsTouristDestination , data =hotel1.df)
summary(fit3)
# Model:    RoomRent = b0 + b1*IsTouristDestination
# b0 = 4111.003 ,  b1 = 1957.943
             

#HYPOTHESIS 3
#H-3 -> RoomRent decreases in MetroCity as compared to non-Metrocity
boxplot(hotel1.df$RoomRent,hotel1.df$IsMetroCity,horizontal = TRUE)
t.test(hotel1.df$RoomRent[hotel1.df$IsMetroCity==1],hotel1.df$RoomRent[hotel1.df$IsMetroCity==0])
#The p-value is less than <0.05 and hence the difference between the variables is significant and hence the null hypothesis can be rejected.

fit4<-lm(RoomRent~IsMetroCity , data =hotel1.df)
summary(fit4)
# Model:    RoomRent = b0 + b1*IsMetroCity
# b0 = 5782.794  ,  b1 =-1082.464

#The negative intercept clearly tell that RoomRent is low in MetroCity as compared to non-MetroCity


# For making Table 1: SUMMARY STATISTICS OF INDIAN HOTEL INDUSTRY

Delhi <- hotel1.df[ which(hotel1.df$CityName=="Delhi") , ] 
summary(Delhi)

Mumbai <- hotel1.df[ which(hotel1.df$CityName=="Mumbai") , ] 
summary(Mumbai)

Jaipur <- hotel1.df[ which(hotel1.df$CityName=="Jaipur") , ] 
summary(Jaipur)

Bangalore <- hotel1.df[ which(hotel1.df$CityName=="Bangalore") , ] 
summary(Bangalore)

Goa <- hotel1.df[ which(hotel1.df$CityName=="Goa") , ] 
summary(Goa)

Kochi <- hotel1.df[ which(hotel1.df$CityName=="Kochi") , ] 
summary(Kochi)