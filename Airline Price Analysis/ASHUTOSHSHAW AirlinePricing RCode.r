# Analysis of Airline Ticket Pricing
# NAME: <ASHUTOSH SHAW>
# EMAIL: <ashu.shaw09@gmail.com>
# COLLEGE: <DTU> 




#Setting the working directory 
setwd("F:/studies/Data Analytics Internship/Lecture Datasets")

#Viewing the data frame 
View(air.df)

#Summarizing the data 
summary(air.df)


# Visualizing using plots
boxplot(air.df$PricePremium~air.df$Airline)
boxplot(air.df$PriceEconomy~air.df$Airline)
boxplot(air.df$PricePremium~air.df$TravelMonth)
boxplot(air.df$PriceEconomy~air.df$TravelMonth)


#Visualizing with PRICE_RELATIVE as Y vs Categorical variables
boxplot(air.df$PriceRelative~air.df$TravelMonth) 
boxplot(air.df$PriceRelative~air.df$IsInternational)                    # Huge differnce is observed
boxplot(air.df$PriceRelative~air.df$Aircraft)


#Converting other variables in categories and then visualizing bivariately



x<-factor(air.df$SeatsEconomy)
boxplot(air.df$PriceRelative~x)                        # For seats >180, the realtive price is mostly good.

x1<-factor(air.df$SeatsPremium) 
boxplot(air.df$PriceRelative~x1)                     #No definite pattern

x3<-factor(air.df$PitchEconmoy)
boxplot(air.df$PriceRelative~x3)                   # as pitch increases, the relative price decreases

x2<-factor(air.df$PitchPremium)
boxplot(air.df$PriceRelative~x2)                  # as pitch increases, the relative price increases


x4<-factor(air.df$WidthPremium)              #Width increases, Relative price increases
boxplot(air.df$PriceRelative~x5)


#Scatterplot for non-categorical variables
library(car)
scatterplot(air.df$PriceRelative,air.df$SeatsEconomy)              # As seats increase, the relative price decreases

#Pattern obtained by scatterplot vs (variable) and boxplot vs factor(variable) gives the same inferences


#CORRGRAM
library(corrgram)
corrgram(air.df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)


Co-relation Matrix

#extracting the numeric coloumns
my_data <- air.df[, c(3,6:18)]
res<-cor(my_data)               #making the co-relation matrix
round(res,2)                    #rounding to two digits


cor(flight.df$PriceRelative,flight.df$PitchDifference)                #There is a strong relation between price relative and pitch difference (i.e with increase in price the pitch increases) as the p value is less than 0.05 (p-value < 2.2e-16)

cor.test(flight.df$PriceRelative,flight.df$PitchDifference)




cor(flight.df$PriceRelative,flight.df$WidthDifference)

> cor.test(flight.df$PriceRelative,flight.df$WidthDifference)         #There is a strong relation between price relative and width difference (i.e with increase in price in premium economy the width increases) as the p value is less than 0.05 (p-value < 2.2e-16)
p-value < 2.2e-16

#HYPOTHESIS
#The Increase in Price in Pre,iu, Economy Increases the Pitch Difference and Width Difference
#or in other words with Relative price increase the PitchDifference and WidthDifference increase
 
t.test(air.df$PitchDifference,air.df$PriceRelative )               #p-value is less than 0.05 so we can generate the regression model

#Linear Regression Model 

#Formulate a Regression Model: 
# PREDICTING Relative Price with Pitch Difference and Width Difference
#  PriceRelative: Dependent variable 
#  PitchDiffernece , WidthDifference: Independent variable
#  Model:    PriceRelative = b0 + b1*PitchDiffernece +b2*WidthDifference


lm1<-lm(PriceRelative~PitchDifference+WidthDifference,data=air.df)

summary(lm1)       #Gives us htat both Pitch Difference and Width Difference are significant variables 
            
coefficients(lm1) #getting the intercept and beta coefficients

# Model:    PriceRelative = b0 + b1*PitchDiffernece +b2*WidthDifference
# b0 =  -0.10514235,  b1 =   0.06019158 , b2= 0.11621441 
# Model:    PriceRelative =-0.10514235 + 0.06019158*PitchDiffernece +0.11621441 *WidthDifference



fitted(lm1)

#So the summary tells us that the NULL Hypothesis is false and thus alternate hypothesis is true 


#HYPOTHESIS TWO

# Relative Price of Premium Seats has an inverse relation with Percentage of premium seats
# Relative Price of Premium Seats Increases 

# 7.Run T-Tests appropriate, to test your Hypotheses
#a
t.test(PriceRelative, PercentPremiumSeats, paired=TRUE) 



#Formulate a Regression Model: 
# PREDICTING Relative Price of seats FROM Percent of premium seats
#  PriceRelative: Dependent variable 
#  PercentPremiumSeats: Independent variable
#  Model:    PriceRelative = b0 + b1*PercentPremiumSeats
# The lm() function in R gets (PriceRelative, PercentPremiumSeats) as input
#      and returns beta coefficients {b0, b1} as output


fit <- lm(PriceRelative ~ PercentPremiumSeats, data = air.df)
summary(fit)

# Model:    PriceRelative = b0 + b1*PercentPremiumSeats
# b0 = 0.70,  b1 = -0.015
# Model:    PriceRelative = 0.70 - 0.015*PercentPremiumSeats

#BETA COEFFICIENTS coefficients()
fit$coefficients

# CONFIDENCE INTERVALS (95%)

confint(fit)




 