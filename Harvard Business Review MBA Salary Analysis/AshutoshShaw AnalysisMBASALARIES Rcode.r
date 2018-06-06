# Analysis of MBA SALARIES
# NAME: <ASHUTOSH SHAW>
# EMAIL: <ashu.shaw09@gmail.com>
# COLLEGE: <DTU>



#Setting the working directory 
setwd("F:/studies/Data Analytics Internship/Lecture Datasets")

#Reading the data and creating a data frame 
mba.df <-read.csv(paste("MBA.csv",sep=""),)

#Viewing the data-frame
View(mba.df)

#Summarizing the data
summary(mba.df)

#Creating a dataset of the placed students
placed.df<-mba.df[which(mba.df$salary>0),]

#Viewing the dataset
View(placed.df)

#Visualization
plot(placed.df$sex,placed.df$salary)
cor.test(placed.df$salary,placed.df$sex) # Not significant, low correlation

plot(placed.df$work_yrs,placed.df$salary)

#Removing the data of the people who didnt answer to the survey
placed1.df<-placed.df[(placed.df$salary>999),]
View(placed1.df)

plot(placed1.df$work_yrs,placed1.df$salary)
cor.test(placed1.df$work_yrs,placed1.df$salary) #Significant, highly correlated

plot(placed1.df$frstlang,placed1.df$salary)
cor.test(placed1.df$frstlang,placed1.df$salary) #significant, highly correlated


plot(placed1.df$quarter,placed1.df$salary)
cor.test(placed1.df$quarter,placed1.df$salary)  #insignificant,low correlation

plot(placed1.df$satis,placed1.df$salary)
cor.test(placed1.df$satis,placed1.df$salary)    #insignificant, low correlation

plot(placed1.df$f_avg,placed1.df$salary)
plot(placed1.df$f_avg,placed1.df$salary,xlim=c(2,4)) # Observe the values from x=2 to x=4
cor.test(placed1.df$f_avg,placed1.df$salary)     #insignificant

plot(placed1.df$s_avg,placed1.df$salary)
cor.test(placed1.df$f_avg,placed1.df$salary)     #insignificant, low correlation

plot(placed1.df$gmat_tpc,placed1.df$salary)
plot(placed1.df$gmat_tpc,placed1.df$salary,xlim=c(60,100)) # Observe the values from x=60 to x=100
cor.test(placed1.df$gmat_tpc,placed1.df$salary)     #insignificant, low correlation

plot(placed1.df$gmat_vpc,placed1.df$salary)
cor.test(placed1.df$gmat_vpc,placed1.df$salary)   #insignificant, low correlation


plot(placed1.df$gmat_qpc,placed1.df$salary)
cor.test(placed1.df$gmat_qpc,placed1.df$salary)   #insignificant, low correlation

plot(placed1.df$gmat_tot,placed1.df$salary)
cor.test(placed1.df$gmat_tot,placed1.df$salary)   #insignificant, low correlation



boxplot(salary, 
        main="MBA Starting Salaries",
        horizontal=TRUE,
        col=c("yellow"),
        xlab="salary" )

bwplot(work_yrs ~ salary,horizontal=TRUE, 
       xlab = "salary")
bwplot(quarter ~ salary,horizontal=TRUE, 
       xlab = "salary")


#6.Draw Scatter Plots to understand how are the variables correlated pair-wise
#The green line shows the best linear fit The red line shows the best non-linear fit 

#a
scatterplot(quarter,salary, xlab="salary", ylab="quarter")
#b
scatterplot(work_yrs,salary, xlab="work_yrs", ylab="salary")
#c
scatterplot(salary ~ age, data=mba, spread=TRUE, smoother.args=list(lty=2), pch=19,
            main="Salary vs Age of Student", xlab="age", ylab="salary")

#d
scatterplot(salary ~ s_avg, data=mba, spread=TRUE, smoother.args=list(lty=2), pch=19,
            main="Salary vs spring MBA average", xlab="s_avg", ylab="salary")

#7.Draw a Corrgram; Create a Variance-Covariance Matrix
cov(mba)

#Corrgram
corrgram(mba, order=TRUE, 
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt, 
         main="Corrgram of MBA Starting Salaries")


#B.Draw Contingency Tables, as appropriate
#C.Run chi-square tests, as appropriate

## ----(iii) xtabs---------------------------------------------------------
mytable <- xtabs(~ work_yrs+salary, data=placed)
addmargins(mytable)
mytable # frequencies
## ----(iii) Pearson-------------------------------------------------------
chisq.test(mytable)

mytable1 <- xtabs(~ age+salary, data=placed)
mytable1 # frequencies
## ----(iii) Pearson-------------------------------------------------------
chisq.test(mytable1)

mytable2 <- xtabs(~ salary+quarter, data=placed)
addmargins(mytable2)
mytable2 # frequencies
## ----(iii) Pearson-------------------------------------------------------
chisq.test(mytable2)

mytable3 <- xtabs(~ salary+s_avg, data=placed)
addmargins(mytable3)
mytable3 # frequencies
## ----(iii) Pearson-------------------------------------------------------
chisq.test(mytable3)



#D.Run t-tests, as appropriate

#a
t.test(placed$salary,placed$work_yrs, paired=TRUE) 
#b
t.test(placed$age, placed$salary, paired=TRUE) 
#c
t.test(placed$quarter, placed$salary, paired=TRUE)

t.test(placed$s_avg, placed$salary, paired=TRUE)

#p-value is less than 0.05 so we can generate the regression model


#E.Articulate a Hypothesis (or two) that you could test using a Regression Model
# Quarter of MBA students has an inverse relation with salary of MBA students
# Age of MBA student has an inverse relation with salary of MBA students  
#Salary of MBA student increases with spring MBA average

#F.Formulate a Regression Model: 
# PREDICTING starting salary FROM spring MBA average
#  salary: Dependent variable 
#  spring MBA average: Independent variable
#  Model:    salary = b0 + b1*spring MBA average
# The lm() function in R gets (salary, spring MBA average) as input
#      and returns beta coefficients {b0, b1} as output 

fit1 <- lm(salary ~ s_avg, data = placed)
summary(fit1)

# Model:    salary = b0 + b1*spring MBA average
# b0 = 88179,  b1 =4803
# Model:    salary = 88179 + 4803*spring MBA average

#BETA COEFFICIENTS coefficients()
fit1$coefficients

#Lists the predicted values in a fitted model
fitted(fit1)

# CONFIDENCE INTERVALS (95%)
confint(fit1)

# residuals()  Lists the residual values in a fitted model
residuals(fit1)

#Statistical Significance and p-values
summary(fit1)

# The regression coefficient (88179) is significantly dfferent from zero (p < 0.001)

#G. PREDICTING starting salary FROM quartile ranking (1st is top, 4th is bottom)
#  salary: Dependent variable 
#  quarter: Independent variable
#  Model:    salary = b0 + b1*quarter
# The lm() function in R gets (salary,quarter) as input
#      and returns beta coefficients {b0, b1 } as output 

fit2 <- lm(salary ~ quarter, data = placed)
summary(fit2)

#Model:    salary = b0 + b1*quarter 
# b0 = 107668,  b1 = -2050 
# Model:    salary = 107668 -2050*quarter  

#BETA COEFFICIENTS coefficients()
fit2$coefficients

#Lists the predicted values in a fitted model
fitted(fit2)

# CONFIDENCE INTERVALS (95%)
confint(fit2)

# residuals()  Lists the residual values in a fitted model
residuals(fit2)

# Statistical Significance and p-values
summary(fit2)


# PREDICTING starting salary FROM age - in years
#  salary: Dependent variable 
#  age: Independent variable
#  Model:    salary = b0 + b1*age
# The lm() function in R gets (salary, age) as input
#      and returns beta coefficients {b0, b1} as output 

fit <- lm(salary ~ age, data = placed)
summary(fit)

# Model:    salary = b0 + b1*age
# b0 = 29963,  b1 = 2729
# Model:    salary = 29963 + 2729*age

#BETA COEFFICIENTS coefficients()
fit$coefficients

#Lists the predicted values in a fitted model
fitted(fit)

# CONFIDENCE INTERVALS (95%)
confint(fit)

# residuals()  Lists the residual values in a fitted model
residuals(fit)

#Statistical Significance and p-values
summary(fit)



#9.COMPARE THOSE WHO GOT A JOB WITH THOSE WHO DID NOT GET A JOB? IDENTIFY WHY?
# Create a dataframe called notplaced, containing only those students who were not placed.    

notplaced <- mba[ which(mba$salary==0) , ] 


##A. Draw two histograms side-by-side, showing the MBA performance of Placed 
## and Not Placed students

par(mfrow=c(1, 2))
hist(placed$work_yrs, 
     main="For placed students",
     xlab="years of work experience",
     ylab="Count",
     breaks=2,        # more columns 
     col="grey")       # color the bars
hist(notplaced$work_yrs, 
     main="For not placed students",
     xlab="years of work experience",
     ylab="Count",
     breaks=2,        # more columns 
     col="grey")       # color the bars
 par(mfrow=c(1, 1))
###
 
 par(mfrow=c(1, 2))
 hist(placed$s_avg, 
      main="For placed students",
      xlab="spring MBA average",
      ylab="Count",
      breaks=2,        # more columns 
      col="grey")       # color the bars
 hist(notplaced$s_avg, 
      main="For not placed students",
      xlab="spring MBA average",
      ylab="Count",
      breaks=2,        # more columns 
      col="grey")       # color the bars
 par(mfrow=c(1, 1)) 
 
 #B.Draw Contingency Tables, as appropriate
 #C.Run chi-square tests, as appropriate
 
 ## ----(iii) xtabs---------------------------------------------------------
 mytable <- xtabs(~ work_yrs+salary, data=notplaced)
 addmargins(mytable)
 mytable # frequencies
 ## ----(iii) Pearson-------------------------------------------------------
 chisq.test(mytable)
 
 mytable1 <- xtabs(~ age+salary, data=notplaced)
 mytable1 # frequencies
 ## ----(iii) Pearson-------------------------------------------------------
 chisq.test(mytable1)
 
 mytable2 <- xtabs(~ salary+quarter, data=notplaced)
 addmargins(mytable2)
 mytable2 # frequencies
 ## ----(iii) Pearson-------------------------------------------------------
 chisq.test(mytable2)
 
 mytable3 <- xtabs(~ salary+s_avg, data=notplaced)
 addmargins(mytable3)
 mytable3 # frequencies
 ## ----(iii) Pearson-------------------------------------------------------
 chisq.test(mytable3)
 
 ## THE END----------------------------------------------------------