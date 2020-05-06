rm(list=ls()) # clears workspace
#
#
# Nicholas Golina
#
#

#
#
# Assignment 2 & June 16th, 17th, and 19th 2017
#
#

#
#
#This Defines the Shift Function
shift<-function(x,shift_by){
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  
  if (length(shift_by)>1)
    return(sapply(shift_by,shift, x=x))
  
  out<-NULL
  abs_shift_by=abs(shift_by)
  if (shift_by > 0 )
    out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0 )
    out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out<-x
  out
}

#Coding for y1
#Getting the Data
#This allows for R to import the data and view it
Assignment2 <- read.csv("~/Google Drive/UA/Senior Year/Economic Forecasting/Data/ps2.csv")

View(Assignment2)

#
#Statistical Analysis for y1
#Visual Identification
#This defines all the variables as time series variables

y <- ts(Assignment2$y1)

y2 <- ts(Assignment2$y2)

x <- ts(Assignment2$x1)

x2 <- ts(Assignment2$x2)

x3 <- ts(Assignment2$x3)

#
#
#This defines the lagged variables for future use in the problem set.
#All are lagged once.

y.lag1 <- lag(y, -1)

y2.lag1 <- lag(y2, -1)

x.lag1 <- lag(x, -1)

x2.lag1 <- lag(x2, -1)

x3.lag1 <- lag(x3, -1)

#
#
#This plots the time series variable y1. 
#There are a lot of uneven cyclical fluctuations in y1 over time.
#Due to this, there does not appear to be a trend in the variable.

ts.plot(y, main="Plot of y1 as a time series", col="black")
lines(y.lag1, col="red")

#
#
#This estimates the following equation using OLS: 
#y1,t = β0 + β1x1,t + β2x2,t + εt

OLS <- y ~ x + x2

#
#
#This does a regression with the original equation defined as an object OLS.

reg1 <- lm(OLS)

#
#
#This summarizes the regression from the original equation defined as an object OLS.
#The x1 or x variable is not statistically significant at the 10%, 5%, or the 1% level.
#The parameter estimate for x1 is .517, which means that an increase in x1 of 1 is associated 
#with an increase in y of .517. However, there is no evidence of a causal relationship in this case.
#The x2 variable is statistically significant at the 1% level.
#The parameter estimate for x2 is 1.4267, which means that an increase in x2 of 1 is associated with 
#an increase in y of 1.4267. 

summary(reg1)

#
#
#This creates the residuals for the first regression and turns the residuals into a time series variable.
#A lagged variable was also created for the residuals.

resid <- ts(resid(reg1))

resid.lag1 <- lag(resid, -1)

#
#
#This plots the residuals as a time series.
#serial correlation does not appear to be present because the residuals fluctuate 
#in many different patterns.However, there could the patterns are cyclical for the 
#most part, which means that there could be some instances where serial correlation 
#is present at different points in time. However, that can only be done through more 
#sophisticated techniques such as the acf. 

ts.plot(resid, main="Plot of Residuals as a time series for the first regression", col="black")
lines(resid.lag1, col="red")

#
#
#This creates an acf plot for my residuals. 
#The acf plot indicates that for the most part the acf coefficients fall below
#the line of significance. Howevever there appears to be a major outlier 
#at the beginning of the acf plot that makes positive serial correlation a possible issue for the residuals
#of equation 1.

acf(resid)

#
#Durbin Watson for y1
#This uses a shift function of the residuals to lag the residuals by 1 easier.

ResidShift1 <- shift(resid, 1)

#
#
#This is a manual set of code that replicates the equation for the durbin watson and
#stores the equation into an object DW.

DW <- sum((resid - ResidShift1)^2, na.rm=TRUE)/sum(resid^2)

#
#
#This prints the durbin watson statistic for y1
# With 100 observations at alpha .05 and 2 independent the dl is 1.634. 
#Simultaneously, the dl is 1.715. The DW stat is 1.896958, which means that I fail to reject
# the null hypothesis that P = 0 and thus I have no proof of serial correlation.


print(DW)

#
#
#Coding for y2
#Statistical Analysis for y2
#Visual Identification
#
#This creates a plot for y2.
#For y2 overall there does not appear to be a trend throughout the entire set of time periods. 
#However, for time 40 to 60 there does appear to be a downward trend with vayrying fluctuations.
#This could mean that some autocorrelation could be present, but probably not much. 

ts.plot(y2, main="Plot of y2 as a time series", col="blue")
lines(y2.lag1, col="green")

#
#
#This creates a regression and summarizes the regression for the following equation:
#y2,t =α+β3x3,t +εt.
#The parameter estimate is 1.6242, which means that an increase in x3 of 1 is associated
#with an increase in y2 of 1.6242. This is statistically significant at the 1% level. 

OLS2 <- y2 ~ x3
reg2 <- lm(OLS2)
summary(reg2)

#
#
#This creates the residuals for the 2nd regression and creates a lag function for them of 1 lag.


resid2 <- ts(resid(reg2))
resid2.lag1 <- lag(resid2, -1)

#
#
#This creates a plot for the residuals for the 2nd regression.
#There does not appear to be much of a pattern except a downward trend from time 40 to time 80.
#However, it is very messy, which means there might be a problem with auto correlation. 

ts.plot(resid2, main="Plot of the residuals as a time series for the 2nd regression", col="red")
lines(resid2.lag1, col="purple")

#
#
#This creates an acf plot for the residuals for the 2nd regression.
#Based off of the acf plot, the line of significance was continually violated before the lag of 5.
#Between lag 15 and 20 the line of significance was violated. This means that positive autocorrelation 
#is definitely a problem for the residuals from the 2nd regression. 

acf(resid2)

#
#Durbin Watson for y2
#This creates a shift function to better lag the residuals.

Resid2Shift1 <- shift(resid2, 1)

#
#
#This is the calculation of the DW stat for the 2nd regression and 
#the print function to print it. The DW stat is 0.6599222.
#Since I have 100 observations and one independant variable
# My DU = 1.694 and my DL = 1.654 at the .05 alpha. Since .66 falls below my DL, there is
# evidence to suggest that P>0 indicating a positive serial correlation as was guessed from my
# ACF plot.

DW2 <- sum((resid2 - Resid2Shift1)^2, na.rm=TRUE)/sum(resid2^2)

print(DW2)

#
#Removing AutoCorrelation with a time trend
#This is creates a time variable of the 100 observations present in the dataset. 

time <- (1:100)

#
#
#This creates a 3rd regression with the time component and summarizes it.

OLS3 <- lm(y2 ~ x3 + time)

summary(OLS3)

#
#
#This creates the residuals for the 3rd regression and the shift function 
#for the residuals. 

resid3 <- ts(resid(OLS3))

Resid3Shift1 <- shift(resid3, 1)

#
#
#This creates a DW stat and prints it for the residuals from the 3rd regression.
#The DW Stat was actually lower this time at 0.6642165, which means 
#the time component did nothing to solve for the autocorrelation issue.
#This is even though there were three independent variables with a dl of 
#1.613 and a du of 1.736.

DW3 <- sum((resid3 - Resid3Shift1)^2, na.rm=TRUE)/sum(resid3^2)

print(DW3)

#
#Removing Autocorrelation Through the Hildreth Lu Procedure
#This creates a series of equations and regressions using different values of rho 
#within the boundary of .65<rho<.70. The value of .69 is the value of 
#rho that minimized the sum of squared residuals the most at 1.108.

py265 <- y2 -.65*(y2.lag1)

px365 <- x3 -.65*(x3.lag1)

regrho65 <- lm(py265 ~ px365)

summary(regrho65)

py266 <- y2 -.66*(y2.lag1)

px366 <- x3 -.66*(x3.lag1)

regrho66 <- lm(py266 ~ px366)

summary(regrho66)

py267 <- y2 -.67*(y2.lag1)

px367 <- x3 -.67*(x3.lag1)

regrho67 <- lm(py267 ~ px367)

summary(regrho67)

py268 <- y2 -.68*(y2.lag1)

px368 <- x3 -.68*(x3.lag1)

regrho68 <- lm(py268 ~ px368)

summary(regrho68)

py269 <- y2 -.69*(y2.lag1)

px369 <- x3 -.69*(x3.lag1)

regrho69 <- lm(py269 ~ px369)

summary(regrho69)

py270 <- y2 -.70*(y2.lag1)

px370 <- x3 -.70*(x3.lag1)

regrho70 <- lm(py270 ~ px370)

summary(regrho70)

#
#
#This creates the residuals, shift function for the residuals, and the lag
#function for the residuals for the equation and regression that has a rho value 
#of .69. 

Residrho69 <- ts(resid(regrho69))

Residrho69Shift1 <- shift(Residrho69, 1)

Residrho69.lag1 <- lag(Residrho69, -1)

#
#
#This creates the DW Stat and prints it for the residuals from the hildreth lu procedure.
#The DW Stat is 1.851992 with a dl of 1.654 and a du of 1.694 from 100 observations.
#This means that we caannot reject the null hypothesis that p=0. 
#This means that the procedure removed the autocorrelation.

DW4 <- sum((Residrho69 - Residrho69Shift1)^2, na.rm=TRUE)/sum(Residrho69^2)

print(DW4)

#
#
#This is a plot of the residuals for the equation using rho=.69. 
#The plot looks a lot more random in trends than the other plots from the 2nd and 
#3rd regression. 

ts.plot(Residrho69, main="Plot of the Residuals for the Hildreth Lu Procedure as a time series", col="brown")
lines(Residrho69.lag1, col="black")


