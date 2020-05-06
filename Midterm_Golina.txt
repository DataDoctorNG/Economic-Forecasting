rm(list=ls()) # clears workspace
#
#
# Nicholas Golina
#
#

#
#
# Mid Term Take Home June 27th and 28th 2017
#This defines the shift function.

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

#
#
#Coding for the midterm_take_home dataset
#Getting the Data
#This imports and views the data. 

midterm_take_home_Data <- read.csv("~/Google Drive/UA/Senior Year/Economic Forecasting/Data/midterm_take_home.csv", header = TRUE)

View(midterm_take_home_Data)

#
#
#This creates time series variables for x and y.

x <- ts(midterm_take_home_Data$x)

x.lag1 <- shift(x, 1)

y <- ts(midterm_take_home_Data$y)

y.lag1 <- shift(y, 1)

plot(x, y, main="Plot of x and y", col="red")

#
#Running the Initial Regression
#This creates the regression equation and summarizes it. The parameter estimate for beta indicates that an 
#increase in x of 1 is associated with an increase in y of 2.28006. The parameter estimate for the intercept indicates that when x
#is 0 y is estimated to be .51453. Both of these estimates are staistically significant at the 1% level.

Reg_Initial <- lm(y ~ x)

summary(Reg_Initial)

#
#Testing for Serial Correlation
#This creates the residuals for the regression and creates a plot of the residuals for x and y. An acf 
#plot is used as well. The plot of the residuals indicates that there might be some alternating positive and negative  
#patterns in the residuals.The acf plot indicates that there could be autocorrelation since the 1st lagged varaiable 
#indicates a breach of the siginificance level.Most likely there will be some 1st order autocorrelation 
#due to the frequent error runs with the residuals. 

TS_Resid_Initial <- ts(Reg_Initial$residuals)
View(TS_Resid_Initial)

ts.plot(TS_Resid_Initial, main="Plot of Residuals as a Time Series for the Initial Regression", col="black")

acf(TS_Resid_Initial)

#
#
#This does the Breusch-Godfrey procedure for the regression.
#The null hypothesis is that there is no autocorrelation for for the error terms.  
#The alternative hypoethsis is that there is autocorrelation for the error terms 
#The LM statistic is 30.56 and the chi squared value is 3.84146. This means that 
#we may reject the null hypothesis that there is no autocorrelation for the error terms. 

xreg <- ts(shift(midterm_take_home_Data$x, 1))
View(xreg)

plot(TS_Resid_Initial, xreg, main="Plot of xreg", col="black")

TS_Resid_Initial.lag1 <- shift(TS_Resid_Initial, 1)

Bruesch_Reg_Initial <- lm(TS_Resid_Initial ~ xreg + TS_Resid_Initial.lag1)

plot(TS_Resid_Initial, ts(midterm_take_home_Data$x), main="Plot of Residuals against x", col="black")

plot(TS_Resid_Initial, TS_Resid_Initial.lag1, main="Plot of Residuals against lagged residuals", col="black")

summary(Bruesch_Reg_Initial)

View(TS_Resid_Initial.lag1)

LMStat_Initial <- 200*(0.1528)

qchisq(.95, 1)

print(LMStat_Initial)

#
#
#This calculates the Durbin Watson Statistic for the initial regression. 
#The statistic is 2.79177. The dl is  1.748 and the du is  1.789. This means that since the 
#stat is greater than 4 - DL (2.252), we can reject the null that there is no autocorrelation 
#and show further evidence that there is negative autocorrelation.

DW_Resid_Initial.lag1 <- lag(TS_Resid_Initial, -1)

DW_Initial <- sum((TS_Resid_Initial - DW_Resid_Initial.lag1)^2, na.rm=TRUE)/sum(TS_Resid_Initial^2)

print(DW_Initial)

#
#Cochrane Orchutt
#This creates the rho value from the cochrane orchutt procedure. The rho value is -.3997451. 

Rho_Initial <- sum((TS_Resid_Initial.lag1*TS_Resid_Initial), na.rm=TRUE)/sum((TS_Resid_Initial.lag1)^2, na.rm = TRUE)

print(Rho_Initial)

#
#
#This creates new variables using the cochrane orchutt method, which are then regressed 
#against each other. The summary indicates that beta has a parameter estimate of 2.56566, which
#means that an increase in x of 1 is associated with an increase in y of 2.56566. The parameter 
#estimate of the intercept is 0.30038. This means that when x is 0, y will be 0.30038.

y2rho <- y + Rho_Initial*(y.lag1)

x2rho <- x + Rho_Initial*(x.lag1)

y2 <- y2rho

x2 <- x2rho

y2.lag1 <- shift(y2, 1)

x2.lag1 <- shift(x2, 1)

Reg2 <- lm(y2 ~ x2)

summary(Reg2)

#
#Re-Testing for Serial Correlation
#This creates the error term and lagged error terms from the new regression.

TS_Resid_2nd <- ts(resid(Reg2))
View(TS_Resid_2nd)

TS_Resid_2nd.lag1 <- shift(TS_Resid_2nd, 1)
View(TS_Resid_2nd.lag1)

#
#
#This shows graphical evidence for the Cochrane Orchutt procedure.The regular plot suggests 
#that there are much more random fluctuations in the patterns of the residuals. The acf plot suggests
#that the negative autocorrelation actually increased. 

ts.plot(TS_Resid_2nd, main="Plot of Residuals as a Time Series for the 2nd Regression", col="purple")

acf(TS_Resid_2nd)

#
#
#This uses the Bruesch Godfrey procedure to calculate the autocorrelation for 
#the amended function. The LM Statistic is 41.72, which means that the cochrane orchutt procedure 
#did not remove the autocorrelation and we may reject the null hypothesis that there is no 
#autocorrelation. 

Bruesch_Reg2 <- lm(TS_Resid_2nd ~ x2[2:200]  + TS_Resid_2nd.lag1)

summary(Bruesch_Reg2)

LMStat_2nd <- 200*(0.2086)

print(LMStat_2nd)

#
#
#This calculates the 2nd Durbin Watson Statistic. The value of the statistic is 2.880076
#which means that 1st order autocorrelation increased to a more negative level and we may reject the
#null hypothesis that there is no autocorrelation. 

DW_Resid_2nd.lag1 <- lag(TS_Resid_2nd, -1)

DW_2nd <- sum((TS_Resid_2nd - DW_Resid_2nd.lag1)^2, na.rm=TRUE)/sum(TS_Resid_2nd^2)

print(DW_2nd)

#
#Putting it All Together
#1. The initial confidence interval for the first regression by subtracting the parameter estimate by the standard error
#was (2.20954, 2.35058) and the confidence interval for the 2nd regression was (2.48979, 2.64153). This means that 
#the beta estimate does not fall within the 95% confidence interval of the regressions, because the beta value is not within 
#the range of the interval.
#2. Theoretically, it would seek to minimize the sum of squared residuals, which would allow for the value of rho to improve 
#with a new set of residuals to use for the cochrane orchutt procedure. This would allow for autocorrelation 
#to improve by that I mean decrease. The procedure on the first regression actually worsened autocorrelation as evidenced
#by comparing the LM stat from the first Breusch Godfrey test to the 2nd.The new beta for x on net moved farther 
#away and is now negative. This means that another iteration might be needed to get a better rho value for differencing. 

#
#
#This creates the confidence intervals for the 1st question. 

Posit_Confidence_Initial <- 2.28006 + 0.07052
Neg_Confidence_Initial <- 2.28006 - 0.07052

print(Posit_Confidence_Initial)
print(Neg_Confidence_Initial)

Posit_Confidence_2nd <- 2.56566 + 0.07587
Neg_Confidence_2nd <- 2.56566 - 0.07587

print(Posit_Confidence_2nd)
print(Neg_Confidence_2nd)

#
#
#This creates the coiterative procedure for the first cochrane orchutt procedure. 

mean_x1 <- mean(x)

mean_y1 <- mean(y)

Intercept1 <- mean_y1 - mean_x1*(2.28006)

New_Residuals1 <- ((y - x*(2.28006)) - Intercept1)

New_Residuals.lag1 <- shift(New_Residuals1, 1)

# 
#
#This calculates the 1st rho and the LM Stat for the procedure. It is 112.54. 

Rho1 <- sum((New_Residuals.lag1*New_Residuals1), na.rm=TRUE)/sum((New_Residuals.lag1)^2, na.rm = TRUE)

print(Rho1)

yrho1 <- y + Rho1*(y.lag1)

xrho1 <- x + Rho1*(x.lag1)

rhoReg1 <- lm(yrho1 ~ xrho1)

summary(rhoReg1)

Bruesch_Reg3 <- lm(New_Residuals1 ~ xrho1  + New_Residuals.lag1)

summary(Bruesch_Reg3)

LMStat_2nd <- 200*(0.5627)

print(LMStat_2nd)
