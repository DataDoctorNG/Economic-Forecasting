rm(list=ls()) # clears workspace
#
#
# Nicholas Golina
#
#

#
#
# Final Take Home July 13th and July 14th 
#
#

install.packages("forecast")
library("forecast")

#
#
#Funtions to Be Defined

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
    out<- x 
  out
}

#
#
#Getting the Data
#This imports the data and reads it. 

final_take_home1 <- read.csv("~/Google Drive/UA/Senior Year/Economic Forecasting/Data/final_take_home1.csv")
View(final_take_home1)

final_take_home2 <- read.csv("~/Google Drive/UA/Senior Year/Economic Forecasting/Data/final_take_home2.csv")
View(final_take_home2)

#
#
#Time Series yt
#

y <- ts(final_take_home1$y)

plot(y, main= "Plot of y", col="blue")

pacf(y)
pacf(diff(y))

acf(y)
acf(diff(y))

#Evidence from the ACF, Differenced ACF, PACF, and Differenced PACF Plots
#=======================================================================================================#
#The ACF plot demonstrates that the current data set is nonstationary, which might require differencing.#
#The differenced ACF plot causes a quick decay in autocorrelation from 0 to 5 and then oscillates back  #
#and forth. This means it is stationary The partial acf designates that AR could range from 1 to 3. MA  # 
#terms could range from 1 to 6, because of the autocorrelation from the acf plot.                       #
#=======================================================================================================#

#
#
#Testing for the optimal ARIMA Models y

arima11 <- arima(y, order = c(1,1,1), include.mean=FALSE, 
      seasonal = list(order = c(0,0,0)))
summary(arima11)

arima12 <- arima(y, order = c(1,1,2), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima12)

arima13 <- arima(y, order = c(1,1,3), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima13)

arima14 <- arima(y, order = c(1,1,4), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima14)

arima15 <- arima(y, order = c(1,1,5), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima15)

arima16 <- arima(y, order = c(1,1,6), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima16)

arima21 <- arima(y, order = c(2,1,1), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima21)

arima22 <- arima(y, order = c(2,1,2), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima22)

arima23 <- arima(y, order = c(2,1,3), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima23)

arima24 <- arima(y, order = c(2,1,4), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima24)

arima25 <- arima(y, order = c(2,1,5), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima25)

arima26 <- arima(y, order = c(2,1,6), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima26)

arima31 <- arima(y, order = c(3,1,1), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima31)

arima32 <- arima(y, order = c(3,1,2), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima32)

arima33 <- arima(y, order = c(3,1,3), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima33)

arima34 <- arima(y, order = c(3,1,4), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima34)

arima35 <- arima(y, order = c(3,1,5), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima35)

arima36 <- arima(y, order = c(3,1,6), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(arima36)

#
#
#Corrected AIC test for ARIMA(1,1,3), and ARIMA(2,1,2)

print(AIC(arima13) + ((8*5)/(1000 - 4 - 1)))

print(AIC(arima22) + ((8*5)/(1000 - 4 - 1)))

#========================================================================================================#
#When considering parsimony, the MAPE, and AIC, an ARIMA(1,1,3) and ARIMA(2,1,2) works better from the   #                                                                                                     
#other models. The AIC and MAPE for an ARIMA(1,1,3) was 3442.59 and 4.239937. The corrected AIC          #
#was 3442.631.The AIC and MAPE for an ARIMA(2,1,2) model was 3442.58 and 4.242838. The corrected AIC was #
#3442.619. Based on the fact that both are so similiar, I will choose the ARIMA(2,1,2), because the data #
#points are not large, which could neccessitate using the MAPE as a judge. Additionally, the root mean   #
#squared error is smaller for an ARIMA(2,1,2) model at 1.347541 compared to the ARIMA(1,1,3) model at    #
#1.347547. This means that since the root mean squared error can be helpful to avoid large forecasting   #
#error it appears that the ARIMA(2,1,2) is the best model.                                               #
#========================================================================================================#

#
#
#Plot of the Predicted values vs the Observations 

optimal_arima <- arima22

Predicted_arima <- y - optimal_arima$residuals

#
#
#Testing the normality of the residuals
  
plot(density(resid(optimal_arima)))

qqnorm(resid(optimal_arima))

qqline(resid(optimal_arima))

#=========================================================================================================#
#The density plot shows that the ARIMA(2,1,2) model residuals are normally distributed and the Q-Q Plot   #
#shows that few residuals deviate from the line to indicate the solvency of the model. This means that    #
#the model can be used for a prediction and is adequate.                                                                  #
#=========================================================================================================#
  
#
#
#Ploting the Optimal ARIMA Model
#

y_Predicted <- y - optimal_arima$residuals

ts.plot(y, main="Comparing the Observed and Predicted values for y", xlab="Time",
        ylab="Observed and Predicted y", sub="Red = Observed Green = Predicted", col="red")
lines(y_Predicted, col= "green")

#=========================================================================================================#
#When looking at the observed values and predicted values of the model the ARIMA(2,1,2) follows the       #
#observed values for x with slight deviations from red lines representing x.                               #
#=========================================================================================================#

#
#
#Time Series xt
#

x <- ts(final_take_home2$x)

ts.plot(x, main= "Plot of x", col="red")
ts.plot(x[95:110], main= "Plot of x from observation 95 to 110", col="blue")
ts.plot(x[349:355], main= "Plot of x from observation 349 to 355", col="purple")

pacf(x)
pacf(diff(x))

acf(x)
acf(diff(x))

#=========================================================================================================#
#The differenced ACF plot seems to make the data set stationary, but lag 21 to 24 has autocorrelation.    #
#So, a Chow Test might be required. There appears to be a structural break at either 100 or 350, because  #
#a large break seems to occur from previous trends.                                                       #
#=========================================================================================================#

#
#
#Chow Tests

x.lag1 <- shift(x,-1)
x.lag2 <- shift(x,-2)

#
#
#Dummy Tests
dummy_pre <- 1:600

dummy100 <- ifelse(dummy_pre < 100, 0, 1)
x.int100 <- x.lag1*dummy100
reg.int100 <- lm(x ~ -1 + x.lag1 + dummy100 + x.int100, data=final_take_home2)
summary(reg.int100)
x2.int100 <- x.lag2*dummy100
reg2.int100 <- lm(x ~ -1 + x.lag1 + x.lag2 + dummy100 + x2.int100, data=final_take_home2)
summary(reg2.int100)

dummy101 <- ifelse(dummy_pre < 101, 0, 1)
x.int101 <- x.lag1*dummy101
reg.int101 <- lm(x ~ -1 + x.lag1 + dummy101 + x.int101, data=final_take_home2)
summary(reg.int101)
x2.int101 <- x.lag2*dummy101
reg2.int101 <- lm(x ~ -1 + x.lag1 + x.lag2 + dummy101 + x2.int101, data=final_take_home2)
summary(reg2.int101)

dummy102 <- ifelse(dummy_pre < 102, 0, 1)
x.int102 <- x.lag1*dummy102
reg.int102 <- lm(x ~ -1 + x.lag1 + dummy102 + x.int102, data=final_take_home2)
summary(reg.int102)
x2.int102 <- x.lag2*dummy102
reg2.int102 <- lm(x ~ -1 + x.lag1 + x.lag2 + dummy102 + x2.int102, data=final_take_home2)
summary(reg2.int102)

dummy350 <- ifelse(dummy_pre < 350, 0, 1)
x.int350 <- x.lag1*dummy350
reg.int350 <- lm(x ~ -1 + x.lag1 + dummy350 + x.int350, data=final_take_home2)
summary(reg.int350)
x2.int350 <- x.lag2*dummy350
reg2.int350 <- lm(x ~ -1 + x.lag1 + x.lag2 + dummy350 + x2.int350, data=final_take_home2)
summary(reg2.int350)

#=========================================================================================================#
#I tested for 100, 101, and 102, because the structural break could occur at multiple places depending    #
#I only tested at 350, because shortly before 350 there is a brief plateau from the data set based on     #
#the plots above, which means there most likely cannot be a break other than at 350. For the dummy test   #
#at observation 100, the test at 1 lagged term without an intercept indicates that the dummy variable is  #
#statistically significant at the 5% level. The test with 2 lagged terms shows similiar results only the  # 
#p value is .0244, which is larger than the .0123 p value for the previous dummy test. For observation 101#  
#the test with 1 lagged term without an intercept showed that it was statistically significant at the 5%  #
#level with a p value at .0324. The test with 2 lagged terms showed that the dummy variable is only       # 
#statistically significant at the 10% level with a p value of 0.0541. For the test of observation 102     #
#a test with 1 lagged term without an intercept shows that the dummy is statistically significant at the  #
#5% level with a p value of .0367. The test with 2 lagged terms shows that the dummy variable is not      #
#statistically significant. The test with 1 lagged term for observation 350 showed that the dummy variable#
#is statistically significant at the 1% level with a p value of .000309. At the test with 2 lags,         #
#the test is staistically significant at the 5% level with a p value of 0.123. This means that most likely#
#there could be a structural break at observation 350 and maybe at 100 with the tests showing significance#
#so, it is important to look at the F test to determine further evidence of structural breaks.            #
#=========================================================================================================#

#
#
#F Tests

allsum <- lm(x ~ -1 + x.lag1, data=final_take_home2)
allsumsquare_residuals <- sum((allsum$residuals)^2)
allsum2 <- lm(x ~ -1 + x.lag1 + x.lag2, data=final_take_home2)
allsumsquare_residuals2 <- sum((allsum2$residuals)^2)

presum100 <- lm(x[1:99] ~ -1 + x.lag1[1:99], data=final_take_home2)
presumsquare_residuals100 <- sum((presum100$residuals)^2)
presum1002 <- lm(x[1:99] ~ -1 + x.lag1[1:99] + x.lag2[1:99], data=final_take_home2)
presumsquare_residuals1002 <- sum((presum1002$residuals)^2)

postsum100 <- lm(x[100:600] ~ -1 + x.lag1[100:600], data=final_take_home2)
postsumsquare_residuals100 <- sum((postsum100$residuals)^2)
postsum1002 <- lm(x[100:600] ~ -1 + x.lag1[100:600] + x.lag2[100:600], data=final_take_home2)
postsumsquare_residuals1002 <- sum((postsum1002$residuals)^2)

Chow100 <- ((allsumsquare_residuals - presumsquare_residuals100 - postsumsquare_residuals100)/1)/((presumsquare_residuals100 + postsumsquare_residuals100)/(600-2))
print(Chow100)
Chow1002 <- ((allsumsquare_residuals2 - presumsquare_residuals1002 - postsumsquare_residuals1002)/2)/((presumsquare_residuals1002 + postsumsquare_residuals1002)/(600-4))
print(Chow1002)

presum101 <- lm(x[1:100] ~ -1 + x.lag1[1:100], data=final_take_home2)
presumsquare_residuals101 <- sum((presum101$residuals)^2)
presum1012 <- lm(x[1:100] ~ -1 + x.lag1[1:100] + x.lag2[1:100], data=final_take_home2)
presumsquare_residuals1012 <- sum((presum1012$residuals)^2)

postsum101 <- lm(x[101:600] ~ -1 + x.lag1[101:600], data=final_take_home2)
postsumsquare_residuals101 <- sum((postsum101$residuals)^2)
postsum1012 <- lm(x[101:600] ~ -1 + x.lag1[101:600] + x.lag2[101:600], data=final_take_home2)
postsumsquare_residuals1012 <- sum((postsum1012$residuals)^2)

Chow101 <- ((allsumsquare_residuals - presumsquare_residuals101 - postsumsquare_residuals101)/1)/((presumsquare_residuals101 + postsumsquare_residuals101)/(600-2))
print(Chow101)
Chow1012 <- ((allsumsquare_residuals2 - presumsquare_residuals1012 - postsumsquare_residuals1012)/2)/((presumsquare_residuals1012 + postsumsquare_residuals1012)/(600-4))
print(Chow1012)

presum102 <- lm(x[1:101] ~ -1 + x.lag1[1:101], data=final_take_home2)
presumsquare_residuals102 <- sum((presum102$residuals)^2)
presum1022 <- lm(x[1:101] ~ -1 + x.lag1[1:101] + x.lag2[1:101], data=final_take_home2)
presumsquare_residuals1022 <- sum((presum1022$residuals)^2)

postsum102 <- lm(x[102:600] ~ -1 + x.lag1[102:600], data=final_take_home2)
postsumsquare_residuals102 <- sum((postsum102$residuals)^2)
postsum1022 <- lm(x[102:600] ~ -1 + x.lag1[102:600] + x.lag2[102:600], data=final_take_home2)
postsumsquare_residuals1022 <- sum((postsum1022$residuals)^2)

Chow102 <- ((allsumsquare_residuals - presumsquare_residuals102 - postsumsquare_residuals102)/1)/((presumsquare_residuals102 + postsumsquare_residuals102)/(600-2))
print(Chow102)
Chow1022 <- ((allsumsquare_residuals2 - presumsquare_residuals1022 - postsumsquare_residuals1022)/2)/((presumsquare_residuals1022 + postsumsquare_residuals1022)/(600-4))
print(Chow1022)

presum350 <- lm(x[1:349] ~ -1 + x.lag1[1:349], data=final_take_home2)
presumsquare_residuals350 <- sum((presum350$residuals)^2)
presum3502 <- lm(x[1:349] ~ -1 + x.lag1[1:349] + x.lag2[1:349], data=final_take_home2)
presumsquare_residuals3502 <- sum((presum3502$residuals)^2)

postsum350 <- lm(x[350:600] ~ -1 + x.lag1[350:600], data=final_take_home2)
postsumsquare_residuals350 <- sum((postsum350$residuals)^2)
postsum3502 <- lm(x[350:600] ~ -1 + x.lag1[350:600] + x.lag2[350:600], data=final_take_home2)
postsumsquare_residuals3502 <- sum((postsum3502$residuals)^2)

Chow350 <- ((allsumsquare_residuals - presumsquare_residuals350 - postsumsquare_residuals350)/1)/((presumsquare_residuals350 + postsumsquare_residuals350)/(600-2))
print(Chow350)
Chow3502 <- ((allsumsquare_residuals2 - presumsquare_residuals3502 - postsumsquare_residuals3502)/2)/((presumsquare_residuals3502 + postsumsquare_residuals3502)/(600-4))
print(Chow3502)

#=========================================================================================================#
#For the f tests the f value to beat for the regressions using the full model with 1 lagged term is       #
#3.85705564, while the f value for 2 lagged terms is 3.01084060. The F test for observation 100 gets an   #
#f value for the model with 1 lagged term of .0003476941, while test with 2 lagged terms has an f value of#
#5.801633. The f test for observation 101 has an f value of 0.5559508, while the test with 2 lagged terms #
#had an f value of 4.107691. The f test for observation 102 has an f value of .656308 for a test with 1   #
#lagged term. For a test with 2 lagged terms the f value is 8.098132. For the 350th observation a test    #
#with 1 lagged term, the f value is 15.90828. For the test with 2 lagged terms the f value is 13.66991    #
#With this evidence we may reject the null hypothesis that there is not a structural break at observation #
#350 while we cannot definitively do so for observation 100, 101, and 102 due to conflicting statistics.  #
#We can therefore from here on out use the data at and above 350 to find our model.                       #
#=========================================================================================================#

#
#
#Testing for the optimal ARIMA Models x

Alt_x <- x[350:600]

pacf(Alt_x)
pacf(diff(Alt_x))

acf(Alt_x)
acf(diff(Alt_x))

Alt_arima10 <- arima(x[350:600], order = c(1,1,0), include.mean=FALSE, 
                 seasonal = list(order = c(0,0,0)))
summary(Alt_arima10)

Alt_arima60 <- arima(x[350:600], order = c(6,1,0), include.mean=FALSE, 
                     seasonal = list(order = c(0,0,0)))
summary(Alt_arima50)

Alt_arima210 <- arima(x[350:600], order = c(21,1,0), include.mean=FALSE, 
                     seasonal = list(order = c(0,0,0)))
summary(Alt_arima210)

Alt_optimal_arima <- Alt_arima10

Predicted_x_arima <- x[350:600] - Alt_optimal_arima$residuals

AIC_Corrected_arima10 <- (AIC(Alt_optimal_arima) + ((2*2)/(600 - 2)))
print(AIC_Corrected_arima10)

AIC_Corrected_arima50 <- (AIC(Alt_arima60) + ((10*6)/(600 - 5 - 1)))
print(AIC_Corrected_arima50)

#=========================================================================================================#
#The differenced ACF indicates some evidence of stationarity from observation 350 to 600. However, since  #
#the ACF looks similiar to the regular ACF for all of x, it might be becasue the data is a random walk    #
#with trend that just cannot be made fully stationary with the current patterns.Since the only high       #
#autocorrelation is at lag 1,6, and 21 I will only be testing AR models with those parameters.            #
#When taking parsimony into consideration, the ARIMA(1,1,0) is the best model because the aic is 685.11,  #
#which is lower than an ARIMA(6,1,0) model at 691.66 aic. While the MAPE and root mean squared errors are #
#lower for ARIMA(6,1,0) and ARIMA(21,1,0) models than an ARIMA(1,1,0), the differences are very minut.    #
#For example the MAPE for an ARIMA(1,1,0) is 6.369387, while an ARIMA(6,1,0) model MAPE is 6.339741. That #
#is not much of an improvement and it is when parsimony should be preferred. 
#=========================================================================================================#

#
#
#Testing the normality of the residuals

plot(density(resid(Alt_optimal_arima)))

qqnorm(resid(Alt_optimal_arima))

qqline(resid(Alt_optimal_arima))

#=========================================================================================================#
#The density plot of the residuals of the model shows that they are normally distributed and the Q-Q plot #
#shows that the residuals are almost all staying within the boundary of adequacy. This means that the     #
#model can be used for forecasting.                                                                       #
#=========================================================================================================#

#
#
#Ploting the Optimal ARIMA Model

ts.plot(x[350:600], main="Comparing the Observed and Predicted values for x", 
        xlab="Time", ylab="Observed and Predicted x", sub="Blue = Observed Brown = Predicted", col="Blue")
lines(Predicted_x_arima, col= "Brown")

#=========================================================================================================#
#The plot of x from 350 to 600 and the predicted values of the ARIMA(1,1,0) show that the model is        #
#very accurate in predicting the patterns of x.                                                                                       #
#=========================================================================================================#





