rm(list=ls()) # clears workspace
#
#
# Nicholas Golina
#
#

#
#
# Project Coding July 2nd, July 9th, 
#
#
install.packages("forecast")
library(forecast)

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
#This imports the data and creates plots for the wages variable. 

Project_Data <- read.csv("~/Google Drive/UA/Senior Year/Economic Forecasting/Data/Project_Data .csv")
View(Project_Data)

Wages <- ts(Project_Data$Median_Wages, frequency= 4)

ts.plot(Wages, main="Plot of the Median Wages Variable", col = "red", ylab="Median Wages in CPI Adjusted Dollars", sub ="Figure 1")
ts.plot(diff(Wages), main="Plot of the Differenced Median Wages Variable", col = "blue", ylab="Differenced Median Wages in CPI Adjusted Dollars", sub ="Figure 2")

pacf(Wages, main= "PACF of Real Median Weekly Earnings")
pacf(diff(Wages), main= "PACF of Real Median Weekly Earnings Differenced")

acf(Wages, main= "ACF of Real Median Weekly Earnings")
acf(diff(Wages), main= "ACF of Real Median Weekly Earnings Differenced")

#
#
#Structural Break Tests
#The structural break is at observation 80

ts.plot(Wages[1:153], main="Plot of Real Median Weekly Earnings", col = "red", ylab="Median Weekly Earnings in CPI Adjusted Dollars", sub ="Figure 3")
ts.plot(Wages[50:100], main="Testing for the Structural Break", col = "red", ylab="Median Weekly Earnings in CPI Adjusted Dollars", sub ="Figure 2")
ts.plot(Wages[75:85], main="Testing for the Structural Break", col = "red", ylab="Median Weekly Earnings in CPI Adjusted Dollars", sub ="Figure 2")
ts.plot(Wages[80:153], main="Data Post Structural Break", col = "red", ylab="Median Weekly Earnings in CPI Adjusted Dollars", sub ="Figure 7")

#
#
#Dummy Tests

dummy_pre <- 1:153
x.lag1 <- shift(Wages,-1)
x.lag2 <- shift(Wages,-2)

dummy <- ifelse(dummy_pre < 80, 0, 1)
x.int20 <- x.lag1*dummy
reg2.int2 <- lm(Wages ~ -1 + x.lag1 + dummy + x.int20, data=Project_Data)
summary(reg2.int2)
x.int200 <- x.lag2*dummy
reg2.int20 <- lm(Wages ~ -1 + x.lag1 + x.lag2 + dummy + x.int200, data=Project_Data)
summary(reg2.int20)

dummy2 <- ifelse(dummy_pre < 81, 0, 1)
x.int21 <- x.lag1*dummy2
reg1.int3 <- lm(Wages ~ -1 + x.lag1 + dummy2 + x.int21, data=Project_Data)
summary(reg1.int3)
x.int210 <- x.lag2*dummy2
reg1.int30 <- lm(Wages ~ -1 + x.lag1 + x.lag2 + dummy2 + x.int210, data=Project_Data)
summary(reg1.int30)

dummy3 <- ifelse(dummy_pre < 82, 0, 1)
x.int22 <- x.lag1*dummy3
reg1.int4 <- lm(Wages ~ -1 + x.lag1 + dummy3 + x.int22, data=Project_Data)
summary(reg1.int4)
x.int220 <- x.lag2*dummy3
reg1.int40 <- lm(Wages ~ -1 + x.lag1 + x.lag2 + dummy3 + x.int220, data=Project_Data)
summary(reg1.int40)

#
#
#F Tests

allsum <- lm(Wages ~ x.lag1, data=Project_Data)
allsumsquare_residuals <- sum((allsum$residuals)^2)
allsum2 <- lm(Wages ~ x.lag1 + x.lag2, data=Project_Data)
allsumsquare_residuals2 <- sum((allsum2$residuals)^2)

presum2 <- lm(Wages[1:79] ~ x.lag1[1:79], data=Project_Data)
presumsquare_residuals2 <- sum((presum2$residuals)^2)
presum22 <- lm(Wages[1:79] ~ x.lag1[1:79] + x.lag2[1:79], data=Project_Data)
presumsquare_residuals22 <- sum((presum22$residuals)^2)

postsum2 <- lm(Wages[80:153] ~ x.lag1[80:153], data=Project_Data)
postsumsquare_residuals2 <- sum((postsum2$residuals)^2)
postsum22 <- lm(Wages[80:153] ~ x.lag1[80:153] + x.lag2[80:153], data=Project_Data)
postsumsquare_residuals22 <- sum((postsum22$residuals)^2)

Chow20 <- ((allsumsquare_residuals - presumsquare_residuals2 - postsumsquare_residuals2)/2)/((presumsquare_residuals2 + postsumsquare_residuals2)/(153-4))
print(Chow20)
Chow202 <- ((allsumsquare_residuals2 - presumsquare_residuals22 - postsumsquare_residuals22)/3)/((presumsquare_residuals22 + postsumsquare_residuals22)/(153-6))
print(Chow202)

presum3 <- lm(Wages[1:80] ~ x.lag1[1:80], data=Project_Data)
presumsquare_residuals3 <- sum((presum3$residuals)^2)
presum32 <- lm(Wages[1:80] ~ x.lag1[1:80] + x.lag2[1:80], data=Project_Data)
presumsquare_residuals32 <- sum((presum32$residuals)^2)

postsum3 <- lm(Wages[81:153] ~ x.lag1[81:153], data=Project_Data)
postsumsquare_residuals3 <- sum((postsum3$residuals)^2)
postsum32 <- lm(Wages[81:153] ~ x.lag1[81:153] + x.lag2[81:153], data=Project_Data)
postsumsquare_residuals32 <- sum((postsum32$residuals)^2)

Chow21 <- ((allsumsquare_residuals - presumsquare_residuals3 - postsumsquare_residuals3)/2)/((presumsquare_residuals3 + postsumsquare_residuals3)/(153-4))
print(Chow21)
Chow212 <- ((allsumsquare_residuals2 - presumsquare_residuals32 - postsumsquare_residuals32)/3)/((presumsquare_residuals32 + postsumsquare_residuals32)/(153-6))
print(Chow212)

presum4 <- lm(Wages[1:81] ~ x.lag1[1:81], data=Project_Data)
presumsquare_residuals4 <- sum((presum4$residuals)^2)
presum42 <- lm(Wages[1:81] ~ x.lag1[1:81] + x.lag2[1:81], data=Project_Data)
presumsquare_residuals42 <- sum((presum42$residuals)^2)

postsum4 <- lm(Wages[82:153] ~ x.lag1[82:153], data=Project_Data)
postsumsquare_residuals4 <- sum((postsum4$residuals)^2)
postsum42 <- lm(Wages[82:153] ~ x.lag1[82:153] + x.lag2[82:153], data=Project_Data)
postsumsquare_residuals42 <- sum((postsum42$residuals)^2)

Chow22 <- ((allsumsquare_residuals - presumsquare_residuals4 - postsumsquare_residuals4)/2)/((presumsquare_residuals4 + postsumsquare_residuals4)/(153-4))
print(Chow22)
Chow222 <- ((allsumsquare_residuals2 - presumsquare_residuals42 - postsumsquare_residuals42)/3)/((presumsquare_residuals42 + postsumsquare_residuals42)/(153-6))
print(Chow222)

#
#
#Dividing the data from the structural break

Wages1 <- Wages[1:79]

Wages2 <- Wages[80:153]

acf(Wages1)

acf(Wages2)

#
#
#Holt Winters Procedure

HW_Wages <- ts(Project_Data$Median_Wages[80:153], frequency = 4)

HWProject <- HoltWinters(HW_Wages, seasonal = "additive")

HW_Predicted <- HWProject$fitted[,2]

HWProject2 <- HoltWinters(HW_Wages, gamma = FALSE, seasonal = "additive")

HW_Predicted2 <- HWProject2$fitted[,2]

HW_MAPE_Trend <- mean(abs((HW_Wages-HW_Predicted)/HW_Wages))*100
print(HW_MAPE_Trend)

HW_MAPE_NoTrend <- mean(abs((HW_Wages-HW_Predicted2)/HW_Wages))*100
print(HW_MAPE_NoTrend)

HW_Root_Trend <- (mean((HW_Wages - HW_Predicted)^2))^.5
print(HW_Root_Trend)

plot(HW_Wages, main= "Plot of the Observed Observations vs the Holt Winters Filtering", col= "red", sub= "Figure 13")
lines(HW_Predicted, col="green")
legend(1,355, c("Observed values","Holt Winters Filtering"), lty=c(1,1), 
lwd=c(2,2),col=c("red","green")) 

plot(HW_Wages, main= "Plot of the Observations vs the Holt Winters Filtering Without Trend", col= "red", sub= "figure 8")
lines(HW_Predicted2, col="blue")
legend(22,320, c("Observed values","Holt Winters Filtering"), lty=c(1,1), 
lwd=c(2,2),col=c("red","blue")) 

#
#
#Naive Model 

Naive_Wages <- ts(Project_Data$Median_Wages[80:153])

Naive_lag1 <- lag(Naive_Wages, -1)
Naive_lag4 <- lag(Naive_Wages, -4)
Naive_lag5 <- lag(Naive_Wages, -5)

Naive_Predict <- Naive_lag4 + ((Naive_lag1 - Naive_lag5)/4)

Naive_MAPE <- mean(abs((Naive_Wages-Naive_Predict)/Naive_Wages))*100
print(Naive_MAPE)

Naive_Root_Trend <- (mean((Naive_Wages - Naive_Predict)^2))^.5
print(Naive_Root_Trend)

plot(Naive_Wages, main= "Plot of the Observations vs the Naive Seasonality Model", col= "yellow", sub= "figure 9")
lines(Naive_Predict, col="orange")
legend(4,355, c("Observed values","Predicted Values"), lty=c(1,1), 
lwd=c(2,2),col=c("yellow","orange")) 

#
#
#Sarima Procedure

summary(arima(Wages, order = c(2,1,2), include.mean=FALSE, 
      seasonal = list(order = c(0,1,0), period = 4)))
print(BIC(arima(Wages, order = c(2,1,2), include.mean=FALSE, 
                seasonal = list(order = c(0,1,0), period = 4))))

summary(arima(Wages, order = c(2,0,2), include.mean=FALSE, 
              seasonal = list(order = c(0,1,0), period=4)))
print(BIC(arima(Wages, order = c(2,0,2), include.mean=FALSE, 
                seasonal = list(order = c(0,1,0), period=4))))

summary(arima(Wages, order = c(2,0,3), include.mean=FALSE, 
              seasonal = list(order = c(0,1,0), period=4)))
print(BIC(arima(Wages, order = c(2,0,3), include.mean=FALSE, 
                seasonal = list(order = c(0,1,0), period=4))))

summary(auto.arima(Wages))

summary(arima(Wages, order = c(2,0,2), include.mean=FALSE, 
      seasonal = list(order = c(2,1,2), period=4)))
print(BIC(arima(Wages, order = c(2,0,2), include.mean=FALSE, 
          seasonal = list(order = c(2,1,2), period=4))))

summary(arima(Wages, order = c(2,0,2), include.mean=FALSE, 
              seasonal = list(order = c(2,1,10), period=4)))
print(BIC((arima(Wages, order = c(2,0,2), include.mean=FALSE, 
           seasonal = list(order = c(2,1,10), period=4)))))

Optimal_Sarima <- arima(Wages, order = c(2,0,2), include.mean=FALSE, 
                        seasonal = list(order = c(2,1,2), period=4))

Predicted_Sarima <- Wages - Optimal_Sarima$residuals

plot(Wages, main= "Plot of the Observed Observations vs the Optimal Sarima Model", col= "blue", sub= "figure 10")
lines(Predicted_Sarima, col="brown")
legend(24,320, c("Observed values","Sarima Filtering"), lty=c(1,1), 
lwd=c(2,2),col=c("blue","brown"))

#
#
#Testing the full model vs pre and post structural break

Pre_Wages <- ts(Project_Data$Median_Wages[1:79], frequency= 4)

Pre_Optimal_Arima <- arima(Pre_Wages, order = c(2,0,2), include.mean=FALSE, 
                          seasonal = list(order = c(2,1,2), period=4))

summary(Pre_Optimal_Arima)
print(BIC(Pre_Optimal_Arima))

Post_Wages <- ts(Project_Data$Median_Wages[80:153])

Post_Optimal_Arima <- arima(Post_Wages, order = c(2,0,2), include.mean=FALSE, 
                           seasonal = list(order = c(2,1,2), period=4))

summary(Post_Optimal_Arima)
print(BIC(Post_Optimal_Arima))

#
#
#Sarima Procedure incorporating structural breaks 

Wages_Post_Structural_Break <- ts(Project_Data$Median_Wages[80:153], frequency = 4)

pacf(Wages_Post_Structural_Break, main= "PACF Post Structural Break", sub ="Figure 11")
pacf(diff(Wages_Post_Structural_Break), main= "PACF Post Structural Break Differenced")

acf(Wages_Post_Structural_Break, main= "ACF Post Structural Break", sub= "Figure 12")
acf(diff(Wages_Post_Structural_Break), main= "ACF Post Structural Break Differenced")

summary(auto.arima(Wages_Post_Structural_Break))

#
#
#pre seasonal

summary(arima(Wages_Post_Structural_Break, order = c(2,0,0), include.mean=FALSE, 
              seasonal = list(order = c(0,1,0), period=4)))
print(BIC((arima(Wages_Post_Structural_Break, order = c(2,0,0), include.mean=FALSE, 
           seasonal = list(order = c(0,1,0), period=4)))))

summary(arima(Wages_Post_Structural_Break, order = c(3,0,0), include.mean=FALSE, 
              seasonal = list(order = c(0,1,0), period=4)))
print(BIC((arima(Wages_Post_Structural_Break, order = c(3,0,0), include.mean=FALSE, 
           seasonal = list(order = c(0,1,0), period=4)))))

#
#
#seasonal

summary(arima(Wages_Post_Structural_Break, order = c(2,0,0), include.mean=FALSE, 
              seasonal = list(order = c(0,1,1), period=4)))
print(BIC((arima(Wages_Post_Structural_Break, order = c(2,0,0), include.mean=FALSE, 
           seasonal = list(order = c(0,1,1), period=4)))))

summary(arima(Wages_Post_Structural_Break, order = c(2,0,0), include.mean=FALSE, 
              seasonal = list(order = c(1,1,1), period=4)))
print(BIC((arima(Wages_Post_Structural_Break, order = c(2,0,0), include.mean=FALSE, 
           seasonal = list(order = c(1,1,1), period=4)))))

Optimal_Sarima_Post <- arima(Wages_Post_Structural_Break, order = c(2,0,0), include.mean=FALSE, 
                             seasonal = list(order = c(0,1,1), period=4))

Structural_Break_Predict <- (Wages_Post_Structural_Break - Optimal_Sarima_Post$residuals)

plot(Wages_Post_Structural_Break, main= "Plot of the Observations vs the Predicted Values", ylab= "Real Median Weekly Earnings", col= "black", sub= "Figure 14")
lines(Structural_Break_Predict, col="orange")
legend(2,355, c("Observed values","Predicted Values"), lty=c(1,1), 
lwd=c(2,2),col=c("Black","Orange")) 

plot(density(resid(Optimal_Sarima_Post)), main= "The Density Plot of the Residuals" ,sub= "Figure 16")

qqnorm(resid(Optimal_Sarima_Post), sub= "Figure 15")

qqline(resid(Optimal_Sarima_Post))

predict(Optimal_Sarima_Post, n.ahead = 1)

#
#
#Garch testing and model

acf(Optimal_Sarima_Post$residuals, main = "Autocorrelation Within the Residuals of the Sarima Model", sub= "Figure 17")

Box.test(Optimal_Sarima_Post$residuals, lag = 4, type = "Ljung-Box", fitdf = 3) 

#
#
#Summary Statistics

print(mean(Wages_Post_Structural_Break))
max(Wages_Post_Structural_Break)
min(Wages_Post_Structural_Break)
print(sd(Wages_Post_Structural_Break))


