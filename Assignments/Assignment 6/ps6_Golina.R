rm(list=ls()) # clears workspace
#
#
# Nicholas Golina worked with Katie Leslein, Jephtha Oppong-atta and Kevin Coyne
#
#

#
#
# Assignment 6 July 7th and 8th 2017
#
#

#
#
#Some Math First
#Chapter 7 Problem 1
#1. xt = xt−1 − 0.25xt−2 + wt + 0.5wt−1
#xt + 0.25xt−2 + xt−1 = wt + 0.5wt−1
#xt(.25*B^2 + B + 1) = wt(1 + .5B)
#.25*xt(B^2 + 4B + 4) = wt*.5(2 + B) 
#.25*xt(B + 2) = wt*.5
#This is an ARMA(2,1) and it is stationary 
#2. xt = 2xt−1 −xt−2 +wt
#xt - 2xt−1 + xt−2 = wt
#xt(B^2 - 2B + 1) = wt
#xt(B - 1)(B - 1) = wt
#This is an ARMA(2,0) model and it is nonstationary
#3. xt = 0.5xt−1 + 0.5xt−2 + wt − 0.5wt−1 + 0.25wt−2
#xt - 0.5xt−1 - 0.5xt−2 = wt − 0.5wt−1 + 0.25wt−2
#-.5*xt(B^2 + B - 2) = .25*wt(B^2 - 2B + 4)
#-.5*xt(B + 2)(B - 1) = .25*wt(B - 2)(B - 2)
#This is an ARMA(2,2) model and it is nonstationary. 
#Chapter 7 Problem 2
#1. xt = 0.5xt−1 + xt−4 − 0.5xt−5 + wt − 0.3wt−1
#xt - 0.5xt−1 - xt−4 + 0.5xt−5 = wt − 0.3wt−1
#xt(1 - .5B)(1 - B^4) = -.3wt(B - 1)
#This is a SARIMA(,,,)(,,,)4
#2. xt = xt−1 + xt−12 − xt−13 + wt − 0.5wt−1 − 0.5wt−12 + 0.25wt−13
#xt - xt−1 - xt−12 + xt−13 = wt − 0.5wt−1 − 0.5wt−12 + 0.25wt−13
#xt(B^13 - B^12 - B + 1) = wt − 0.5wt−1 − 0.5wt−12 + 0.25wt−13
#xt(1 - B)(1 - B^12) = wt(1-.5B)(1-.5B^12)
#This is a SARIMA(,,,)(,,,)12

#
#
#

ps6 <- read.csv("~/Google Drive/UA/Senior Year/Economic Forecasting/Data/ps6.csv")
View(ps6)

z1 <- ts(ps6$z1)
z2 <- ts(ps6$z2)
z3 <- ts(ps6$z3[1:228])

#
#
#

pacf(z1)
acf(z1)
acf(diff(z1))
pacf(diff(z1))
plot(z1)
plot(diff(z1))

print(AIC(arima(z1, order = c(1, 1, 0), include.mean=FALSE)))

print(AIC(arima(z1, order = c(2, 1, 0), include.mean=FALSE)))

print(AIC(arima(z1, order = c(3, 1, 0), include.mean=FALSE)))

print(AIC(arima(z1, order = c(0, 1, 1), include.mean=FALSE)))

print(AIC(arima(z1, order = c(0, 1, 2), include.mean=FALSE)))

print(AIC(arima(z1, order = c(0, 1, 3), include.mean=FALSE)))

print(AIC(arima(z1, order = c(1, 1, 1), include.mean=FALSE)))

print(AIC(arima(z1, order = c(2, 1, 2), include.mean=FALSE)))

print(AIC(arima(z1, order = c(3, 1, 3), include.mean=FALSE)))

print(AIC(arima(z1, order = c(1, 1, 2), include.mean=FALSE)))

print(AIC(arima(z1, order = c(2, 1, 1), include.mean=FALSE)))

print(AIC(arima(z1, order = c(1, 1, 3), include.mean=FALSE)))

print(AIC(arima(z1, order = c(3, 1, 1), include.mean=FALSE)))

print(AIC(arima(z1, order = c(2, 1, 3), include.mean=FALSE)))

print(AIC(arima(z1, order = c(3, 1, 2), include.mean=FALSE)))

modelz1 <- arima(z1, order = c(3, 1, 3), include.mean=FALSE)

plot(density(resid(modelz1)))

qqnorm(resid(modelz1))

qqline(resid(modelz1))

z1ARMA <-

yhatz1 <- 

plot(z1, main="Comparing the Observed and Predicted values for z1", xlab="Time", ylab="Observed and Predicted z1", sub="Red = Observed Green = Predicted", col="red")
lines(y1hatz1, col= "green")

#
#
#

pacf(z2)
pacf(diff(z2))
acf(z2)
acf(diff(z2))
plot(z2)
plot(diff(z2))

print(AIC(arima(z2, order = c(1, 1, 1), include.mean=FALSE)))

print(AIC(arima(z2, order = c(2, 1, 2), include.mean=FALSE)))

print(AIC(arima(z2, order = c(3, 1, 3), include.mean=FALSE)))

print(AIC(arima(z2, order = c(4, 1, 4), include.mean=FALSE)))

print(AIC(arima(z2, order = c(1, 1, 2), include.mean=FALSE)))

print(AIC(arima(z2, order = c(2, 1, 1), include.mean=FALSE)))

print(AIC(arima(z2, order = c(1, 1, 3), include.mean=FALSE)))

print(AIC(arima(z2, order = c(3, 1, 1), include.mean=FALSE)))

print(AIC(arima(z2, order = c(1, 1, 4), include.mean=FALSE)))

print(AIC(arima(z2, order = c(4, 1, 1), include.mean=FALSE)))

print(AIC(arima(z2, order = c(4, 1, 1), include.mean=FALSE)))

print(AIC(arima(z2, order = c(2, 1, 3), include.mean=FALSE)))

print(AIC(arima(z2, order = c(3, 1, 2), include.mean=FALSE)))

print(AIC(arima(z2, order = c(2, 1, 4), include.mean=FALSE)))

print(AIC(arima(z2, order = c(4, 1, 2), include.mean=FALSE)))

print(AIC(arima(z2, order = c(3, 1, 4), include.mean=FALSE)))

print(AIC(arima(z2, order = c(4, 1, 3), include.mean=FALSE)))

modelz2 <- arima(z2, order = c(1, 1, 2), include.mean=FALSE)

plot(density(resid(modelz2)))

qqnorm(resid(modelz2))

qqline(resid(modelz2))

z2ARMA <-
  
yhatz2 <- 
  
plot(z2, main="Comparing the Observed and Predicted values for z2", xlab="Time", ylab="Observed and Predicted z2", sub="Red = Observed Green = Predicted", col="red")
lines(yhatz2, col= "green")

#
#
#

pacf(z3)
pacf(diff(z3))
acf(z3)
acf(diff(z3))
plot(z3)
plot(diff(z3))



print(AIC(arima(z3, order=c(0,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(2,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(3,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))



print(AIC(arima(z3, order=c(3,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(4,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(5,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(6,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))
#
#
#

print(AIC(arima(z3, order=c(7,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))




#
#
#
print(AIC(arima(z3, order=c(8,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(9,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(10,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(11,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(12,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(13,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(14,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,14), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(15,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,15), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,13), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(16,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,16), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(17,0,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,0), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,1), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,2), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,3), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,4), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,5), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,6), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,7), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,8), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,9), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,10), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,11), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,17), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,12), seasonal=list(order=c(0,0,0), frequency=12, include.mean=FALSE))))

#
#
#


#
#
#

print(AIC(arima(z3, order=c(0,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(2,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(3,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))



print(AIC(arima(z3, order=c(3,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(4,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(5,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(6,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))
#
#
#

print(AIC(arima(z3, order=c(7,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))




#
#
#
print(AIC(arima(z3, order=c(8,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(9,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(10,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(11,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(12,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(13,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,13), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(14,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,14), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(15,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,15), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(16,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,16), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(17,0,0), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,1), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,2), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,3), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,4), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,5), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,6), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,7), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,8), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,9), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,10), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,11), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,17), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,12), seasonal=list(order=c(0,0,1), frequency=12, include.mean=FALSE))))




print(AIC(arima(z3, order=c(0,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(2,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(3,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))



print(AIC(arima(z3, order=c(3,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(4,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#

print(AIC(arima(z3, order=c(5,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(6,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))
#
#
#

print(AIC(arima(z3, order=c(7,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))




#
#
#
print(AIC(arima(z3, order=c(8,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(9,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(10,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(11,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(12,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(13,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,13), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(13,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(14,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,14), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(14,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(15,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,15), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(15,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(16,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,16), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(16,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

#
#
#
print(AIC(arima(z3, order=c(17,0,0), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,0,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,0,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(0,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(1,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,1), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(2,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,2), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(3,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,3), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(4,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,4), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(5,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,5), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(6,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,6), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(7,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,7), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(8,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,8), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(9,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,9), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(10,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,10), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(11,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,11), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,17), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(17,1,12), seasonal=list(order=c(0,1,1), frequency=12, include.mean=FALSE))))

print(AIC(arima(z3, order=c(12,1,12), seasonal=list(order=c(9,1,9), frequency=12, include.mean=FALSE))))

modelz3 <- 

plot(density(resid(modelz3)))

qqnorm(resid(modelz3))

qqline(resid(modelz3))

z3ARMA <-
  
yhatz3 <- 
  
plot(z3, main="Comparing the Observed and Predicted values for z3", xlab="Time", ylab="Observed and Predicted z3", sub="Red = Observed Green = Predicted", col="red")
lines(yhatz3, col= "green")

