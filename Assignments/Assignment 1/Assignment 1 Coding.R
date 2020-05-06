rm(list=ls()) # clears workspace
#
#
# Nicholas Golina, Katie Leslein
#
#

#
#
# Assignment 1 & June 12th and 13th 2017
#
#

#
#
#Coding for NBA Data
#
#

#
#
#Coding for Data Basics
#
#

nbasalary <- read.csv("~/Google Drive/UA/Senior Year/Economic Forecasting/Data/nbasalary.csv") 

View(nbasalary)

mean(nbasalary$age, na.rm=TRUE)

sd(nbasalary$age, na.rm=TRUE)

#
#
#Answers for The Data Basics Part of the NBA Data Section
#1. The reason is because RStudio does not account for missing values within the data set that are marked as not applicable. 
#2. The mean is 27.58763 and the standard deviation is 3.549898.

#
#
# Coding for Plotting the Data
#
#

plot(nbasalary$exper, nbasalary$wage, 
     main="Scatter plot of years of experience and wages", xlab="exper", ylab="wage", col="purple")

#
#
#Answers for the Plotting Data Part of the NBA Data Section
#1. There appears to be a moderate correlation between wages and experience based on the somewhat upward sloping direction of the data as experience increases.
#

#
#
#Coding for Statistical Analysis
#
#

cor(nbasalary$exper, nbasalary$wage, use="complete.obs", method="pearson")

reg <- lm(wage ~ exper, data=nbasalary)

summary(reg)

#
#
#Answers for the Statistical Analysis Part of the NBA Data Section
#1. There is a moderately positive relationship between wages and years of experience.
#2. For Beta 0, a basketball player has no years of experience the wage is .79014 according to the model of the linear regression. For Beta 1, we can be 99% confident based on the t values that an additional year of experience is associated with a .12231 dollar increase in wages.
#3. The regression analysis is somewhat in line with the visual analysis because I analyzed that it would be a moderately positive relationship, which is shown by the regression as well with high t values. 

#
#
#Coding for Randomly Generated Data
#
#

#
#
#Coding for Creating the Data
#
#

set.seed(1234)

X <- runif(100, min = -2, max = 2)

U <- rnorm(100, mean = 0, sd=1)

Y <- 2 - (1/2)*X^2 + U

#
#
#Answers for the Creating the Data Part of the Randomly Generated Data Section
#
#

#
#
#Coding for Statistical Analysis
#
#

f <- Y ~ X

cor(X, Y, use="complete.obs", method="pearson")

cor(Y, X, use="complete.obs", method="pearson")

reg2 <- lm(f)

summary(reg2)

#
#
#Answers for The Statistical Analysis Part of the Randomly Generated Data Section
#1. The correlation of just over .1 suggests a weak positive relationship between the X variable and the Y variable. The correlation is not a good measure of the relationship, because Y can have the same relationship with X as X has with Y, which does not apply in all circumstances especially since they have different characteristics and therefore can affect each other differently. Take GMOS and GDP growth. If GDP Growth is directly associated with more GMOS that does not mean that GMOS have the same relationship with GDP growth in terms of causing it. In addition to correlation not showing causation as evidence, the regression will provide further evidence of why the representation of the correlation is not quite accurate in showing the relationship between X and Y.   
#2. The regression analysis is better because it can give you a better sense of whether there is a causal link between X and Y. The T values provide a better statistical technique to look at the strength of the relationship. It actually is very weak since the t value is low and the p value is above .1. 

