#Last Assignment Code

#Answers to hw questions, copy to word!!!
#
#1a. We need to check for a high correlation to determine
#    whether the explanatory variable is a predictor of the response
#
#b. Out of the output I obtained, I should
#   use the p-value retrieved from the summary
#   statistics to determine correlation between
#   explanatory variables.
#
#c. 
#
#
#
#
#
#
#

## Scatterplot matrix and correlation matrix
pairs(AIR, pch=16, main="Scatterplot Matrix for AIR data set")
cor(AIR)

my.mod <- lm(temp ~ ozone + solar + wind, data = AIR)
summary(my.mod)

plot(my.mod$fitted, my.mod$residuals, 
     pch=16, 
     main="Residuals vs Fitted Values", 
     ylab="Residual", 
     xlab="Fitted Value")
abline(0,0)  ## Add a horizontal line at 0 

## Normal Probability Plot of the Residuals
qqnorm(my.mod$residuals)
qqline(my.mod$residuals)


