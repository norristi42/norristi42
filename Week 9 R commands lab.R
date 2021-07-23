## Import the FISH data set

## Scatterplot matrix and correlation matrix
pairs(FISH, pch=16, main="Scatterplot Matrix for FISH data set")
cor(FISH)

## Residual plot versus fitted values
## First create your model (need in order to get residuals)
my.mod <- lm(catch ~ residences + size + structure, data = FISH)
summary(my.mod)

## Now create residuals plot:
plot(my.mod$fitted, my.mod$residuals, 
     pch=16, 
     main="Residuals vs Fitted Values", 
     ylab="Residual", 
     xlab="Fitted Value")
abline(0,0)  ## Add a horizontal line at 0 

## Normal Probability Plot of the Residuals
qqnorm(my.mod$residuals)
qqline(my.mod$residuals)

## Try removing the residences vairable
## from the model because it is correlated with size 
my.mod2 <- lm(catch ~ size + structure, data = FISH)
summary(my.mod2)


##Let's check for possible transformations!

############################################################
############### MODEL WITH CATCH TRANSFORMED ###############
############################################################

my.modLNCATCH <- lm(log(catch) ~ size + structure, data = FISH)
summary(my.modLNCATCH)

## Plots:
## Show the pairs plot again (all the pairwise scatterplots)
pairs(log(catch) ~ size + structure, data = FISH, 
      pch=16, 
      main="Scatterplot Matrix for FISH data set")

## Residuals vs fitted plot (residuals y-axis, fitted values x-axis)
plot(my.modLNCATCH$residuals ~ my.modLNCATCH$fitted, 
     pch=16, 
     main="Residuals vs Fitted Values", 
     ylab="Residual", xlab="Fitted Value")
abline(0,0)

## Normal Probability Plot of the Residuals
qqnorm(my.modLNCATCH$residuals)
qqline(my.modLNCATCH$residuals)


#############################################################
########### MODEL WITH CATCH AND SIZE TRANSFORMED ###########
#############################################################

my.modLNCATCHSIZE <- lm(log(catch) ~ log(size) + structure, data = FISH)
summary(my.modLNCATCHSIZE)

## Plots:
## Show the pairs plot again (all the pairwise scatterplots)
pairs(log(catch) ~ log(size) + structure, data = FISH, pch=16, main="Scatterplot Matrix for FISH data set")

## Residuals vs fitted plot (residuals y-axis, fitted values x-axis)
plot(my.modLNCATCHSIZE$residuals ~ my.modLNCATCHSIZE$fitted, 
     pch=16, 
     main="Residuals vs Fitted Values", 
     ylab="Residual", 
     xlab="Fitted Value")
abline(0,0)

## Normal Probability Plot of the Residuals
qqnorm(my.modLNCATCHSIZE$residuals)
qqline(my.modLNCATCHSIZE$residuals)


## This model seems to meet the conditions well enough
## Let's use model: log(catch) ~ log(size) + structure


################################################
############# Backwards selection: #############
################################################

## Starting model: log(catch) ~ log(size) + structure
## But can we make it better?
## (Can we get a model where all of the variables are useful at predicting
## the log(catch)? )

## First try removing the log(size) variable
## log(size) removed first because the evidence that it was useful 
## was the weakest evidence in the model output (t-statistic = 1.843, p-value 0.0828)
## Note: we only look at the evidence for the explanatory variables (not the intercept)

my.finalmod <- lm(log(catch) ~ structure, data = FISH)
summary(my.finalmod)
