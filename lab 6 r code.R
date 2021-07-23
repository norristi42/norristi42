## ST352 Week 8 Lab Code

#####################################################################################
############### Simple Linear Regression (Two Quantitative Variables) ###############
###############       Creating Graphs to Check Assumptions            ###############
#####################################################################################

## Replace ZZZ with the name of the dataset
## Replace YYY with the name of the response variable
## Replace XXX with the name of the explanatory variable

## Obtain a scatterplot (check LINEARITY assumption)
plot(PLANET$dist_from_sun, PLANET$year_length, 
     main="Distance from Sun vs. Year Length", 
     xlab="Distance (millions of miles)", 
     ylab="Year Length (earth years)", 
     pch=16)

## Other plots (the normal QQ plot and the residuals plot) require first fitting a linear model
## Choose an appropriate name for model and use to replace MMM (example: my.mod or mod1, etc)
MMM <- lm(dist_from_sun~year_length, data = PLANET)

## Obtain a residual plot (check LINEARITY / CONSTANT VARIANCE assumptions)
plot(PLANET$year_length, MMM$residuals, pch=16)
abline(0,0)  ## Adds a horizontal line at 0

## Obtain a normal probability plot (NORMALITY)
qqnorm(MMM$residuals)
qqline(MMM$residuals)


####################################################################################
##############      Transforming Variables in Linear Regression       ##############
############## Fitting Model and Creating Graphs to Check Assumptions ##############
####################################################################################

## Apply a natural log transformation (base e) to the Response variable

## Look at scatterplot-- improvement?
plot(PLANET$dist_from_sun, log(PLANET$year_length), 
     main= "Distance from Sun vs. Year Length", 
     xlab= "Distance from Sun (Millions of Miles)", 
     ylab= "LOG(year length (earth years))", 
     pch=16)

## Other plots require first fitting a linear model
## Choose an appropriate name for the transformed response model and use to replace TTT

TTT <- lm(log(year_length) ~ dist_from_sun, data = PLANET)

## Residual plot
plot(PLANET$year_length, TTT$residuals, pch=16)
abline(0,0)

## Normal probability plot of the residuals
qqnorm(TTT$residuals)
qqline(TTT$residuals)


####################################################################################
#############       Transforming Variables in Linear Regression       ##############
############# Getting Model Parameter Estimates and Making Predictions #############
####################################################################################

## Get a summary of the linear model with log transformed data
summary(TTT)

## Prediction interval for value of the response (given specific value of explanatory variable)
## Replace VVV with the specific value of the explanatory variable for which you want a prediction
## Replace CCC with the level of confidence for your interval

my.PI <- predict.lm(TTT, newdata = data.frame(dist_from_sun = 200), interval="prediction",  level = 0.95)
my.PI

## Don't forget to backtransform the bounds:
exp(0.6283127)
exp(-2.56619)
exp(3.822815)
## OR backtransform all three at once
exp(my.PI)

## Confidence Interval for the mean value of the response (given specific value of explanatory variable)
my.CI <- predict.lm(TTT, newdata = data.frame(XXX = VVV), interval="confidence",  level = CCC)
my.CI

## Don't forget to backtransform the bounds:
exp(0.59523)
exp(0.5782155)
exp(0.6122629)
## OR backtransform all three at once
exp(my.CI)


##############################################################################
############## Analyzing Groups Separately in Linear Regression ##############
##############################################################################

## Splitting the data set
## Replace GGG with the name of the grouping variable being subset by (what you are splitting dataset up by)
## Choose an appropriate name for the dataset of the first group and use to replace Z1Z
## Choose an appropriate name for the transformed response model and use to replace Z2Z

MALES <- subset(FEV, sex == 1) ## Note for FEV dataset 1 corresponds with males
FEMALES <- subset(FEV, sex == 0) ## and 0 corresponds with females
## Now we have a separate dataset each for the female group and the male group

## Create a scatterplot with different symbols for males and females
plot(MALES$height, MALES$volume, 
                 main="Height vs FEV in Children",
                 xlab="Height (in)",
                 ylab="FEV (L)",
                 pch=20)

## Note that in the above code, we have only plotted the male values. Now we
## must add the females values to the plot:
points(FEMALES$height, FEMALES$volume, col="red", 
       pch=18)  ## pch being different for males and females implies different types of points on plot

## Add a legend to our graph
legend("topleft", legend=c("Males", "Females"), pch=c(20, 18), col=c("black", "red"))


## Run seperate regression analysis on both groups
## Choose an appropriate name for the first group model and use to replace M1M
## Choose an appropriate name for the second group model and use to replace M2M

mod.males <- lm(log(volume) ~ height, data = MALES)  ## Linear model for male group
summary(mod.males)

mod.females <- lm(log(volume) ~ height, data = FEMALES)  ## Linear model for female group
summary(mod.females)


## Prediction interval for the first group (for specific value of explanatory variable)
## Replace VVV with the value of the explanatory variable for which you want a prediction
## Replace CCC with the level of confidence for your interval

my.PI.1 <- predict.lm(mod.males, newdata = data.frame(height = 60), interval="prediction",  level = 0.95)
my.PI.1

exp(0.571438)
exp(1.159397)
exp(my.PI.1)  ## Exponentiate the endpoints of prediction interval at same time

## Prediction interval for the second group (for specific value of explanatory variable)
my.PI.2 <- predict.lm(mod.females, newdata = data.frame(height = 60), interval="prediction",  level = 0.95)
my.PI.2

exp(0.546223)
exp(1.146511)
exp(my.PI.2)  ## Exponentiate the endpoints of confidence interval at same time
