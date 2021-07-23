######################################################################################
## Two sample t-test
######################################################################################

## One of the first things you should do when you jump on R Studio is to require(mosaic)
require(mosaic)

## Import the SSURVEY data set. You will need to fill in the blanks. 
## Items to replace:
##     YYY with response variable
##     XXX with explanatory variable
##     ZZZ with name of data set


boxplot(Price~Product, 
        data = safeway, 
        horizontal = TRUE,
        main = " Survey Data ",
        xlab = " Employed ")



## Dotplots for small sample sizes
## REPLACE:
##  AAA and BBB with numbers between 0 and 25. Each number represents a 
##     different symbol. The first number is the plotting symbol for the group 
##     that comes first alphabetically.You can explore the different symbols by running ?pch
## CCC and DDD with different names of colors. You a find a list of all color
##     names here:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf


## Option 1
dotPlot(~Price, 
        groups = Product, 
        data = safeway, 
        pch=c(19,2),  ## affects width of dots
        col=c("blue","green"),  ## Colors that you want the dots, first color goes to first group (alphabetically)
        main = "Comparison of Amount of Exercise between Employed and Non-Employed OSU students ", 
        xlab = "Exercise per day (minutes) ")

## Option 2
dotPlot(~exercise | work,  ## This combines the "~YYY" and "groups = XXX" into one line
        data = SSURVEY,  
        main = "Comparison of Amount of Exercise between Employed
        and Non-employed OSU students", 
        xlab = "exercise per day (minutes)")


## Obtain summary statistics for each group
favstats(exercise ~ work, data = SSURVEY)


#######################################################
## Hypothesis Tests
#######################################################

## Two-sample t-methods (un-pooled or not pooled)
## REPLACE:
##     EEE with the null value for the population parameter (shoud be a #)
##     FFF with the one of options: "two.sided"   "less"   or   "greater"
##     remember that R takes the group that is alphabetically first as
##     group 1

t.test(price.sfwy ~ price.walm, 
       data = merged,
       mu = 0, 
       alternative = "two.sided")

## Two-sample t-test (pooled)
t.test(price.sfwy ~ Product,
       data = merged, 
       var.equal = FALSE,  ## By default, R assumes var.equal = FALSE, or unpooled
       mu = 0, 
       alternative = "two.sided")


### If you wanted to change the confidence level in your t-test:
## REPLACE:
##     GGG with your level of confidence as a decimal

t.test(money~meal, 
       data = MONEY, 
       var.equal = TRUE,  ## Only include when you want pooled variance
       mu = 0, 
       alternative = "two.sided", 
       conf.level = 0.95)  ## Confidence level as decimal


#########################################################
## BOOTSTRAP METHOD
########################################################

## 1) Calculate difference in sample means

diff <- diffmean(money~meal, data = MONEY)

## 2) We now need the sample size for each group. One way to do this is 
favstats(money ~ meal, data = MONEY)

## Then set nwork equal to the sample size for the employed and nnowork
## equal to the sample size for the non-employed. Type the sample sizes for each
## group after the <-

size <- length(safeway$Price)  ## Sample size for safeway products
nnowork <- length(SSURVEY$work[SSURVEY$work=="notemployed"]) ## Sample size for employed group
  
  ## A second way to accomplish the same thing is using this code:
  ## This way is nice if you don't want to count/consistently want correct sample size
  
    nwork <- length(SSURVEY$work[SSURVEY$work=="employed"])

    nnowork <- length(SSURVEY$work[SSURVEY$work=="notemployed"])


## 3) Bootstrap distribution of differences in sample means
##    using student survey data comparing exercise and employment status

bootdiff <- do(10000)*(
    mean(sample(MONEY$money[MONEY$meal=="yes"], yplan, replace = TRUE)) - 
      mean(sample(MONEY$money[MONEY$meal =="no"],noplan, replace = TRUE))
)

## 4) We need to center this distribution on the null value for 
##    the true difference in population means:

adj.bootdiff <- bootdiff$result - diff  ## Recall we need distribution centered on null value to get a p-value

## 5) Finding the p-value
sum(abs(adj.bootdiff) >= abs(diff))/ length(adj.bootdiff)



###########################################################################
## CONSTRUCTING A CONFIDENCE INTERVAL
###########################################################################

## A confidence interval was part of the output for the t-tests above
## You can also get the bounds directly this way:
confint(t.test(exercise ~ work, 
               data = SSURVEY, 
               conf.level = 0.95))


## Confidence Interval using percentile method

quantile(bootdiff$result, c(0.025, 0.975))



##########################################################################
## FOR YOUR INFORMATION ONLY!
##########################################################################
## If you wanted to perform this hypothesis on the medians instead of the means, you
## may use the code below:

## Differences in sample medians

## To find the difference in medians:
diffmed <- median(SSURVEY$exercise[SSURVEY$work=="employed"]) - 
            median(SSURVEY$exercise[SSURVEY$work=="notemployed"])

## To create a bootstrap distribuiton of differences in medians
bootdiffmed <- do(10000)*
  (median(sample(SSURVEY$exercise[SSURVEY$work=="employed"], nwork, replace = TRUE)) - 
      median(sample(SSURVEY$exercise[SSURVEY$work=="notemployed"], nnowork, replace = TRUE)))

## Find the two-sided p-value
bootdiffmedadj <- bootdiffmed$result-diffmed

sum(abs(bootdiffmedadj) >= abs(diffmed))/ length(bootdiffmedadj)

quantile(bootdiffmed$result, c(0.025, 0.975))

##################################################################################

## Commands IF a randomization test on the MEANS was to be performed:

randtest <- do(10000)*diffmean(exercise~shuffle(work), data = SSURVEY)

diff <- diffmean(exercise ~ work, data = SSURVEY)

sum(abs(randtest$diffmean) >= diff)/ length(randtest$diffmean)


## Commands IF a randomization test was to be performed on MEDIANS

diffmed <- median(SSURVEY$exercise[SSURVEY$work=="employed"]) - 
             median(SSURVEY$exercise[SSURVEY$work=="notemployed"])

randtestmed <- do(10000)*{
  rand1<-shuffle(SSURVEY$work)
  median(SSURVEY$exercise[rand1=="employed"]) -
    median(SSURVEY$exercise[rand1=="notemployed"])
}

sum(abs(randtestmed$result) >= abs(diffmed)) /  length(randtestmed$result)


#######################################################################
##  EXAMPLE 2: PAIRED METHODS
######################################################################


## Paired t-methods

t.test(safeway$Price,
       walmart$Price,
       paired = TRUE, 
       mu = 0, 
       alternative = "greater")
change <- merged$price.sfwy - merged$price.walm
qqnorm(change)
qqline(change)

## Bootstrap methods were not discussed in lab, but if you wanted to see them here they are!

sampmed <- median(change)
hypvalue <- 0
adjdata <- change - (sampmed - hypvalue)
adj.bootmed <- do(10000)*median(sample(adjdata, length(adjdata), replace=TRUE))

sum(adj.bootmed$median >= sampmed) / length(adj.bootmed$median)


## To find the confidence interval for the median difference (before - after)
## using the percentile method:

## since the bootstrap distribution needs to be centered at the sample median
## of the differences, generate a bootstrap distribution on the variable "change"
## (the observed differences)

bootmed <- do(10000)*median(sample(change, length(change), replace=TRUE))
quantile(adj.bootmed$median, c(0.025, 0.975))


#######################################################################
## HELPFUL CODE FOR THE WORKSHEET! - COMPARISON OF MPG BETWEEN AMERICAN AND FOREIGN CARS
#######################################################################

boxplot(combined ~ American, 
        data = MPG, 
        horizontal = TRUE,
        main = "Comparison of MPG between American and Foreign cars",
        xlab = "miles per gallon",
        ylab = "type of car")

favstats(combined ~ American, data =MPG)

t.test(combined ~ American, 
       data = MPG, 
       var.equal = TRUE , 
       mu = 0)

## Some Assignment 2 Code
hist(safeway$Price, 
     main="The price of each product at Safeway",     
     ylab="Count at each price", 
     xlab="Price of Product")  ## R assum

hist(walmart$Price, 
     main="The price of each product at Walmart",     
     ylab="Count at each price", 
     xlab="Price of Product")  ## R assum

sum(safeway$Price)

sum(walmart$Price)

## 2) We now need the sample size for each group. One way to do this is 
favstats(~Price, data = safeway)

bootdiff <- do(10000)*(
  mean(sample(safeway$Price, replace = TRUE)))


## Paired t-methods

t.test(merged$price.sfwy,
       merged$price.walm,
       paired = TRUE, 
       mu = 0, 
       alternative = "greater")
change <- merged$price.sfwy - merged$price.walm
qqnorm(change)
qqline(change)

## Bootstrap methods were not discussed in lab, but if you wanted to see them here they are!

sampmean <- mean(change)
hypvalue <- 0
adjdata <- change - (sampmean - hypvalue)
adj.bootmean <- do(10000)*mean(sample(adjdata, length(adjdata), replace=TRUE))

sum(adj.bootmean$mean >= sampmean) / length(adj.bootmean$mean)

bootmean <- do(10000)*mean(sample(change, length(change), replace=TRUE))
quantile(adj.bootmean$mean, c(0.025, 0.975))


##### PART 2 OF HW #####


boxplot(money~meal, 
        data = MONEY, 
        horizontal = TRUE,
        main = "Money Spent on Food based on Meal Plan (Y/N)",
        xlab = "Money Spent on Food per week ")
        
favstats(money ~ meal, data = MONEY)

t.test(money~meal, 
       data = MONEY, 
       var.equal = TRUE,  ## Only include when you want pooled variance
       mu = 0, 
       alternative = "two.sided", 
       conf.level = 0.95)  ## Confidence level as decimal

## 1) Calculate difference in sample means

diff <- diffmean(money~meal, data = MONEY)

## 2) We now need the sample size for each group. One way to do this is 
favstats(money ~ meal, data = MONEY)

size <- length(MONEY$meal)

yplan <- length(MONEY$meal[MONEY$meal=="Yes"])

noplan <- length(MONEY$meal[MONEY$meal=="No"])

## 3) Bootstrap distribution of differences in sample means
##    using student survey data comparing exercise and employment status

bootdiff <- do(10000)*(
  median(sample(MONEY$money[MONEY$meal=="Yes"], yplan, replace = TRUE)) - 
    median(sample(MONEY$money[MONEY$meal =="No"],noplan, replace = TRUE))
)


## 4) We need to center this distribution on the null value for 
##    the true difference in population means:

adj.bootdiff <- bootdiff$result - diff  ## Recall we need distribution centered on null value to get a p-value

## 5) Finding the p-value
sum(abs(adj.bootdiff) >= abs(diff))/ length(adj.bootdiff)

# 6) Confidence Interval

confint(t.test(money ~ meal, 
               data = MONEY, 
               conf.level = 0.95))

quantile(bootdiff$result, c(0.025, 0.975))


