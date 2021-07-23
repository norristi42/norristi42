## Input EAR dataset
## The EAR data set is based on 202 children with acute otitis media (OME) 
## who participated in a randomized clinical trial.  Each child had OME at 
## the beginning of the study in either one (unilateral cases) or 
## both (bilateral cases) ears.  Each child was randomly assigned to 
## receive a 14-day course of one of two antibiotics, either cefaclor (CEF) 
## or amoxicillin (AMO).  The focus here is on the 202 children whose 
## middle-ear status was determined at a 14-day follow-up visit.  

## Variables: clearance (categorical), antibiotic (categorical), and age group (categorical)

#######################################
######### Test for Association ########
#######################################

## Is there an association between clearnance of an infected ear (after 14 days)
## and the type of antibiotic used?

## Create a table of counts with marginal totals
memory.table <- table(MEMORY$remember, MEMORY$diet)
memory.table  ## Gives the table without the column totals, row totals, and grand total
addmargins(memory.table)  ## Gives the table WITH the column totals, row totals, and grand total


## To show the column percents
prop.table(memory.table, margin=2)
prop.table(ear.table, margin=2)*100

## Check the expected counts
chisq.test(memory.table)$expected

## OR YOU CAN CODE (for the same results as above):
ear.test <- chisq.test(ear.table)
ear.test$expected


## To obtain the output from the chi-squared test
## Useful things in output:
## "X-squared" is out chi-squared statistic
## df = degrees of freedom for our chi-squared statistic
## p-value -- because who doesn't love a p-value?

chisq.test(ear.table)

## Verify similar results with a Fisher's Exact Test
fisher.test(ear.table)  ## Both give small p-values (very strong evidence against the null hypothesis)

fisher.test(memory.table)

#######################################
######### Goodness of Fit test ########
#######################################

## H_0: p_1 = 0.25,  p_2 = 0.5,  p_3 = 0.25  
## p_1 = proportion of children in the < 2 years age group
## p_2 = proportion of children in the 2-5 years age group
## p_3 = proportion of children in the 6+ years age group

## First get a table of counts, sorting by age category (<2 yrs, 2-5 yrs, and 6+ years)
## Recall that 1 = "<2 yrs"       2 = "2-5 yrs"  and    3 = "6+ yrs"
earnew.table <- table(EAR$age)
earnew.table  ## Now everyone in the dataset is sorted by age group
prop.table(earnew.table)


## Obtain a p-value
chisq.test(earnew.table, p=c(0.25, 0.5, 0.25))  ## Tiny p-value!

#####################################################################

###########################################
######### SLR (regression) example ########
###########################################

## Import the ROSES data set
## Variables: 
## number = number of roses sold around Valentine's Day (quantitative)
## price = the wholesale selling price of a dozen roses (quantitative)

## Obtain a scatterplot
plot(MEMORY$remember, MEMORY$diet, 
     main = "Number of Roses vs. Wholesale Price",
     xlab = "Price of a dozen roses ($)",
     ylab = "Number of roses sold around Valentine's Day",
     pch = 16)

## Obtain a residual plot and normal QQ plot

## We must first create our linear model (necessary to calculate residuals)
## To get the linear model (lm), use the lm() function
## Format: lm(response_var ~ explan_var, data = DATANAME)
## Use "summary(model.name)" to get the estimates for the intercept, slope, and R^2

my.mod <- lm(number ~ price, data = R)
summary(my.mod)

## Now we can create graphs using the residuals saved within the 
## linear model object
## Format: plot(var_x.axis, var_y.axis)

plot(ROSES$price, my.mod$residuals, 
     main = "Residual Plot",
     xlab = "number of roses sold", ylab = "residuals", pch=16)
abline(0,0)  ## Adds a horizontal line at zero

## Create a scatterplot (original dataset) with the regression line added
plot(ROSES$price, ROSES$number, 
     main="Number of Roses vs. Wholesale Price",
     xlab="Price of a dozen roses ($)",
     ylab="Number of roses sold around Valentine's Day",
     pch=16)
abline(my.mod)  ## Adds the SLR line from earlier

## Finding Correlation
cor(ROSES$number, ROSES$price)

## Notice, order of the variables in the cor() function doesn't matter
## Correlation doesn't care about order of variables (everything else usually does)
cor(ROSES$price, ROSES$number)

## Making a prediction using your linear model
predict(my.mod, 
        data.frame(price = 70.00))  ## I want to predict number roses sold when price is $35.50

#Assignment 4 MISC

prop.test(x = c(9,24), n = c(63,138))

prop.test(x = c(9, 24), 
          n = c(63, 138), 
          alternative = "two.sided", 
          conf.level = 0.95)

prop.test(x = c(9, 24), 
          n = c(63, 138), 
          alternative = "two.sided", 
          conf.level = 0.95)
