##############################################
## Week 10: ANOVA with categorical variable ##
##############################################

require(mosaic)

## Import the CHICKEN dataset


## Use a boxplot to look at the weights by feed type 
boxplot(weight ~ feed,
        data = CHICKEN,
        horizontal = TRUE,
        main = "Chicken Weights",
        ylab = "Weight (grams)", 
        xlab = "Feed type") 

## Look at the summary statistics for weight by feed type:
favstats(weight ~ feed, data = CHICKEN)

## Fit the model: weight ~ feed type
my.mod <- lm(weight ~ feed, data = CHICKEN)
summary(my.mod)  

## Note: R makes horsebean the default feed, because 
## "horsebean" is first, alphabetically. The coefficient estimates are 
## essentially testing whether the mean weight for that type of feed is 
## different from the mean weight of chickens that ate horsebean feed


## Get an ANOVA table-- Is at least one of the mean weights different
## by feed type? (yes)
anova(my.mod)

## With an F-statistic of 15.365 on 5 and 65 degrees of freedom and a
## p-value < 0.001, there is strong evidence that the mean weight of chickens
## is different for at least one type of feed

## ... but which mean weights differ from one another by feed?

###########################
## Pairwise comparisons  ##
###########################

## Format: 
## pairwise.t.test(DATA$response_var, DATA$explan_var, p.adjust.method = c("bonf","none"))
## Selecting "bonf" will use Bonferroni adjustment to calculate new alpha level
## Selecting "none" will not use any adjustment to your alpha (significance) level
pairwise.t.test(CHICKEN$weight, CHICKEN$feed, 
                p.adjust.method = "bonf")
