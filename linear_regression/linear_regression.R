#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

par(mar = c(4, 4, 2, 2), mfrow = c(1, 1)) #optional
with(states.data,plot(x = metro, y = energy,pch=16))

# From the plot, it appears that energy consumed increases as the percentage of the
# residents living in metro until it reaches a certain energy consumption and then
# drops. The relationship is parabolic. 

# Let us fit a regression on the data without transforming the data
energy.mod <- lm(energy ~ metro, data = states.data)
summary(energy.mod)

# As expected, the adjusted R square of this model is only 9.7%. We can do better
# with transformations

# for a parabolic relationship, we can do two different transformations.
# 1. sqrt of the response term and the square the predictor variables
# 2. add a square of the predictor variable term in addition to the predictor variable
# itself.

# Here we evaluated both and went with the first one.

energy.mod2 <- lm(sqrt(energy) ~ (metro)^2 , data = states.data)
summary(energy.mod2)

# From the summary above, you can see that we improved the adjusted R^2 from
# 9.7% to 11.9% with transformations. 

# But, clearly, the R^2 is still small. We know from the plot that linear regression
# may not be the best option for expressing the relationship between energy and metro
# A polynomial regression would be the best option.

# Let us plot the model to verify the  deviation from the modeling assumptions

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(energy.mod2, which = c(1, 2)) # "which" argument optional

# As you can see from the residual vs. fitted plot, we see a trend in the residuals.
# This indicates that our model deviated from the linear regression assumptions
# The Q-Q plot also shows that and there are clear deviators; namely observations
# 19, 44 and 51


# Now, we will add a few more terms to see if the resulting model is significantly
# better than the existing model

energy.morevars.mod <-  update(energy.mod2,. ~ . + expense + house + senate)
summary(energy.morevars.mod)                      

# With the addition of new variables, our adusted R^2 improved to 36.92%. 
# A signficant improvement indeed.

# Let us plot the new model
plot(energy.morevars.mod, which = c(1, 2)) # "which" argument optional

# Still the same story, but, a slightly better than the energy.mod model
# Perhaps, we could get a better model plot after eliminating the 3 problematic 
# observations.

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

# Let us add an interaction term to the model created in exercise 1.
# To do so, we need to undestand the correlations between the predictor variables
# from model 1
# cor.plot from corrplot package comes handy
library(corrplot)
cols = c('metro','expense','house','senate')
states.data[,cols] = sapply(states.data[,cols],function(x) as.numeric(x))
m <- cor(states.data[,cols],use="pairwise.complete.obs")
par(mar = c(4, 4, 2, 2), mfrow = c(1, 1)) #optional
corrplot(m,method="number")

# There seems to be a high correlation between senate and house. So, let us add
# the interaction term between those variables

energy.morevars.inter.mod <- update(energy.morevars.mod,.~.+house*senate)
summary(energy.morevars.inter.mod)

# As you can see the interaction term seemed to improve the model.

# Now, let us try to add region. Remember, region is a factor variable, so, the 
# the other regions will have coefficients that are with respect to the first 
# value in region i.e. West

energy.morevars.inter.region.mod <- update(energy.morevars.inter.mod, .~. + region)
summary(energy.morevars.inter.region.mod)

# As you can see from the coefficients of various regions, there are significant
# differences among the 4 regions.  Remember for region = West, the intercept
# is the coefficent
