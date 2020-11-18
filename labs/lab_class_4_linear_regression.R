
#--------------------------------------------------------
# Basic Linear Regression
#--------------------------------------------------------
# remove all existing objects in memory
rm(list = ls())

# load these libraries
library('tidyverse')


#--------------------------------------------------------
# Formulas in R 
#--------------------------------------------------------
# formulas in R start with the dependent variable on the 
# left hand side (LHS), then by a "~" tilde, following by 
# all the dependent variables you wish to estimate on the
# right hand side (RHS)

# e.g. y ~ x1 + x2

data(mpg)
hwy ~ year + displ + cyl  

#--------------------------------------------------------
# Linear Models using lm()
#--------------------------------------------------------

# estimate a linear model with displacement, and 
# cycl on the RHS, and hwy as the 
# development variable (LHS)
# Use the 'mpg' dataframe to estimate the model
# and store the regression equation as 'mod1'
mod1 <- lm(hwy ~ displ + cyl, 
           data = mpg)

# print out a summary of the linear model
summary(mod1)

# or just view the whole "list" object of 
# the model results
str(mod1)

#--------------------------------------------------------
# estimating "prettier" regression output
#--------------------------------------------------------
# install.packages('sjPlot')
library('sjPlot')
# install.packages('sjPlot')
library('tidymodels')
# output a prettier table of results 
# looks very nice in RMarkdown! 
tab_model(mod1)

# output a plot of regression coefficients
plot_model(mod1)

# output a table of coefficients and their p-values, t-stats
tidy(mod1)



#--------------------------------------------------------
# Exercises - Part 1
#--------------------------------------------------------
# 1. Estimate a regression model of city mpg on year, 
# displacement, and engine cylinders and store this as 'mod3'
# 2. Interpret in words the coefficient for year
# 3. Interpret in words the coefficient for engine cylinders
# 4. If you finish and still have time, try using 'plot_model()'
#    'tab_model' and 'tidy' on 'mod3' (may need to load/install 
#     the packages tidymodels and sjPlot)


#--------------------------------------------------------
# P-values and T-stats
#--------------------------------------------------------
# install package
# install.packages('moderndive')
library('moderndive')
get_regression_table(mod1)

#--------------------------------------------------------
# Factors!
#--------------------------------------------------------
DF <- data.frame(y = rnorm(5),
                 x1 = 1:5,
                 disease = c("has diptheria","typhoid","typhoid","has diptheria","C"))
head(as_tibble(DF))

DF<- DF %>% mutate(x2 = as.factor(x2))

levels(DF$x2)
fct_count(DF$x2)
fct_anon(DF$x2)
fct_unique(DF$x2)

head(DF)
DF <- DF %>% 
  mutate(x2 = as.factor(x2))
glimpse(DF)

library('forcats')
fct_unique(DF$x2)
fct_count(DF$x2)
fct_rev(DF$x2)
DF$x2 <- fct_shift(DF$x2)

model.matrix(~ x1 + x2,
             data = DF)

#--------------------------------------------------------
# Estimating Regression Model with Factors
#--------------------------------------------------------
data(mpg)
mpg <- mpg %>% 
  mutate(class = factor(class))
mod2 <- lm(hwy ~ displ + class,
           data = mpg)
summary(mod2)


levels(mpg$class)

#--------------------------------------------------------
# Change factors with fct_lump
#--------------------------------------------------------
plot(mpg$class)       

mpg <- mpg %>% 
  mutate(class_lump = 
           fct_lump(class, n = 2))
levels(mpg$class_lump)
plot(mpg$class_lump)

#--------------------------------------------------------
# Changing factor levels using relevel()
#--------------------------------------------------------
mod4 <- lm(hwy ~ displ + 
             relevel(class_lump, ref = "Other"),
           data = mpg)
summary(mod4)


#--------------------------------------------------------
# Generate model predictions using "predict" function
#--------------------------------------------------------
# predict on the same data
preds <- predict(mod4)

# can also predict on a new dataset!
preds_new <- predict(mod,
                     newdata = newX)


resids <- mod4$residuals
resids <- mpg$hwy - preds
mean(resids)



#--------------------------------------------------------
# Generate predicted/true plot
#--------------------------------------------------------

# combine preds and resids into a data frame
results <- data.frame(
  preds = preds,
  true = mpg$hwy,
  resids = resids
)
ggplot(results, 
       aes(x = true, y = preds)) +
  geom_point(alpha = 1/2, size = 4) +
  geom_abline(color = "red") +
  xlim(10,40) + ylim(10,40)


#--------------------------------------------------------
# Exercises - Lab 2
#--------------------------------------------------------
# 1. Use the mutate and the as.factor() functions
#    to create a factor variable from the drv variable
# 2. Estimate a regression model predicting highway mpg as a function 
#    of displacement, year, and factor drive style (drv)
# 3. Interpret the coefficient on 'drvf'
# 4. Generate predictions and residuals for this model
# 5. Plot the model predictions against the true values 
