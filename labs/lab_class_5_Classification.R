#--------------------------------------------------------
# log transformations 
#--------------------------------------------------------
# install.packages('')

library('tidyverse')
data(mpg)


mod1 <- lm(log(hwy) ~ log(displ) + year,
           data = mpg)

summary(mod1)

#--------------------------------------------------------
# Testing and Training Sets
#--------------------------------------------------------

# install rsample if necessary
# install.packageS('rsample')
library('rsample')
library('tidyverse')
data(mpg)

# always want to set a seed before doing any randomization 
# procedure to make our code reproducible
set.seed(1818)

# set train proportion = % of total data in training set
# common values are 0.9, 0.75, 0.8, 0.5
train_prop <- 0.8
mpg_split <- initial_split(mpg, prop = train_prop)

print(mpg_split)

# create testing and training sets using the 
# training and testing functions
mpg_train <- training(mpg_split)
mpg_test <- testing(mpg_split)


# check the number of rows to ensure training 
# and testing split is correct
nrow(mpg_train)
nrow(mpg_test)

#--------------------------------------------------------
# Estimating Models on Training Sets
#--------------------------------------------------------
# estimate a model using the training set
mod <- lm(hwy ~ year + displ, 
          data = mpg_train)

# generate in-sample (training) predictions
preds_train <- predict(mod)
# either method works
preds_train <- predict(mod, newdata = mpg_train)


# generate out-of-sample (test set) predictions
preds_test <- predict(mod, newdata = mpg_test)

#--------------------------------------------------------
# Evaluating In-Sample and Outof Sample
#--------------------------------------------------------
# yardstick is a helpful function 
# that contains metrics of evaluation

library('yardstick')

# create a
results_train <- data.frame(
  `predicted` = preds_train,
  `actual` = mpg_train %>% 
    filter(complete.cases(hwy, year, displ)) %>% 
    select(hwy),
  `type` = rep("train", length(preds_train))
)

results_test <- data.frame(
  predicted = preds_test,
  actual = mpg_test %>% 
    filter(complete.cases(hwy, year, displ)) %>% 
    select(hwy),
  type = rep("test", length(preds_test))
) %>% 
  rename(`predicted` = 1, `actual` = 2, `type` = 3)

rmse(results_train, predicted, actual) 
rmse(results_test, predicted, actual)
mae(results_train, predicted, actual)
mae(results_test, predicted, actual)

metrics(results_train, predicted, actual)
metrics(results_test, predicted, actual)

#--------------------------------------------------------
# Estimating Logistic Regression in R
#--------------------------------------------------------
library('ISLR')
# load data which has credit card default behavior
data(Default)
head(Default)

# make sure to use glm() function! 
# set family = binomial to set logistic function
logit_fit1 <- glm(default ~ student,
                  family = binomial,
                  data = Default)

#--------------------------------------------------------
# Converting logit coefficients to odds ratio
#--------------------------------------------------------

options(scipen = 9)
summary(logit_fit1)
exp(logit_fit1$coefficients)


#--------------------------------------------------------
# Lab 1
#--------------------------------------------------------
# 1. Estimate a logistic regression model predicting 
#    default as a function of student, balance, and income
#    and store this as 'logit_mod2'


# 2. Exponentiate the coefficient vector of logit_mod2
# 3. Interpret the impact of being a student on the probabiliy of default
# 4. Do students face a higher or lower risk of credit card default? 


#--------------------------------------------------------
# Generating Logistic Predictions
#--------------------------------------------------------

logit_fit3 <-  glm(default ~ balance,
                   family = binomial,
                   data = Default)

summary(logit_fit3)

# probability of default with balance of $1000
top <- exp(logit_fit3$coefficients[1] + 
             logit_fit3$coefficients[2] * 1000)
p_hat_1000 <- top / (1 + top)
p_hat_1000


# probability of default with balance of $1000
top <- exp(logit_fit3$coefficients[1] + 
             logit_fit3$coefficients[2] * 2000)
p_hat_2000 <- top / (1 + top)
p_hat_2000

scores <- predict(logit_fit3,
                  type = "response")

#--------------------------------------------------------
# Generating Logistic Predictions
#--------------------------------------------------------
library('yardstick')
?conf_mat
conf_mat(two_class_example, 
         truth = truth, 
         estimate = predicted)


results_logit <- data.frame(
  `truth` = as.factor(Default$default),
  `Class1` =  scores,
  `Class2` = 1 - scores,
  `predicted` = as.factor(ifelse(scores > 0.4,
                       "Yes","No"))
)

cm <- conf_mat(results_logit, 
               truth = truth,
               estimate = predicted)

print(cm)
library(ggplot2)
autoplot(cm, "heatmap")

#--------------------------------------------------------
# ROC plots
#--------------------------------------------------------
library('ggplot2')
library('plotROC')

p <- ggplot(results_logit, 
       aes(m = Class1, d = truth)) + 
  geom_roc(labelsize = 3.5, 
           cutoffs.at = 
             c(0.99,0.9,0.7,0.5,0.3,0.1,0)) +
  theme_minimal(base_size = 16)
print(p)
calc_auc(p)

roc_auc(results_logit, 
        truth = truth, 
        estimate = predicted)

#--------------------------------------------------------
# Exercises
#--------------------------------------------------------
# 1. Generate predictions using your logit_mod2 model
#    that predicts default as a function of 
#    student, balance, and income
# 2. Generate predicted probabilities (score the model)
# 3. Create a results data frame and print a confusion 
#    matrix using the results data
# 4. Plot a ROC curve using the results data
# 5. How well does the model perform?

