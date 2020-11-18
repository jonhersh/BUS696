# ----------------------------------------------------------
# Load Libraries
# ----------------------------------------------------------
set.seed(1818)
options(scipen = 9)

library('tidyverse')
library('rsample')
library('glmnet')
library('glmnetUtils')
library('forcats')

# clean data a bit 
data(mpg)
mpg_clean <- mpg %>% mutate(manufacturer = fct_lump(manufacturer,5),
                      trans = fct_lump(trans,3),
                      class = fct_lump(class,5)) %>% 
               select(-model,-cty)


# ----------------------------------------------------------
# Fit Ridge Model
# ----------------------------------------------------------

# estimate a Ridge model using glmnet
# note if you get an error make sure you 
#  have loaded glmnetUtils
ridge_mod <- cv.glmnet(hwy ~ .,
                       data = mpg_clean,
                       # note alpha = 0 sets ridge!  
                       alpha = 0)

# print the two model sugegsted values of lambda:

print(ridge_mod$lambda.min)
#
print(ridge_mod$lambda.1se)

# print coefficient using lambda.min
coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
  round(3)

# print coefficient using lambda.1se
coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
  round(3)

# put into coefficient vector
ridge_coefs <- data.frame(
  `ridge_min` = coef(ridge_mod, s = ridge_mod$lambda.min) %>% 
    round(3) %>% as.matrix() %>% as.data.frame(),
  `ridge_1se` = coef(ridge_mod, s = ridge_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() %>% as.data.frame()
) %>% rename(`ridge_min` = 1, `ridge_1se` = 2)

# use the plot function to see the MSE
# path as we vary lambda (the amount of penalization)
plot(ridge_mod)

### examine coefficient shrinkage path
# note may need to install devtools first
# install.packages('devtools')
devtools::install_github("jaredlander/coefplot")
library('coefplot')
coefpath(ridge_mod)


# ----------------------------------------------------------
# Lasso Model
# ----------------------------------------------------------

# note cv.glmnet automatically performs 
# k-fold cross-validation 
lasso_mod <- cv.glmnet(hwy ~ .,
                       data = mpg_clean,
                       # note alpha = 1 sets Lasso!
                       alpha = 1)


# Note that lasso estimates a series of models, one for 
# every value of lambda -- the amount of shrinkage

# print the two model sugegsted values of lambda:
print(lasso_mod$lambda.min)
# 
print(lasso_mod$lambda.1se)

# plot how the MSE varies as we vary lambda
plot(lasso_mod)

# to examine the coefficients we must say what value of 
# lambda we want to use. 

# coefficients using lambda.1se
coef(lasso_mod, 
     s = lasso_mod$lambda.1se) %>% 
  round(3)

# coefficients using lambda that minimizes cross-validated error
coef(lasso_mod, 
     s = lasso_mod$lambda.min) %>% 
  round(3)

# put into coefficient vector
lasso_coefs <- data.frame(
  `lasso_min` = coef(lasso_mod, s = lasso_mod$lambda.min) %>% 
    round(3) %>% as.matrix() %>% as.data.frame(),
  `lasso_1se` = coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
    round(3) %>% as.matrix() %>% as.data.frame()
) %>% rename(`lasso_min` = 1, `lasso_1se` = 2)
print(lasso_coefs)

# 
coefpath(lasso_mod)


# ----------------------------------------------------------
#  Lab Exercises
# ----------------------------------------------------------
# 1. Load the semiconductor dataset and split into testing and 
#    training sets
semi <- read_csv('https://raw.githubusercontent.com/TaddyLab/MBAcourse/master/examples/semiconductor.csv')
semi_split <- initial_split(semi, 0.9)
semi_train <- training(semi_split)
semi_test <- testing(semi_split)

# 2. Estimate a lasso model using the training data and call this 
#    lasso_mod2

# 3. Call the plot function against the lasso_mod2 and describe the plot

# 4. Print the coefficient vector using lambda.1se.

# 5. Print the coefficient vector using lambda.min

# 6. How many varaibles are non-zero using lambda.min and lambda.1se? 
#     Why are they different

lasso_mod <- cv.glmnet(FAIL ~ .,
                       data = semi_train,
                       # note alpha = 1 sets Lasso!
                       alpha = 1)

coef(lasso_mod, s = lasso_mod$lambda.1se)

plot(lasso_mod)

coef_lasso_min <- coef(lasso_mod, s = lasso_mod$lambda.min)

head(coef_lasso_min)

coef_lasso_min <- coef_lasso_min %>% as.matrix() %>% 
  as.data.frame() %>% 
  round(3)

coef_lasso_min %>% filter(`1` != 0)

coef_lasso_1se <- coef(lasso_mod, s = lasso_mod$lambda.1se) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  round(3) %>% 
  filter(`1` != 0)

print(coef_lasso_1se)

# 7. Estimate a ridge model using the training data and call this ridge_mod2

# 8. Call the plot function against the model and describe the plot

# 9. Print the ridge coefficient vector using lambda.1se


# ----------------------------------------------------------
# ElasticNet Model
# ----------------------------------------------------------
enet_mod <- cva.glmnet(hwy ~ .,
                       data = mpg_clean,
                       alpha = seq(0,1, by = 0.05))

plot(enet_mod)

# now enet_mod holds a list with all of the sub models, 
# each with alpha = whatever sequence the model was estimated with

minlossplot(enet_mod, 
            cv.type = "min")


str(enet_mod$modlist)

# Use this function to find the best alpha
get_alpha <- function(fit) {
  alpha <- fit$alpha
  error <- sapply(fit$modlist, 
                  function(mod) {min(mod$cvm)})
  alpha[which.min(error)]
}

# Get all parameters.
get_model_params <- function(fit) {
  alpha <- fit$alpha
  lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
  error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best <- which.min(error)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], eror = error[best])
}

# extract the best alpha value and model parameters
best_alpha <- get_alpha(enet_mod)
print(best_alpha)
get_model_params(enet_mod)

# extract the best model object
best_mod <- enet_mod$modlist[[which(enet_mod$alpha == best_alpha)]]

