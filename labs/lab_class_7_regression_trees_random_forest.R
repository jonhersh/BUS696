#------------------------------------------------------------
# Load data and packages
#------------------------------------------------------------
options(scipen = 10)
set.seed(1818)

# please install these if running the first time 
# using install.packages()
library(partykit)
library(tidyverse)
library(titanic)
library(PerformanceAnalytics)
library(rpart)       
library(rpart.plot)  
library('randomForest')



# we will also use these packages later in the lab
# install.packages('visNetwork')
# install.packages('sparkline')



#------------------------------------------------------------
# Regression Trees
#------------------------------------------------------------
help(titanic)
data(titanic_train)

# see fancy correlation chart of titanic dataset
chart.Correlation(titanic_train %>% select(-Name) %>% 
                    select_if(is.numeric), 
                  pch = 20,
                  histogram = TRUE)


# view the top of the titanic data frame
head(titanic_train)

# clean data by creating a binary variable of "survived" vs "did not survive"
# create factors for sex and class of cabin
titanic_df <- titanic_train %>% as_tibble() %>% 
  mutate(Survived = as.factor(Survived),
         Sex = as.factor(Sex),
         Pclass = as.factor(Pclass)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-Name, -Ticket,-Cabin,-Embarked, -PassengerId)



# Use the function ctree in rparty to estimate a 
# single regression tree classification model 
titanic_tree <- ctree(Survived ~ Sex + Pclass, 
                      data = titanic_df)

# print the fitted model object 
print(titanic_tree)

# Viewing the fitted model is easier 
plot(titanic_tree)


#------------------------------------------------------------
# Cross-Validating to Select Optimal Tree Depth
#------------------------------------------------------------
# cross validate to get optimal tree depth
# must use rpart package here

# rpart function to select optimal depth of tree
# read the help() file for rpart.control to learn about 
#  the different function options
# max depth = 6 ensures the final tree only has this 
#  many splits
# min split means minimum observations in a node before 
#  a split can be attempted
# cp is the complexity parameter, overall Rsq must 
#  increase by cp at each step
titanic_mod_rpart <- rpart(Survived ~ Sex + Pclass + Age,
                           data = titanic_df,
                           method = "class",
                           control = list(cp = 0, 
                                          minsplit = 10,
                                          maxdepth = 15))
titanic_mod_rpart$cptable

# plot the relationship between tree complexity (depth and cp)
# and CV error
plotcp(titanic_mod_rpart)



# fancy, interactive tree visual
visNetwork::visTree(titanic_mod_rpart,
                    nodesPopSize = TRUE,
                    edgesFontSize = 18, 
                    nodesFontSize = 20, 
                    width = "100%",
                    height = "1200px")


#------------------------------------------------------------
# Regression Tree Exercises
#------------------------------------------------------------
# 1. Estimate a regression tree model predicting survival as a function 
#    of Sex, Pclass, Age, SibSp and Fare paid using the ctree package. 
#    Store this model as titanic_tree_mod2
# 2. Use the print function against the fitted model to view the text 
#    Descriptions of the model fit
# 3. Use the plot function on the fitted object to produce the tree plot
#    (you can use the option "gp = gpar(fontsize = 6)") 
#    to change the text font size. 
# 4. Who has the best chance of survival? Who has the worst? 
# 5. If you have time, use the rpart package to cross-validate 
#    and select the optimal depth of the model with more predictor variables 
#    (Sex, Pclass, Age, SibSp, and Fare). Call this model titanic_mod_rpart_mod2
# 6. Call the plotcp function against Wthe titanic_mod_rpart. 
#    What is the optimal depth according to the plotcp. 



#------------------------------------------------------------
# bagging - bootstrapp aggregation
#------------------------------------------------------------
# store rownames as columns
titanic_boot_preds <- titanic_df %>% rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))

B <- 100      # number of bootstrap samples
num_b <- 500  # sample size of each bootstrap
boot_mods <- list() # store our bagging models
for(i in 1:B){
  boot_idx <- sample(1:nrow(titanic_df), 
                     size = num_b,
                     replace = FALSE)
  # fit a tree on each bootstrap sample
  boot_tree <- ctree(Survived ~ Pclass + Sex + Age + SibSp + Fare, 
                     data = titanic_df %>% 
                       slice(boot_idx)) 
  # store bootstraped model
  boot_mods[[i]] <- boot_tree
  # generate predictions for that bootstrap model
  preds_boot <- data.frame(
    preds_boot = predict(boot_tree),
    rowname = boot_idx 
  )  
  # rename prediction to indicate which boot iteration it came from
  names(preds_boot)[1] <- paste("preds_boot",i,sep = "")
  # merge predictions to dataset
  titanic_boot_preds <- left_join(x = titanic_boot_preds, y = preds_boot,
                              by = "rowname")
}

## examine some of the individual models
plot(boot_mods[[1]], gp = gpar(fontsize = 8))

plot(boot_mods[[10]], gp = gpar(fontsize = 6))

plot(boot_mods[[50]], gp = gpar(fontsize = 6))

# must convert factor into numeric, note that class "0" = 1, 
# and class "1" = 2, so we need to subtract 1 from every column
titanic_boot_preds %<>% mutate_if(is.factor, as.numeric) %>% 
  mutate_all(function(x){x - 1})

# calculate mean over all the bootstrap predictions
titanic_boot_preds <- titanic_boot_preds %>% 
  mutate(preds_bag = 
  select(., preds_boot1:preds_boot100) %>% 
  rowMeans(na.rm = TRUE))

# congratulations! You have bagged your first model!
ggplot(titanic_boot_preds, aes(x = preds_bag)) + 
  geom_histogram()


#---------------------------------------------------------------
# Random Forest
#---------------------------------------------------------------
library('randomForest')

rf_fit <- randomForest(Survived ~ 
                         Pclass + Sex + Age + SibSp + Fare, 
                       data = titanic_df,
                       type = classification,
                       mtry = 3,
                       na.action = na.roughfix,
                       ntree = 600)

print(rf_fit)

plot(rf_fit)

#---------------------------------------------------------------
# Tuning Random Forests To Determine
#  Optimal Parameters (mtry)
#---------------------------------------------------------------
rf_mods <- list()
oob_err <- NULL
test_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(Survived ~ 
                           Pclass + Sex + Age + SibSp + Fare, 
                         data = titanic_df,
                         mtry = mtry,
                         na.action = na.roughfix,
                         ntree = 600)
  oob_err[mtry] <- rf_fit$err.rate[600]
  
  cat(mtry," ")
}

results_DF <- data.frame(mtry = 1:9, oob_err)
ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point() + theme_minimal()


#---------------------------------------------------------------
# Random Forest Exercises
#---------------------------------------------------------------
# 1. Estimate a random forest model predicting survival using the predictors 
#    Pclass, Sex, Age, SibSp and Fare. First set mtry = 5, and select ntree = 400. 
#    Call this model rf_fit
# 2. Call the print function against rf_fit
# 3. call the plot() function against the fitted model and describe the plot. 
# 4. How many trees should we use to estimate the model?


