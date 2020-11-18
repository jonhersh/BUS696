#------------------------------------------------------------
# Load data and packages
#------------------------------------------------------------
library('tidyverse')
library('lubridate')

AB <- read.csv(here::here("datasets","ab_data.csv"))

head(AB)

#---------------------------------------------------------------
# clean data file to prepare for analysis
#---------------------------------------------------------------

# group by user_id, arrange by timestamp, and remove 
# duplicate IDs
# create a day date variable from the raw 
#  timestamp information
AB_clean <- AB %>% group_by(user_id) %>% 
  arrange(timestamp) %>% 
  filter(!duplicated(user_id)) %>% 
  mutate(day = as.Date(timestamp))

# and group by date and control/treatment indicator
AB_sum <- AB_clean %>% 
  group_by(day, group) %>% 
  summarize(conv_rate = mean(converted))

print(AB_sum)

#---------------------------------------------------------------
# Plot difference between treated and control
#---------------------------------------------------------------
ggplot(AB_sum, aes(x = day, y = conv_rate, color = group, group = group)) + 
  geom_point() + 
  geom_line() +
  theme_minimal()


#---------------------------------------------------------------
# Estimate model to identify treatment effect
#---------------------------------------------------------------
AB_clean <- AB_clean %>% 
  mutate(treated = factor(group, 
         levels = c("control","treatment")))

logit_mod <- glm(converted ~ treated,
                 data = AB_clean, 
                 family = "binomial")

summary(logit_mod)


#---------------------------------------------------------------
# Cluster standard errors around day 
#  and control for covariates (day effect)
#---------------------------------------------------------------
library('miceadds')
library('sandwich')
logit_cluster <- glm.cluster(converted ~ 
                               treated + factor(day),
                          cluster = "day",
                          data = AB_clean)

summary(logit_cluster)


#---------------------------------------------------------------
# Read in Cookie Cats Data
#---------------------------------------------------------------

cats <- read.csv(here::here("datasets","cookie_cats.csv"))
head(cats)
# userid - unique player ID
# version - whether the player was put in the control group 
# (gate_30 - a gate at level 30) 
#  or the test group (gate_40 - a gate at level 40).
# sum_game rounds - the number of game rounds played by the player 
#    during the first week after installation
# retention_1 - did the player come back and play 1 day after installing?
# retention_7 - did the player come back and play 7 days after installing?

# 1. Arrange by userid and remove any duplicate IDs that exist
#    Use the mutate function to create a "treated" variable = 1
#    if player was in the treated category where the gate was set to level 40

cats_clean <- cats %>% 
  arrange(userid) %>% 
  filter(!duplicated(userid)) %>% 
  mutate(treated = if_else(version == "gate_40",1,0),
         ret_1 = if_else(retention_1 == "TRUE",1,0),
         ret_7 = if_else(retention_7 == "TRUE",1,0))

# 2. Estimate the treatment impact of the gate 40 
#    intervention on day 1 retention 

# 3. Estimate the treatment impact of the gate 40 
#    intervention on day 7 retention 

# 4. Estimate the treatment impact of the gate 40 
#    intervention on game rounds played

# 5. What do you conclude? Should they adopt the treatment?
#    Why or why not?

