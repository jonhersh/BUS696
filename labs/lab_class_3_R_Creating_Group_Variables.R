

# ------------------------------------------------
# Loading data 
# ------------------------------------------------
library('tidyverse')

# the here package is very useful, it allows us to select across folders relative to our "home"
# directory of the project
# note here::here allows us to use the here function in the here package without loading it 

# download the IMDB_movies.csv dataset here, and store it in a subfolder called "datasets"
# https://github.com/jonhersh/MGSC310/tree/master/datasets

# OR you can run the code below to 
fs::dir_create(here::here("datasets"))

# this downloads a file from the net and stores it in your datasets folder
download.file("https://raw.githubusercontent.com/jonhersh/BUS696/master/datasets/IMDB_movies.csv", 
              here::here("datasets", "IMDB_movies.csv"), 
              method = "curl", 
              replace = TRUE)


movies <- read.csv(here::here("datasets", "IMDB_movies.csv"))

# ------------------------------------------------
# GLIMPSE to summarize data
# ------------------------------------------------

# see last lab

# ------------------------------------------------
# Pipe Operator!  
# ------------------------------------------------

# see last lab

# ------------------------------------------------
# Slice function: to select ROWS 
# ------------------------------------------------

# see last lab


# ------------------------------------------------
# Arrange function: to ORDER dataset
# ------------------------------------------------

# see last lab

# ------------------------------------------------
# SELECT columns of the dataset using the 'select' function
# ------------------------------------------------

# see last lab


# ------------------------------------------------
# RENAME variables using the RENAME function
# ------------------------------------------------

# see last lab

# ------------------------------------------------
# FILTER and ONLY allow certain rows using the FILTER function
# ------------------------------------------------

# see last lab



# ------------------------------------------------
# Exercises - on your own using the movies_clean dataset
# ------------------------------------------------

# see last lab




# ------------------------------------------------
# MISSING VALUES are values that are unknown in your dataset
# ------------------------------------------------
# R stores missing values as as NAs
is.na(NA)

is.na(c(1,2,NA))

sum(is.na(c(1,2,NA,NA)))

sum(is.na(movies %>% select(budget)))

is.na(1 + NA)

is.na(NA)
1 > NA
1 + 1 == NA

NA == NA
1/NA

mean(movies$duration, na.rm = TRUE)

sum(1, 2, NA)

sum(1,2,NA, na.rm = TRUE)

1 * NA
y <- NA
y
x <- 1
y == x

sum(y, na.rm = TRUE)

# ------------------------------------------------
# LOOP through numbers using the FOR loop 
# ------------------------------------------------

# for loops are created using the synthax
# for(i in start:end){
# do something with i
# }

for(j in 1:10){
  print(j)
}

for(i in 1:ncol(movie)){
  print() 
}

# we're #1 
# you're my #1

for(i in 1:ncol(movies)){
  # all the stuff we do
  # start for value of i = 1
  # ending at value of i = 5

  print(paste0("Variable name: ", names(movies)[i] ,", another string")) 
}

# we're #1 
# youre my #1 

for(k in 1:ncol(movies)){
  print(names(movies)[k])
}

for(i in 1:10){
  print(i)
}

# how to see how many missings you have in each column?
# well, we want to sum through every column using a for loop
# then print the variable name using names(movies[i])
# then print the sum of is.na() for just that variable

# for each column in the movies
for(i in 1:ncol(movies)){
  
  # print the following
  print(
    
    # first print "Variable: "
    paste0("Variable: ", 
           
           # then print the variable name, then "NAs: "
           names(movies)[i], " NAs: ", 
           
           # then print the sum of the number of missing values 
           # for that variable
           sum(is.na(movies %>% select(i)))
          )
        
        )
}

# we're #1 
# indexing with python?


# ------------------------------------------------
# Creating functions
# ------------------------------------------------
# we create a function in R by writing 
# function_name <- function(input1, input2,...){
  # function arguments
# }

sum_func <- function(var1, var2){
  print(var1 + var2)
}

sum_func(1, 10)

print_names <- function(data_frame){
  print(names(data_frame))
} 

print_names(movies)

# Let's take the code we wrote above and translate 
# it to a function called "num_missing". 
# We can then call the function and pass our movies dataframe 
# to it to export 
num_missing <- function(data_frame){
  for(i in 1:ncol(data_frame)){
    print(
      paste0("Variable name ", 
             names(data_frame)[i], " NAs: ", 
             sum(is.na(data_frame %>% select(i)))
      )
      
    )
  }
}

num_missing(movies)

data(mpg)
num_missing(mpg)

# ------------------------------------------------
# MUTATE to Transform variables in your dataset
# ------------------------------------------------

# adding new variables using mutate()
# note %<>% == DF <- DF %>%  
# let's create new varibles budgetM and grossM that 
# are budget and gross in units of millions
library('magrittr')


movies_clean <- movies %>%
            mutate(budgetM = budget/1000000,
                   grossM = gross/1000000,
                   profitM = grossM - budgetM,
                   lgross = log(gross + 1))

# inverse hyperbolic sin transformation 

movies %>% glimpse()

# so it looks like there's some outliers
# The most expensive movie ever made was Pirates of 
# the Caribbean: On Stranger Tides
# which cost $387.8m. Any movies with a budget higher 
# than this must be a data anomaly

# Let's use the filter command to remove these

movies_clean <- movies %>% filter(budgetM < 400) 


# ------------------------------------------------
# Find Duplicate Rows with duplicated() 
# and find_duplicates() (must install hablar package)
# ------------------------------------------------
# number of duplicated rows
movies %>% duplicated() %>% sum()

# view duplicated rows
# install.packages(hablar)
movies %>% hablar::find_duplicates()


# ------------------------------------------------
# Output final clean version of dataset
# ------------------------------------------------
# remove duplicate rows, create new budget and gross variables, 
# rename director and title
# remove budgets greater than 400M, 
# order title, year, budget, director and gross first, then store in new file 
movies_clean <- 
  movies %>% 
  distinct() %>% 
  mutate(budgetM = budget/1000000,
         grossM = gross/1000000,
         profitM = grossM - budgetM) %>%
  rename(director = director_name, 
         title = movie_title,
         year = title_year) %>% 
  relocate(title, year, country, director, budgetM, grossM, imdb_score) %>% 
  filter(budgetM < 400) 

movies_clean %>% glimpse()

movies %>% select_if(is.character) %>% names()


# ------------------------------------------------
# Create summary statistics by GROUP using group_by()
# ------------------------------------------------
# group summaries using summarise and group_by
director_avg <- 
  movies_clean %>% 
    # group_by() is used to indicate the grouping variable
  group_by(director) %>%
  
    # summarize creates a new variable based on this group
    # here we create averages by director using the 'mean'
    # function 
  summarize(gross_avg_director = mean(grossM, na.rm = TRUE))


# view results
director_avg %>% arrange(-gross_avg_director) %>% print() 


# slice to see more rows
director_avg %>% arrange(-gross_avg_director) %>% slice(1:20) 


# ------------------------------------------------
# Create grouped variables using the Summarize function
# n() creates counts by 
# sd() creates standard deviations
# ------------------------------------------------
# let's create budget by director, gross by director, profit by director, 
# number films by director 
director_df <- 
  movies_clean %>% 
  group_by(director) %>%
  summarize(
      
      # create average budget by director
      budget_avg_director = mean(budgetM, na.rm = TRUE),
      # create average gross by directory
      gross_avg_director = mean(grossM, na.rm = TRUE),
      # create average movie profit by director
      profit_avg_director = mean(profitM, na.rm = TRUE),
      # create variable that lists number of films 
      # by director
      num_films = n(),
      # create a standard deviation of profit
      # by director
      profit_sd_director = sd(profitM, na.rm = TRUE)
      
      )


director_df %>% 
  arrange(desc(profit_avg_director)) %>% 
            slice(1:20)



# ------------------------------------------------
# Exercises - on your own
# ------------------------------------------------
# 1. Print a dataframe with the film director name, and 
#     number of films for the 10 directors with the most films in the dataset
# 2. What movie genres have the highest average profit?  
#    (hint, must use a new group_by() command)
# 3. Which countries have the most films in the top 5000 IMDB database? 
#    (hint, must use a new group_by() command)
# 4. How many missing values are there for the profit_avg_director?
# 5. Why do some directors have “NA” for profit_avg_director? 
  
  




