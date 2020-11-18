

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
download.file("https://raw.githubusercontent.com/jonhersh/MGSC310/master/datasets/IMDB_movies.csv", 
              here::here("datasets", "IMDB_movies.csv"), 
              method = "curl", 
              replace = TRUE)


movies <- read.csv(here::here("datasets", "IMDB_movies.csv"))

# ------------------------------------------------
# GLIMPSE to summarize data
# ------------------------------------------------
# let's summarize the data using the glimpse function
glimpse(movies)


# ------------------------------------------------
# Pipe Operator!  
# ------------------------------------------------
# The pipe operator "%>%" is super useful!
# It allows us to execute a series of functions on an object in stages
# The general recipe is Data_Frame %>% function1() %>% function2() etc
# Functions are applied right to left

movies %>% glimpse()
glimpse(movies)

# cmd shift 

movies %>% glimpse() 
glimpse(movies)



# ------------------------------------------------
# Slice function: to select ROWS 
# ------------------------------------------------
# SLICE: slice to view only the first 10 rows
movies %>% slice(1:10)

# SLICE to view only rows 300 to 310 
movies %>% slice(300:310)


# ------------------------------------------------
# Arrange function: to ORDER dataset
# ------------------------------------------------

# arrange the dataframe in descening order by budget, and store this back as movies
movies <- movies %>% arrange(desc(budget))

# arrange the dataframe in ascending order by budget and store this back as movies
movies <- movies %>% arrange(desc(budget))

# arrange via multipe columns, by budget and title year, then output rows 1 to 10
movies %>% 
  arrange(desc(budget), desc(title_year)) %>% 
  slice(1:10)

# ------------------------------------------------
# SELECT columns of the dataset using the 'select' function
# ------------------------------------------------
# selecting columns using the select() function
# here we create a subset of the original dataset that only contains director_name and movie title
movies_keys <- movies %>%  select(director_name, movie_title)
glimpse(movies_keys)

# using select to programmatically select several variables that 'start with' a certain string
movies_actors <- movies %>% select(starts_with("actor"))
glimpse(movies_actors)

# here we 
# everything() is a useful function, and 
movies <- movies %>% select(director_name, movie_title, title_year, everything())
glimpse(movies)

# ------------------------------------------------
# RENAME variables using the RENAME function
# ------------------------------------------------

# use the rename function to rename variables
movies <- movies %>%  rename(director = director_name)
glimpse(movies)

# ------------------------------------------------
# FILTER and ONLY allow certain rows using the FILTER function
# ------------------------------------------------
# filter removes any rows that DO NOT meet the logical operator


# ONLY select large budget movies and store this as a new data frame
movies_big <- movies %>% filter(budget > 100000000)
glimpse(movies_big)

# ONLY select english language films and store this as a new data frame
movies_eng <- movies %>% filter(language == "English")
glimpse(movies_eng)
dim(movies_eng)


# select both ENGLIGH and SPANISH films
movies_lang <- movies %>% filter(language == "English" | language == "Spanish")
dim(moviesSub)

# select big budget spanish films and store this as a new dataset
movies_big_spanish <- movies %>% filter(language == "Spanish" | budget > 1e10)
dim(movies_big_spanish)
glimpse(movies_big_spanish)


# ------------------------------------------------
# Factors -- record strings as numerics and a 'label' for that numeric value
# ------------------------------------------------
# see unique values of a factor
unique(movies_eng$language)
is.character(movies_eng$language)
is.factor(movies_eng$language)
head(movies_eng)


# ------------------------------------------------
# MISSING VALUES are values that are unknown in your dataset
# ------------------------------------------------
# R stores missing values as as NAs
is.na(NA)
1 > NA
1 + 1 == NA
NA == NA
y <- NA
y
x <- 1
y == x

# ------------------------------------------------
# LOOP through numbers using the FOR loop 
# ------------------------------------------------
# how to see how many missings you have in each column?
# well, we want to sum through every column using a for loop
# then print the variable name using names(movies[i])
# then print the sum of is.na() for just that variable
for(i in 1:ncol(movies)){
  print(
    paste0("Variable: ", 
           names(movies)[i], " NAs: ", 
           sum(is.na(movies %>% select(i)))
          )
        
        )
}

# ------------------------------------------------
# MUTATE to Transform variables in your dataset
# ------------------------------------------------

# adding new variables using mutate()
# note %<>% == DF <- DF %>%  
# let's create new varibles budgetM and grossM that 
# are budget and gross in units of millions
movies %<>% mutate(budgetM = budget/1000000,
                   grossM = gross/1000000,
                   profitM = grossM - budgetM)

movies %>% glimpse()

# so it looks like there's some outliers
# The most expensive movie ever made was Pirates of 
# the Caribbean: On Stranger Tides
# which cost $387.8m. Any movies with a budget higher 
# than this must be a data anomaly

# Let's use the filter command to remove these

movies_clean <- movies %>% filter(budgetM < 400) 


# ------------------------------------------------
# Remove Duplicates with distinct()
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


# ------------------------------------------------
# Exercises - on your own using the movies_clean dataset
# ------------------------------------------------
# 1. What are the highest grossing Steven Spielberg films?
# 2. What's the highest grossing film in the dataset?
# 3. Which film lost the most money?
# 4. Which film made the most money?
# 5. How many "PG-13" movies are there in the database?
# 6. Which movie has the most facebook likes?
# 7. Make 1-2 interesting ggplots using the movies_clean dataset


# ------------------------------------------------
# Create summary statistics by GROUP using group_by()
# ------------------------------------------------
# group summaries using summarise and group_by
director_avg <- 
  movies_clean %>% 
  group_by(director) %>%
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
  summarize(budget_avg_director = mean(budgetM, na.rm = TRUE),
            gross_avg_director = mean(grossM, na.rm = TRUE),
            profit_avg_director = mean(profitM, na.rm = TRUE),
            num_films = n(),
            profit_sd_director = sd(profitM, na.rm = TRUE)
            )


director_df %>% 
  arrange(desc(profit_avg_director)) %>% 
            slice(1:20)



# ------------------------------------------------
# Exercises - on your own using the director_df dataset
# ------------------------------------------------
# 1. Which director made the most films in the T 5000 database
# 2. Which director has the highest standard deviation of profit?
# 3. Which director has the highest profit? 
# 4. Which director has the lowest profit? 
# 5. How many movies has George Lucas Made?
# 6. Make 1-3 ggplots using the director_df showing revealing patterns. 

