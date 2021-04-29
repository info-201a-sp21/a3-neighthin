# Assignment 3: Using Data
#
# Before you get started:
# - Set your working directory to "source file location" using the Session menu
# - Run the following line of code to delete all variables in your workspace
#     (This will make it easier to test your script)
rm(list = ls())

### Built in R Data ###########################################################

# In this section, you'll work with the variable `Titanic`, a data set which is
# built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.

is.data.frame("Titanic")

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`

titanic_df <- data.frame(Titanic)

# It's important to understand the _meaning_ of each column before analyzing it
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: [shows if observations were in the 1st, 2st, 3rd class or a crew mate]
# Sex: [Differentiates between male and female]
# Age: [Whether the observation is either an adult or a child]
# Survived: [Whether they lived or passed away]
# Freq: [The amount of observations that either survived or didn't]


# Create a variable `children` that is a data frame containing only the rows
# from `titanic_df` with information about children on the Titanic
# Hints:
# - Filter rows using a vector of boolean values (like vector filtering)
# - See chapter 10.2.3

children <- titanic_df[titanic_df$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: Remember the `sum()` function!

num_children <- sum(children$Freq)

# Create a variable `most_lost` that is the *row* from `titanic_df` with the
# largest absolute number of losses (people who did not survive)
# You can use multiple lines of code if you find that helpful
# to create this variable
# Hint: Filter for those who did not survive, then look for the row

children_not_survived <- titanic_df[titanic_df$Survived == "No", ]
most_loss <- children_not_survived[children_not_survived$Freq ==
                                  max(children_not_survived$Freq), ]

# Define a function called `survival_rate()` that takes in two arguments which
# must be in *the following order*:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
#
# This function should return a sentence that states the *survival rate*
# (# survived / # in group) of adult men and "women and children" in that
# ticketing class.
# It should read (for example):
# >"Of Crew class, 87% of women and children survived and 22% of men survived."
#
# This is a complicated function! We recommend the following approach:
# - Filter for all rows representing the given ticketing class and save the
#   new data frame to a variable
# - Using this data frame, filter for all rows representing Adult Males
# - Find the total number of men and total number of male survivors to
#   calculate the survival rate
# - Likewise, use the data frame to filter for all Children and Adult Females
# - Perform the above calculation for this group as well
#
# Other approaches are also acceptable, please comment to explain what you do!

survival_rate  <- function(ticket_class, df) {
  # To filter for males that survived
  ticketing_class <- df[df$Class == ticket_class, ]
  adult_males <- ticketing_class[ticketing_class$Sex == "Male" &
                                ticketing_class$Age == "Adult", ]
  total_men <- sum(adult_males$Freq)
  males_survived <- adult_males[adult_males$Survived == "Yes", ]
  total_male_survivors <- sum(males_survived$Freq)
  male_survival_rate <- ((total_male_survivors / total_men) * 100)
  # To filter for females and children survived
  female_children_class <- ticketing_class[ticketing_class$Sex != "Male", ]
  total_fem_child <- sum(female_children_class$Freq)
  fem_child_survivors <- female_children_class[female_children_class$Survived
                                               == "Yes", ]
  total_fem_child_survivors <- sum(fem_child_survivors$Freq)
  fem_child_survival <- (total_fem_child_survivors / total_fem_child) * 100
  return_setence <- paste0("Of ", ticket_class, " class, ", fem_child_survival,
                           "% of women and children survived and ",
                           male_survival_rate, "% of men survived.")
  return(return_setence)
}

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to your `survival_rate` function

first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)

# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
# [YOUR ANSWER HERE]


# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
# [YOUR ANSWER HERE]


### Reading in Data ###########################################################

# In this section, you'll work with .csv data of life expectancy by country
# First, download the csv file of `Life Expectancy` data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory


# Before getting started, explore the GapMinder website to better understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., 1 - 2 sentences
# per data source)

# [The first source is by Mattias Lindgren from the period 1800-1970. This data
# was based on 100 sources compiled by Mattias. He was also the one who assessed
# the fatalities of big disasters and made guesstimates of life expectancy dips]

# [The second source is by IHME for the periods 1970-2016. The IHME used the
# Global Burden of Disease Study 2017, which has data from 1990-2017.]

# [The third source is by UN which deals with the periods 2017-2099. They used
# the UN forecasts from "World Populations Prospects 2019".]


# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`.

life_exp <- read.csv("data/life_expectancy_years.csv",
                     stringsAsFactors = FALSE)

# Write a function `get_col_mean()` that takes a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
# Hint: `mean()` takes in an argument called `na.rm`

get_col_mean <- function(col_name, df) {
  answer <- df[, col_name]
  mean(answer, na.rm = TRUE)
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
# Hint: Use an `*apply` function (lapply, sapply, etc.)

col_means <- as.list(sapply(colnames(life_exp)[-1], get_col_mean, life_exp))

# Create a variable `avg_diff` that is the difference in average country life
# expectancy between 1800 and 2018

avg_diff <- get_col_mean("X1800", life_exp) - get_col_mean("X2018", life_exp)

# Create a column `life_exp$change` that is the change in life
# expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*

life_exp$change <- life_exp$X2018 - life_exp$X2000

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy. Make sure to filter NA values
# Hint: `max()` takes in an argument called `na.rm`

most_improved <- life_exp[life_exp$change == max(life_exp$change, na.rm = TRUE)
                          & !is.na(life_exp$change), "country"]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values
# Hint: Lookup `is.na()`

num_small_gain <- length(life_exp$country[life_exp$change < 1 &
                                            !is.na(life_exp$change)])

# Write a function `country_change()` that takes in a country's name,
# two years as numbers (not strings), and the `life_exp` data frame
# Parameters should be written *in the above order*
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"
# Hint: Use an if/else statement to help compute DIRECTION

country_change <- function(country_name, year_1, year_2, df) {
  country_df <- df[df$country == country_name, ]
  year_1_expect <- country_df[[paste0("X", year_1)]]
  year_2_expect <- country_df[[paste0("X", year_2)]]
  year_difference <- year_2_expect - year_1_expect
  if (year_difference < 0) {
    direction <- "down"
  } else {
    direction <- "up"
  }
  paste0("Between ", year_1, " and ", year_2, ", the life expectancy in ",
         country_name, " went ", direction, " by ", abs(year_difference),
         " years.")
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden

sweden_change <- country_change("Sweden", 1960, 1990, life_exp)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations)
# Hint: Use an if/else statement to paste the countries in the correct order

compare_change <- function(country_1, country_2, df) {
  country_1_change <- df[df$country == country_1, "change"]
  country_2_change <- df[df$country == country_2, "change"]
  gain <- country_1_change - country_2_change
  if (country_1_change < country_2_change) {
    biggest <- country_2
    biggest_gain <- country_2_change
    smallest <- country_1
    smallest_gain <- country_1_change
  } else {
    biggest <- country_1
    biggest_gain <- country_1_change
    smallest <- country_2
    smallest_gain <- country_2_change
  }
  paste0("The country with the bigger change in life expectancy was ", biggest,
         " (gain=", round(biggest_gain, 1),
         "), whose life expectancy grew by ", round(abs(gain), 1),
         " years more than ", smallest, "'s (gain=", round(smallest_gain, 1),
         ").")
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)

usa_or_france <- compare_change("United States", "France", life_exp)

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.

write.csv(life_exp, "data/life_exp_with_change.csv", row.names = FALSE)
