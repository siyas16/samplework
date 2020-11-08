# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# - <https://miro.medium.com/max/4582/1*O4LZwd_rTEGY2zMyDkvR9A.png>


# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`
setwd("~/exercises-siyas16/a4-siyas16/data")

# Load the `dplyr` package
library("dplyr")

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data, making sure to not interpret strings as factors.
kickstarters <- read.csv("ks-projects-201801.csv", stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?
columns <- colnames(kickstarters)
num_of_rows <- nrow(kickstarters)
num_of_columns <- ncol(kickstarters)

# Use the `summary` function to get some summary information
summary_info <- summary(kickstarters)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(col_name, frame) {
  values <- pull(frame, col_name)
  if (mode(values) == "numeric") {
    summary_list <- list(min = min(values), 
                         max = max(values), 
                         mean = mean(values))
  } else if (length(unique(values)) <= 10) {
    summary_list <- list(n_values = length(unique(values)), 
                         unique_values = unique(values))
  } else {
    summary_list <- list(n_values = length(unique(values)),
                         sample_values = sample(unique(values), 10))
  }
  return(summary_list)
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
category_info <- get_col_info("category", kickstarters)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(frame) {
  answer <- lapply(columns, get_col_info, frame)
  names(answer) <- columns
  return(answer)
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
all_columns_info <- get_summary_info(kickstarters)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# 1) The amount of categories (159) of categories of projects seems so large. 
# 2) There are 6 possible states for the projects to be in and I'd like more 
#    info on how many projects are successful.
# 3) The large difference in backers (0 vs. 219382) makes me think that some of 
#    these projects must have been really dumb for them to have no support.

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?
highest_goal_projects <- kickstarters %>%
                  filter(usd_goal_real == max(usd_goal_real, na.rm = TRUE)) %>%
                  pull(name)

# What was the category of the project(s) with the lowest goal?
lowest_goal_projects <- kickstarters %>%
                  filter(usd_goal_real == min(usd_goal_real, na.rm = TRUE)) %>%
                  pull(name)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find 
deadline_of_2018 <- kickstarters %>%
                    filter(substring(deadline, 1, 4) == "2018") %>%
                    summarize(unsuccessful = n()) %>%
                    pull(unsuccessful) 

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
proportion_unsuccessful <- kickstarters %>% 
                           filter(state != "successful") %>% 
                           summarize(unsuccessful = n()) %>%
                           pull(unsuccessful) / num_of_rows * 100
                           
# What was the amount pledged for the project with the most backers?
money_for_most_backed <- kickstarters %>%
                         filter(backers == max(backers, na.rm = TRUE)) %>%
                         pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
most_money_to_failed <- kickstarters %>%
            filter(state == "failed") %>%
            filter(usd_pledged_real == max(usd_pledged_real, na.rm = TRUE)) %>%
            pull(name)

# How much total money was pledged to projects that weren't marked successful?
pledged_to_unsuccessful <- kickstarters %>%
                        filter(state != "successful") %>%
                        summarize(sum = sum(usd_pledged_real, na.rm = TRUE)) %>%
                        pull(sum)

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
most_pledged <-  kickstarters %>%
          group_by(category) %>%
          summarize(usd_pledged_real = sum(usd_pledged_real, na.rm = TRUE)) %>%
          filter(usd_pledged_real == max(usd_pledged_real, na.rm = TRUE)) %>%
          pull(category)

# Which country had the most backers?
most_backers <- kickstarters %>% 
                group_by(country) %>%
                summarize(backers = sum(backers, na.rm = TRUE)) %>%
                filter(backers == max(backers, na.rm = TRUE)) %>%
                pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
year_most_pledged <- kickstarters %>% 
            mutate(year = substring(deadline, 1, 4)) %>%
            group_by(year) %>%
            summarize(usd_pledged_real = max(usd_pledged_real, na.rm = TRUE)) %>%
            filter(usd_pledged_real == max(usd_pledged_real, na.rm = TRUE)) %>%
            pull(year)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from:
# I chose to get the year from deadline because I had already done it in
# another question and because people probably pledged before the deadline so
# it makes more sense to use those numbers.

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3 <- kickstarters %>%
         filter(substring(deadline, 1, 4) == "2018") %>%
         group_by(category) %>%
         summarize(backers = sum(backers, na.rm = TRUE)) %>%
         top_n(3, backers)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_common_day <- kickstarters %>%
                   mutate(day = weekdays(as.Date(launched))) %>%
                   group_by(day) %>%
                   summarize(amount = n()) %>%
                   filter(amount == max(amount, na.rm = TRUE)) %>%
                   pull(day)
                   
# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"
least_successful_day <- kickstarters %>%
          mutate(day = weekdays(as.Date(launched))) %>%
          group_by(day) %>%
          summarize(success_total = sum(state == "successful", 
                                        na.rm = TRUE) / n()) %>%
          filter(success_total == min(success_total, na.rm = TRUE)) %>%
          pull(day)
  
