###################################
###           R Lab 6           ###
###     Data Transformation     ###
###################################

### Overview ###

# This is one of the most important skills that you will learn in this course.
# Most of you will be doing this for the rest of your careers, so it pays to learn it well now.

library(broom)
library(tidyverse)
library(here)
# install.packages("tidylog")
library(tidylog)

# Cleaning data is the process of transforming raw data files into something "usable"
# What is "usable" depends on context


# Tips
#   1. ALWAYS be thinking about what you want your final data set to look like
#               Typically this has to do with your research question
#               What variables do you want in the regression, and at what level?
#   2. Before digging into a data set, make sure you know the UNIT of ANALYSIS.
#               i.e. what do rows in the data represent?
#   3. NEVER overwrite raw data files.


# There are several ways of cleaning data in R
#        - base R: clunky, ugly, boring! Weird syntax!
#        - tidyverse: One of the main advantages of R over Stata or Python


# Let's recap some of the basic cleaning functions in dplyr

pets <- data.frame(
  names = c("bougie", "maggie", "susan", "pete"),
  species = c("cat", "dog", "cat", "parrot"),
  age = c(8, 2, 6, 1)
)
pets

# dplyr::mutate Add/modify columns!
pets %>% 
  mutate(dog_years = age * 7)    # New column

pets %>% 
  mutate(age = age * 12)         # Changed existing column

# dplyr::select Dropping/Rearranging columns!
pets %>% 
  select(names, age)             # Keep specific columns

pets %>% 
  select(-names)                 # Drop specific columns

pets %>% 
  select(age, everything())      # everything() is nice for rearranging

pets %>% 
  select(contains("a"))          # Keep only columns containing the character "a"

# dplyr::filter Removing unwanted rows!
pets %>% 
  filter(age == 8)                          # Only keep rows where age is 8

pets %>% 
  filter(names %in% c("bougie", "maggie"))  # Only keep rows where names are in the vector

pets %>%  
  filter(age > 7, species == "cat")         # Only keep rows that match both conditions

pets %>% 
  filter(names != "pete")                   # Only keep rows where names are NOT "pete"



# Those are some of the basics. Now lets do something harder!

# Joins
# The way we merge two data sets "side by side"
# You have columns/variables in one data set that you want matched to another data set
# Key idea: find the variable which links both data sets first

foods <- data.frame(species = c("cat", "dog", "rabbit"),
                    fav_food = c("fish", "cheese", "carrots"))
# What is the unit of analysis here?


# The most common join: left_join
# Keeps all observations in the "left", or main, data set
# Only adds observations from the "right" or new data that match
pets %>% 
  left_join(foods, by = "species")
# Why is pete's favorite food NA?

# right_join
# Keeps all observations in the "right", or new, data set
# Only adds observations from the "left", or main, data that match
pets %>% 
  right_join(foods, by = "species")
# Less common because our brains work left to right? At least for English speakers?

# inner_join
# Keeps only observations which match in BOTH data sets!
# Proceed with caution...
pets %>% 
  inner_join(foods, by = "species")

# full_join
# Keeps ALL observations!
# Sounds fun, but often not a good idea
pets %>% 
  full_join(foods, by = "species")

# What if one data set has more than one observation per unit?
foods <- foods %>% 
  rbind(c("cat", "trash"))

pets %>% 
  left_join(foods, by = "species")   # Be careful

# Tips for joins
#    1. left_join is by far the most common and most useful
#           Typically we have a "main" data set and want to add things that match
#    2. tidylog package is your friend!
#           It will help you catch so many problems
#    3. Test out your join before overwriting the original data set



# Next: aggregating your data (i.e. changing the unit of analysis!)
# What if we have multiple observations per unit, and want to condense this information?
# We use group_by() and summarise() in dplyr :)

# First let's add some more friends to our data
pets2 <- data.frame(names = c("mitten", "dorothy", "toto", "blue", "cupcake"),
                    species = c("cat", "dog", "parrot", "dog", "parrot"),
                    age = c(12, 2, 3, 10, 4))

pets <- pets %>% 
  rbind(pets2) %>%         # stacking rows with rbind()
  arrange(species)         # reorder rows with arrange()
pets
# What is the unit of analysis?

# Let's find the average age within each species
pets %>% 
  group_by(species)
pets

# group_by doesn't "do" anything on its own.
# But it modifies how all functions below it in the pipeline operate

pets %>% 
  group_by(species) %>%                  
  mutate(species_avg_age = mean(age))           # Mean ages within species group
pets

# Use ungroup() in more complicated sequences and as a good habit
pets %>% 
  group_by(species) %>% 
  mutate(species_avg_age = mean(age)) %>% 
  ungroup() %>%
  mutate(dog_age = age * 7)

# Ok that is cool, but what if we only care about species-level data?
# summarise!
pets %>% 
  group_by(species) %>% 
  summarise(avg_age = mean(age))    # summarise() can mutate and collapse your data
# Now what is the unit of analysis?

# Other fun ways to summarise
pets %>% 
  group_by(species) %>% 
  summarise(total_age = sum(age))

pets %>% 
  group_by(species) %>% 
  summarise(oldest_age = max(age))

pets %>% 
  group_by(species) %>% 
  summarise(difference_in_ages = max(age) - min(age))

pets %>% 
  group_by(species) %>% 
  summarise(median_age = median(age), obs = n(), oldest_age = max(age))


# mutate, select, filter, joins, group_by, summarise
# these are the functions you need for the problem set!

# Real data exercise!

# Research question: did US counties with a higher manufacturing sector
# swing disproportionately in favor of Trump in 2016?

# First step --- What will our final regression look like?
#   Which variables do we need?
#   What level should they be at? Individual counties



# Let's start with the dependent variable
# Conceptually: a county's swing towards Trump in 2016
# Operationalized: Trump's vote margin in 2016 minus Romney's vote margin in 2012

# Load in a data set with county-level election returns for 2016 and 2012
counties <- read_csv(here("week-6", "data", "county_elections.csv"))
# View(counties)
# Always look at your data before going wild with it!

# First we need to calculate total votes by county
counties %>% 
  mutate(total_votes_2016 = trump16 + clinton16 + otherpres16,
         total_votes_2012 = romney12 + obama12 + otherpres12) %>% 
  select(total_votes_2016, total_votes_2012, everything()) %>% 
  View()

# Now calculate the vote margins and difference
counties %>% 
  mutate(total_votes_2016 = trump16 + clinton16 + otherpres16,
         total_votes_2012 = romney12 + obama12 + otherpres12,
         trump_difference = trump16 / total_votes_2016 - romney12 / total_votes_2012) %>% 
  select(trump_difference, everything()) %>% 
  View()

# Saving our work
counties <- counties %>% 
  mutate(total_votes_2016 = trump16 + clinton16 + otherpres16,
         total_votes_2012 = romney12 + obama12 + otherpres12,
         trump_difference = trump16 / total_votes_2016 - romney12 / total_votes_2012)
# Note: sometimes it is dangerous to overwrite the original data, but sometimes it is useful
#    counties <- counties %>%  ...
# When working with smallish data, often I will pipe directly off the read_csv command
#    counties <- read_csv(here("week-6", "data", "county_elections.csv")) %>%  ...
# This ensures a fresh load of the data every time, and can save you a lot of headaches



# Great!
# Now let's prepare out main independent variable
# Conceptually: a county's dependency on manufacturing employment
# Operationalized: the average proportion of county employment in manufacturing from 2014 to 2016

# Load in a data set with employment data from 2014 to 2016
# County Business Patterns
cbp <- read_csv(here("week-6", "data", "cbp_mini.csv")) 
# fips - 5 digit US county codes
# naics - North American Industry Classification System
# These are industry codes. "31" is the heading for manufacturing

# What is the unit of analysis here??

# We need employment proportions, but we are given raw counts
# First step is to calculate county-year employment totals
cbp %>% 
  group_by(year, fips) %>%            # Grouping by two variables(!!)
  mutate(total_emp = sum(emp)) %>%    # Calculating the sum of values from "emp" by group
  View()

# Now that we have county-year totals in employment
# we can find the proportion that falls under the heading "31----"
cbp %>% 
  group_by(year, fips) %>% 
  mutate(total_emp = sum(emp)) %>% 
  ungroup() %>%                              # Un-group for safety!
  filter(naics == "31----") %>%              # Keep only matching rows
  mutate(prop_manuf = emp / total_emp) %>%   # Calculate proportion manufacturing per county-year
  View()
# Notice that our filter() command subtly changed the unit of analysis

# Last step: collapse the data down from county-year level to county level
cbp %>% 
  group_by(year, fips) %>% 
  mutate(total_emp = sum(emp)) %>% 
  ungroup() %>% 
  filter(naics == "31----") %>% 
  mutate(prop_manuf = emp / total_emp) %>% 
  group_by(fips) %>%                            # Grouping by county
  summarise(prop_manuf = mean(prop_manuf)) %>%  # Summarising by average prop_manuf per group
  View()
# Looks good!!!

# Saving the work
cbp <- cbp %>% 
  group_by(year, fips) %>% 
  mutate(total_emp = sum(emp)) %>% 
  ungroup() %>% 
  filter(naics == "31----") %>% 
  mutate(prop_manuf = emp / total_emp) %>% 
  group_by(fips) %>%
  summarise(prop_manuf = mean(prop_manuf))


# Moving on to an important(?) control variable
# Level of economic inequality in a state. 
# GINI coefficient: 1 = very unequal, 0 = very equal
state_gini <- read_csv(here("week-6", "data", "nhgis2016state_gini.csv"))

# What is the unit of analyis :)

# Let's rename the gini coefficent variable to something better
state_gini <- state_gini %>% 
  rename(state_gini_coef = AGL1E001)

# How are we going to merge this variable into our county data set?
# The variable for state, STATEA, in the gini data set
# doesn't match anything in the "counties" data set

# We need a "crosswalk" file
# This is a data set that helps you merge two other data sets together

state_icpsr <- read_csv(here("week-6", "data", "stateicpsrfipscrosswalk.csv"))

# Let's add these state identifiers to our main "counties" data set!
counties %>% 
  left_join(state_icpsr, by = c("state_abbrv" = "statepa"))
# 'by = c("left_key_variable" = "right_key_variable")' saves you from renaming variables
# But you may find it easier just to rename variables so they match

state_icpsr <- state_icpsr %>% 
  rename(state_abbrv = statepa)
# Then
counties <- counties %>% 
  left_join(state_icpsr) 
# We don't even have to specify "by = " in this case because there is only one
# variable name in common! 
# But you should check the tidylog in your console to inspect what happened

# Look at all our new lovely variables
View(counties)

# Let's try to merge the state-level gini data into our county-level data!
counties %>% 
  left_join(state_gini, by = c("statefips" = "STATEA"))
# What happened?

# Incompatible types will throw an error when trying to join data sets
# Out county variable is a "double" or numeric value
# but our state variable is a character value!

# We need to change one of these to match the other's type before trying to join
# Typically it is better to change numeric variables to character variables here
# State Code is a categorical variable, numbers have no numeric meaning
# We can't add and subtract State Codes!

# So let's change the "statefips" variable to a character type
counties %>% 
  mutate(statefips = as.character(statefips)) %>% 
  View()
# Something is not quite right! Where are the leading zeros?
# The single-digit state codes will not match like this :(

counties <- counties %>% 
  mutate(statefips = str_pad(statefips, width = 2, pad = "0"))
# str_pad() not only converts a variable to a character, 
# but it will "pad" the values out to a certain width
# VERY useful function in a lot of contexts

# Finally we are ready to join :)
counties <- counties %>% 
  left_join(state_gini, by = c("statefips" = "STATEA"))


# Last step:
# Join in our prop_manuf variable from the cbp data set
counties %>% 
  left_join(cbp, by = c("countyCode" = "fips"))
# Thwarted again!

# We know how to fix this
counties <- counties %>% 
  mutate(fips = str_pad(countyCode, width = 5, pad = "0")) %>% 
  select(-countyCode) %>%     # Let's tidy up our variable names while we're at it
  left_join(cbp)
# Tidylog tells us there were some failures to match :(
# In real life you would want to understand exactly why these did not match

# anti_join is sometimes useful for this
counties %>% 
  anti_join(cbp) %>% 
  View()
# This will return the rows in "counties" that could not be matched to "cbp"


# Last last step:
# Let's quickly recode our proportions/percents to make them easier to interpret
counties <- counties %>% 
  mutate(prop_manuf = prop_manuf * 100,
         trump_difference = trump_difference * 100)
# Now a "one-unit-change" will correspond to a one percentage point change in our regression




# Finally it is time to run our model!
mod <- lm(trump_difference ~ prop_manuf + state_gini_coef + white_pct + lesscollege_pct,
          data = counties)

tidy(mod)






