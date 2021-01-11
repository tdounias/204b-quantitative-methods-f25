
########################################################
##                        Lab 1                       ##
## Introduction to working with data and making plots ##
########################################################



# We can create our own data in R using the function data.frame(). The left hand
# values below are the names of columns. The right hand vectors are the values
# that go into each column. It is difficult to keep rows organized using this
# method, so we rarely do this in practice! 
# 
# The end result is a data.frame
# object called "pets", with each row representing a specific pet, and the
# columns the characteristics of each pet.
pets <- data.frame(name = c("monkey", "bougie", "mitten", "maggie", "carrots"),
                   species = c("cat", "cat", "cat", "dog", "gerbil"),
                   age = c(8, 8, NA, 2, 1),
                   friendly = c(1, 0, 1, 1, 1))

# To interactively inspect the pets data, using the handy function View(). Or click on the pets object in your Environment tab.
View(pets)

# If we want to change/manipulate this data, we can do so using the tools in the `dplyr` package.
# 
# If you haven't done so yet, you can install the package with:
# install.packages("dplyr")

# After installing a package, you must load the package into your R environment using the library() function. This will have to be done every time you open a fresh RStudio.
library(dplyr)

# Let's add a new variable that is the age of each pet in dog-years! We will define dog-years as the actual age multiplied by 8
# We can do this with mutate() from the dplyr package.
pets <- mutate(pets, dog_age = age * 8)
# This "overwrites" the old pets data with a new version containing the column "dog_age"
View(pets)

# A better way to use dplyr commands is by using the pipe operator: %>% 
# This allows us to string together several commands at once - all building on the original data object ("pets" in this case)
#
# This is equivalent to the code above
pets <- pets %>% 
  mutate(dog_age = age * 8)

# If we wanted to make a new variable ordinal variable for age with the categories "young", "adult", "old" we could use the case_when() function within a mutate.
# In this case, each value of "age" in our data set is checked against the following conditions. If it is less than 2, the pet is given the value "young" for the age_old column for example
pets <- pets %>% 
  mutate(age_ord = case_when(age < 2 ~ "young",
                             age >= 2 & age < 7 ~ "middling_age",
                             age > 7 ~ "old",
                             TRUE ~ "deceseased"))

# Another important data wrangling procedure involves dropping unwanted columns
# In dplyr we do this with the select() function to only keep the columns we want
pets_namespecies <- pets %>% 
  select(name, species)

# We can also directly drop columns with a minus sign if that makes more sense
pet_namespecies <- pets %>% 
  select(-age, -friendly, -dog_age, -age_ord)

# The third major data manipulation task is dropping rows, typically based on some condition
# We do this in dplyr with filter()
friendly_pets <- pets %>% 
  filter(friendly == 1)

# Pipes really shine when we chain multiple dplyr commands together!
pets <- pets %>% 
  mutate(age_ord = case_when(age < 2 ~ "young",
                             age >= 2 & age < 7 ~ "middling_age",
                             age > 7 ~ "old",
                             TRUE ~ "deceseased")) %>% 
  filter(age_ord == "young") %>% 
  select(name, friendly)

# Once we are happy with our data, it is usually a nice idea to save it somewhere
# The best format for doing this in R is as an .RData or .rda file
#
# The package "here" is an excellent tool for making it easier to specify file paths
# Instead of "/Users/bertrandwilden/Documents/poli-204B/data/pets.RData", I can use here("data", "pets.RData"). Don't forget to manually create a "data" folder first!
# install.packages("here")
library(here)

# Save our data with the save() function
save(pets, file = here("data", "pets.RData"))

# Load it back in anytime with load()
load(file = here("data", "pets.RData"))
