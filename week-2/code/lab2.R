###############################
##           Lab 2           ##
## Simulations and Functions ##
###############################



## Recap: working with objects ##
# We use '<-' in R to assign values to objects
numeric_object <- 55
character_object <- "monkey"

# Then when we do things with these objects
numeric_object
numeric_object * 10



#############
## Vectors ##

# Vectors are objects with multiple elements of the same type
num_vec <- c(1, 2, 6, 10, 8, 3, 11)

# The order of the elements is important!
# This is very useful because it allows us to access elements by index
num_vec[1]        # First element
num_vec[-1]       # Everything except the first element
num_vec[c(2, 4)]  # Second and fourth elements

# Access elements using conditionals
num_vec > 5                 # Conditional statements generate a vector of TRUE/FALSEs
num_vec[num_vec > 5]        # We can use this vector to subset the original vector
num_vec[num_vec %% 2 == 0]  # Only keep values, when divided by 2, equal 0 (even numbers)

# Changing elements of a vector
num_vec[1] <- 7
num_vec

# Adding new elements to a vector
longer_vec <- c(num_vec, 9)
longer_vec



##############
## Matrices ##

# Rectangular collection of vectors. Simplified data frames
matrix(data = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
       nrow = 3,
       ncol = 3)

# Matrices can also be created from vectors using cbind() or rbind()
vec_1 <- c(1, 2, 3)
vec_2 <- c(4, 5, 6)
vec_3 <- c(7, 8, 9)

# rbind "row bind" vectors by stacking them on top of each other
rbind(vec_1, vec_2, vec_3)

# cbind "column bind" vectors by placing them side by side
cbind(vec_1, vec_2, vec_3)

# rbind and cbind are VERY useful for adding additional columns or rows 
# These functions work with data frames too!

# Accessing elements of a matrix
mat <- cbind(vec_1, vec_2, vec_3)

mat[1, 1]  # First row, first column (upper left cell)
mat[, 1]   # First column
mat[1, ]   # First row

# We will work more with matrices when we get to regression :)

# Other basic object types in R are Arrays and Lists
# Arrays are higher dimension vectors. Rare in our cases!
# Lists are like vectors whose elements can be any other type of object
#     sometimes useful to know about



###########################
## Distribution Functions ##
# Density function for the Normal distribution: dnorm
curve(dnorm, lwd = 2, xlim = c(-3,  3), ylab = "Density", xlab = "X", main = "Normal PDF")

# Cumulative density function for the Normal distribution: pnorm
curve(pnorm, lwd = 2, xlim = c(-3,  3), ylab = "Density", xlab = "X", main = "Normal CDF")

# Recall: the CDF tells you the probability below (to the left of) some threshold
pnorm(0)
# The probability below the -1.96 threshold
pnorm(-1.96)
# The probability between -1.96 and +1.96
1 - 2*pnorm(-1.96)

# We can also use the mean and sd arguments to get other normal curves
pnorm(2, mean = 3, sd = 1)          # Probability that x < 2 if X ~ N(3, 1)
1 - pnorm(2, 3, 1)                  # Probability that x > 2 if X ~ N(3, 1)
pnorm(4, 4, 2) - pnorm(2, 4, 2)     # Probability that 2 < x < 4 if X ~ N(4, 2^2)

## Why does that work?

# pnorm(2, 4, 2) gives us this:
x <- seq(-4, 12, by = .01)
plot(x, dnorm(x, 4, 2), type="l", lwd=2, xlim=c(-2, 10), main="P(x < 2)", ylab="Density")
x1 <- seq(-8, 2, by = .01)
polygon(c(x1, rev(x1)), c(dnorm(x1, 4, 2), rep(0, length(x1))), col="forestgreen")
pnorm(2, 4, 2)

# pnorm(4, 4, 2) gives us this:
x <- seq(-4, 12, by=.01)
plot(x, dnorm(x, 4, 2), type="l", lwd=4, xlim=c(-2, 10), main="P(x < 4)", ylab="Density")
x1 <- seq(-8, 4, by=.01)
x2 <- seq(-8, 2, by=.01)
polygon(c(x1, rev(x1)), c(dnorm(x1, 4, 2), rep(0, length(x1))), col="blue")
polygon(c(x2, rev(x2)), c(dnorm(x2, 4, 2), rep(0, length(x2))), col="forestgreen")
pnorm(4, 4, 2)

# pnorm(4, 4, 2) - pnorm(2, 4, 2) gives us the area between 2 and 4
plot(x, dnorm(x, 4, 2), type="l", lwd=4, xlim=c(-2, 10), main="P(2 < x < 4)", ylab="Density")
x1 <- seq(2, 4, by=.01)
polygon(c(x1, rev(x1)), c(dnorm(x1, 4, 2), rep(0, length(x1))), col="blue")
pnorm(4, 4, 2) - pnorm(2, 4, 2)


### Exercise 1:
# Assuming a standard normal distribution (X ~ N(0, 1)), calculate the 
# probability that 0 < X < 1



# Each commonly-used distribution has its own version of these functions
dpois(3, lambda = 2)  # The probability that a draw from X ~ Poisson(2) is exactly 3

x <- seq(0, 20)
plot(x, dpois(x, lambda = 5), lwd = 2, ylab = "Density", main = "Poisson PMF lambda = 5")

# Beta Distribution (useful for Bayesian statistics)
curve(dbeta(x, shape1=.1, shape2=.1), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(.1, .1)")
curve(dbeta(x, shape1=1, shape2=1), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(1, 1)")
curve(dbeta(x, shape1=10, shape2=10), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(10, 10)")
curve(dbeta(x, shape1=1000000, shape2=1000000), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(1,000,000, 1,000,000)")
curve(dbeta(x, shape1=2, shape2=20), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(2, 20)")
curve(dbeta(x, shape1=1, shape2=2), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(1, 2)")



######################
## Custom Functions ##

# You can write your own functions in R using the "function" command
# Functions can be really complicated or really simple!

# here's the general idea:

# my_function <- function(x, y, z) {            # tells R that this is a function and defines the possible arguments
#   	             out <- crazy function stuff	# the meat of the function
#                  return(out)                  # returns the output of the function
# }                                             # closes the function up

# Here is an example that converts Fahrenheit temperatures to Celsius
convert_to_celsius <- function(f_temp) {
  c_temp <- (f_temp - 32) * 5/9
  return(c_temp)
}
# Once we run the function above, it is saved into our global environment
# Now we can begin using it
convert_to_celsius(32)

# A more complicated function
# Now we can choose between Celsius or Kelvin outputs, but Celsius is the default
convert_temp <- function(f_temp, scale = "celsius") {
  
  if (scale == "celsius") {
    temp <- (f_temp - 32) * 5/9
  } else if (scale == "kelvin") {
    temp <- (f_temp - 32) * 5/9 + 273.15
  }
  
  return(temp)
}

convert_temp(75, scale = "celsius")
convert_temp(75) # Same thing as above because scale = "celsius" when we defined the function originally
convert_temp(75, scale = "kelvin")
convet_temp(75, scale = "fahrenheit") # error!

# Tips for functions!
   # Before starting, think clearly about what you want the output of the function to look like
      # then you can think about the inputs
   # Make the name evocative. Verb phrases are often a good idea
   # Avoid vague argument names (e.g. "f_temp" is better than "x" above)
   # Don't forget the return() statement!
   # After loading your function into your environment, test it out in the console to make sure it works



###############
## For Loops ##

# Loops are one way to repeat a process many times in R
# This is useful if we want to evaluate a function over and over with different values for arguments
# Or if we want to take advantage of randomness for our simulations

# The parts of a for loop:
# i is an arbitrary variable name that gets defined when we begin our loop
# We use "i" commonly because it is the notation for indices in math, but this variable can be anything!
# 1:10 creates a sequence of integers from 1 to 10 (inclusive)
# This defines the values over which we will iterate when running the loop
for (i in 1:10) {    
  print(5)           # The guts of the loop go inside the curly braces
                     # print() functions can be helpful to see what's happening in our console
}

# Look at i changing! This is super useful
for (i in 1:10) {
  print(i)
}

# Loops can iterate over more than just sequences of integers
pets <- c("dog", "cat", "monkey", "parrot")
for (pet in pets) {
  print(pet)
}

# We can next loops inside each other (!!!)
# Be careful -- combinations get big fast
for (i in 1:3) {
  for (k in 1:5) {
    print(k)
  }
}

# The previous loops just did their thing but didn't give us anything useful after running
# Very often we want to set up a "container" vector to store the results from each loop
# This is where indices become super useful
container <- c() # empty vector
for (i in 1:100) {
  container[i] <- i + 5
}
container

# This also works, but is less efficient because R needs to rewrite the entire vector every iteration!
# Sometimes it can be useful
container <- c()
for (i in 1:100) {
  container <- c(container, i + 5)
}
container



############################################################
## Building blocks of simulations: sequences and sampling ##

## Sequences ##
# We saw some basic sequences before with loops
N <- 10
1:N

# This is nice for loops that we want to run N times
# But often we want more control over our sequences
seq(from = 1, to = 10, by = 1)
seq(1, 10, by = 0.1)

# Sometimes instead of an ordered sequence, we just want to repeat some value N times
rep(5, 10)
rep(c(1,2), 10)

# It is generally better to set up your empty loop "containers" with rep(NA, N) rather than c()
# Where "N" is the number of iterations in your loop
# This makes your code more explicit and clear

## Sampling ##
# The sample() function is extremely useful for understanding probability and statistics
# x refers to the set from which we are sampling. This will typically be a sequence or some other vector
# size is how many elements from x you want the function to return
sample(x = 1:10, size = 5)

# replace = FALSE by default, but we can change that to sample WITH replacement
sample(x = 1:10, size = 5, replace = TRUE)

# sampling from a vector
pets <- c("monkey", "cat", "dog", "gerbil", "rabbit", "fish")
best_pet <- sample(pets, size = 1)
# R can reveal truths about the world...
print(paste0("The best pet is ", best_pet))

## Sampling from Distributions ##
# The sample() function generates random draws from vectors/distributions that we define ourselves
# What if we want to sample from known distributions, such as the Normal or Poisson?
# R has pdf/pmf sampling functions that mirror the syntax of the distribution functions we used earlier (dnorm and pnorm)

rnorm(n = 1)                  # 1 draw from the distribution X ~ N(0, 1)
rnorm(50, mean = 20, sd = 2)  # 50 draws from the distribution X ~ N(20, 2^2)
rpois(1000, lambda=3)         # 1000 draws from the poisson distribution with lambda=3
rbinom(1, 1, 0.5)             # 1 draw from a binomial distribution with p=0.5 and 1 event (Bernoulli distribution)
rbinom(100, 10, 0.75)         # 100 draws from binomial distribution with p=0.75 and 10 events

# Visualizing these samples with histograms is an alternative to curve() with density functions
hist(rnorm(10000))
hist(rpois(10000, lambda=3))   
hist(rbinom(10000, 5, 0.75))


###################
### Simulations ###

# A section of POLI 30 (our undergraduate introductory methods course) starts with 27 students.
# The probability that a given student fails POLI 30 is 0.08, and outcomes for two students are independent.
# What is the probability that everyone in this section will pass the class?
passed <- rbinom(100000, size = 27, prob = 0.92)  # Takes 100,000 draws from the distribution Binomial(n=27, p=0.92)
mean(passed == 27)                                # Calculates the proportion of these draws that equal 27 (everyone passed)

# What is the probability that at least 4 students will fail the class?
mean(passed <= 27 - 4)                       # Calculates the proportion of these draws that are less than or equal to 23


# You roll 2 die, and I roll one dice. What is the probability that the sum of the 2 numbers that you roll
# is larger than the one number that I roll?
n <- 1000000
d1 <- sample(1:6, n, replace = T)
d2 <- sample(1:6, n, replace = T)
d3 <- sample(1:6, n, replace = T)

d1[1] + d2[1] > d3[1]
d1[39] + d2[39] > d3[39]
d1[1134] + d2[1134] > d3[1134]

# Probability that the sum of the 2 numbers that you roll is larger than the one number that I roll?
mean(d1 + d2 > d3)

# What is the expected value of the difference between the sum of your two rolls and my one roll?
mean(d1 + d2 - d3)

# And what does the distribution look like?
hist(d1 + d2 - d3)


# When we are doing something complicated (e.g., sampling *without* replacement),
# it is often easier (and more clear to anyone reading your code) to do this within a loop

# You decide to flip a fair coin until you get a heads
# What is the probability that you will have to flip the coin more than three times?
N <- 100000
results <- rep(NA, N)
for(i in 1:N){
  flips <- rbinom(100, 1, .5)
  first_heads <- min(which(flips == 1))
  results[i] <- first_heads
}
mean(results > 3)
hist(results)

# What is the standard deviation of the distribution of the mean of 100 draws from a 
# geometric distribution with probability p=0.3?
# Note that this is asking for the standard deviation of a distribution of means!

# First, let's see what 100 draws from the geometric distribution looks like
hist(rgeom(100, prob=0.3))

# We want to know something about how the *mean* of these draws is distributed
mean(rgeom(100, prob=0.3))

# Now let's do this 100,000 more times and save the results
N <- 100000
means_outcomes <- rep(NA, N)
number_draws <- 100     
# Note: N is not the same thing as the number of draws!
# N is the number of iterations in the loop; 
# number.draws is the number of draws from the geometric distribution 
#              that we take in each iteration
for(i in 1:N) {
  draws <- rgeom(number_draws, prob=0.3)
  means_outcomes[i] <- mean(draws)
}
sd(means_outcomes)

# What do you notice about the shape of the distribution of the mean?
hist(means_outcomes, freq = F)
curve(dnorm(x, mean(means_outcomes), sd(means_outcomes)), add = T, lwd = 3)


# The replicate() function is useful if we just want to run the same function over and over N times
pets <- c("dog", "cat", "monkey", "parrot")
generate_random_pet <- function() {  # Note: functions do not need any arguments!
  rand_pet <- sample(pets, 1)
}
replicate(10, generate_random_pet())


# Pancake problem!!!
# simulate a pancake and return randomly ordered sides
sim_pancake <- function() {
  pancake <- sample(1:3, 1)
  sides <- matrix(c(1, 1, 1, 0, 0, 0), nrow = 2, ncol = 3)[, pancake] 
  sample(sides)
}
# simulate 10,000 pancakes
pancakes <- replicate(100000, sim_pancake()) 
up <- pancakes[1, ]
down <- pancakes[2, ]
# compute proportion 1/1 (BB) out of all 1/1 and 1/0 
num_11_10 <- sum(up == 1)
num_11 <- sum(up == 1 & down == 1)
num_11 / num_11_10



######################################
### BONUS: Plotting Shaded Regions ###

## Here are some ways to plot shaded regions on histograms and density plots

outcomes <- rnorm(100000, 4, 2)

our.hist <- hist(outcomes)                                # First, save the histogram as an object
color <- ifelse(our.hist$breaks < 2, "blue", "gray70")    # Then extract the break points that R uses for the bins and use them to define the colors
hist(outcomes, col=color)                                 # Finally, make the histogram again and use those colors as an argument in the function

our.hist <- hist(outcomes)
color <- ifelse(our.hist$breaks >= 2 & our.hist$breaks < 4, "blue", "gray70")
hist(outcomes, col=color)

curve(dnorm(x), type="l", lwd=4, xlim=c(-3, 3), main="One-Sided Test")
abline(h=0, lty=3)
x1 <- seq(1.644854, 4, by=.01)
polygon(c(x1, rev(x1)), c(dnorm(x1), rep(0, length(x1))), col="blue")

curve(dnorm(x), type="l", lwd=4, xlim=c(-3, 3), main="Two-Sided Test")
abline(h=0, lty=3)
x1 <- seq(-4, -1.96, by=.01)
polygon(c(x1, rev(x1)), c(dnorm(x1), rep(0, length(x1))), col="blue")
x2 <- seq(1.96, 4, by=.01)
polygon(c(x2, rev(x2)), c(dnorm(x2), rep(0, length(x2))), col="blue")