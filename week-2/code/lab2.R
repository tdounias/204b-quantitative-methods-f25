###############################
##           Lab 2           ##
##        Probability,       ##
## Simulations and Functions ##
###############################


############################
## Distribution Functions ##

# R provides four functions for each common probability distribution
# These begin with a prefix followed by an abbreviation for the distribution
# (e.g. dnorm, pnorm, qnorm, rnorm for the Normal distribution)
  # d* The density function. The height of the curve at a particular point
  # p* The cumulative density function. The probability below some threshold
  # q* The quantile function. This is the inverse of the CDF. 
     # "Useful" for defining intervals containing a given probability density (e.g. 95%)
  # r* Random sampling. Draw random samples from a particular distribution


## Density ##
# Density function for the Normal distribution: dnorm
curve(dnorm, lwd = 2, xlim = c(-4,  4), ylab = "Density", main = "Normal PDF")

# The height of the distribution at a particular value
dnorm(x = 0)

# Let's see what happens when we change the mean
x <- seq(-4, 4, by = 0.01)
plot(x = x, y = dnorm(x, mean = 1), type = "l", lwd = 2, ylab = "Density", main = "Normal PDF, mu = 2")
# Do you expect x = 0 to be greater, smaller, or the same as before?
dnorm(x = 0, mean = 1)

# What about when we change the standard deviation?
plot(x = x, y = dnorm(x, sd = 2), type = "l", lwd = 2, ylab = "Density", main = "Normal PDF, sigma = 2")
dnorm(x = 0, sd = 2)
# Why is x = 0 smaller when we increase the standard deviation but keep the mean constant?

# We can do this with other distributions too!
# Density function for the Binomial distribution: dbinom
x <- seq(0, 5)
plot(x = x, y = dbinom(x, size = 5, prob = 0.7), type = "h", lwd = 4, ylab = "Density", main = "Binomial PMF \n size = 5, prob = 0.7")
# The height of the bar at x = 2
dbinom(2, 5, 0.7)

# If we change the probability parameter to 0.9, how will this change the height of x = 2?
dbinom(2, 5, 0.9)

### Exercise 1:
# What is the probability of getting exactly one head if we flip a fair coin 7 times?
dbinom(1, 7, 0.5)


# Each commonly-used distribution has its own version of these functions
dpois(3, lambda = 2)  # The probability that a draw from X ~ Poisson(2) is exactly 3

x <- seq(0, 20)
plot(x, dpois(x, lambda = 5), type = "h", lwd = 2, ylab = "Density", main = "Poisson PMF \n lambda = 5")

# Beta Distribution (useful for Bayesian statistics)
curve(dbeta(x, shape1=.1, shape2=.1), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(.1, .1)")
curve(dbeta(x, shape1=1, shape2=1), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(1, 1)")
curve(dbeta(x, shape1=10, shape2=10), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(10, 10)")
curve(dbeta(x, shape1=1000000, shape2=1000000), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(1,000,000, 1,000,000)")
curve(dbeta(x, shape1=2, shape2=20), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(2, 20)")
curve(dbeta(x, shape1=1, shape2=2), lwd=2, xlim=c(0,  1), xlab="p", main="Beta(1, 2)")



## Cumulative Density ##
# Cumulative density function for the Normal distribution: pnorm
curve(pnorm, lwd = 2, xlim = c(-3,  3), ylab = "Probability", xlab = "X", main = "Normal CDF")

# The CDF tells you the probability below (to the left of) some threshold on the PDF
pnorm(0)
# The probability below the -1.96 threshold
pnorm(-1.96)
# The probability between -1.96 and +1.96
1 - 2 * pnorm(-1.96)

# We can also use the mean and sd arguments to get other normal curves
pnorm(2, mean = 3, sd = 1)          # Probability that x <= 2 if X ~ N(3, 1)
1 - pnorm(2, 3, 1)                  # Probability that x > 2 if X ~ N(3, 1)
pnorm(4, 4, 2) - pnorm(2, 4, 2)     # Probability that 2 < x <= 4 if X ~ N(4, 2^2)

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
x1 <- seq(2, 4, by = .01)
polygon(c(x1, rev(x1)), c(dnorm(x1, 4, 2), rep(0, length(x1))), col="blue")
pnorm(4, 4, 2) - pnorm(2, 4, 2)


### Exercise 2:
# Assuming a standard normal distribution (X ~ N(0, 1)), calculate the 
# probability that 0 < X <= 1
a <- pnorm(1) - pnorm(0)


### Exercise 3: Connecting pbimon to dbinom
# How would you use pbinom to calculate the probability of 2 heads in 3 flips, dbinom(2, 3, 0.5)
dbinom(2, 3, 0.5)



# Play around with distributions at: https://ben18785.shinyapps.io/distribution-zoo/
# Also includes the R code for pdf/cdfs. And Latex equations!



## Quantile Functions ##
plot(qnorm, ylab = "X", xlab = "Probability")

# What is the value for the 75th percentile of N(0, 1)
qnorm(0.75)
# 25th percentile? What do you notice?
qnorm(0.25)

# What is the 95% interval around the mean of N(20, 3^2)
qnorm(0.05, mean = 20, sd = 3)
qnorm(p = .95, 20, 3)


## Sampling from Distributions ##
# PDFs and CDFs are mathematical constructions, but they are NOT data!
# They describe the process by which data could be generated
# Often the best way to understand probability distributions is to general random samples or draws

rnorm(1)                         # 1 draw from the distribution X ~ N(0, 1)
rnorm(50, mean = 20, sd = 2)     # 50 draws from the distribution X ~ N(20, 2^2)

# Lists of numbers are hard to look at! Let's graph instead
hist(rnorm(10), col = "steelblue", xlab = "X")
hist(rnorm(100), col = "steelblue", xlab = "X")
hist(rnorm(1e7), col = "steelblue", xlab = "X", br = 50)

# Binomial time
rbinom(100, 1, 0.5)      # 100 flips of 1 coin
rbinom(100, size = 5, prob = 0.5)      # 100 flips of 5 coins. Values are counts of "success"

hist(rbinom(10, 1, 0.5), col = "steelblue", xlab = "X")
hist(rbinom(10000, 1, 0.5), col = "steelblue", xlab = "X")
hist(rbinom(10, 5, 0.7), col = "steelblue", xlab = "X")
hist(rbinom(10000, 5, 0.7), col = "steelblue", xlab = "X", br = 20)

# Poisson poisson poisson
rpois(1000, lambda = 4)          # 1000 draws from the poisson distribution with lambda=4

hist(rpois(10000, lambda = 4), col = "steelblue", xlab = "X") 

# Uniform is boring!
# But sometimes useful :)
runif(1000, min = -1, max = 1)   # 1000 draws from uniform distribution from -1 to +1

hist(runif(10, min = -1, 1), col = "steelblue", xlab = "X", xlim = c(-3, 3))
hist(runif(1000, min = -1, 1), col = "steelblue", xlab = "X", xlim = c(-3, 3))



## Z Scores ##
# Let's make an easy vector to work with
x <- c(0, 1, 3, 5, 6)

# Now we can "standardize" it by transforming into z-scores
x_z <- (x - mean(x)) / sd(x)
x_z

hist(x, br = 3)
hist(x_z, br = 3)

# It can turn any distribution into a Normal(0, 1)?
x_draws <- runif(10000, 2, 6)
z <- (x_draws - mean(x_draws)) / sd(x_draws)

mean(x_draws)
mean(z)
var(x_draws)
var(z)

par(mfrow = c(2, 1))
hist(x_draws, br = 20, col = "seagreen", xlim = c(-3, 7))
hist(z, br = 20, col = "gold", xlim = c(-3, 7))

# What is the point of all this?
# Frequently used in regression contexts. We will discuss this later
# Z-tests

# Starting 5 for the UCSD Tritons
nba_heights <- c(190, 170, 206, 162, 180)

# Average height and standard deviation of the UCSD student body
avg_height <- 171
sd_height <- 10

# Creating a z statistic
se <- sd_height/sqrt(length(nba_heights))

z <- (mean(nba_heights) - avg_height) / se
z

# The p-value!
2 * (1 - pnorm(z))

curve(dnorm, lwd = 2, xlim = c(-6,  6), ylab = "Density", main = "Normal PDF")
abline(v = z, lwd = 2, col = "red")


# Why doesn't this work?
2 * (1 - pnorm(mean(nba_heights), mean = avg_height, sd = sd_height))


## ---------------------------------------------------------------------- ##
##                                  Break                                 ##
## ---------------------------------------------------------------------- ##




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
num_vec[8] <- 10 
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
vec_3 <- c("a", "b", "c")

# rbind "row bind" vectors by stacking them on top of each other to form a matrix
rbind(vec_1, vec_2, vec_3)

# cbind "column bind" vectors by placing them side by side to form a matrix
cbind(vec_1, vec_2, vec_3)

# rbind and cbind are VERY useful for adding additional columns or rows 
# These functions work with data frames too!

# Accessing elements of a matrix
matr <- cbind(vec_1, vec_2, vec_3)

mat[1, 1]  # First row, first column (upper left cell)
mat[, 1]   # First column
mat[1, ]   # First row

mat[, "vec_1"]

# We will work more with matrices when we get to regression :)

# Other basic object types in R are Arrays and Lists
# Arrays are higher dimension vectors. Rare in our cases!
# Lists are like vectors whose elements can be any other type of object
#     sometimes useful to know about



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
  
  return(list(temp, "other option"))
}

convert_temp(75, scale = "celsius")
convert_temp(75) # Same thing as above because scale = "celsius" when we defined the function originally
convert_temp(75, scale = "kelvin")
convet_temp(75, scale = "fahrenheit") # error!

# Z Score function
convert_z <- function(x) {
  z <- (x - mean(x)) / sd(x)
  
  return(z)
}


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
# We use "i" commonly because it is the notation for indexes in math, but this variable can be anything!
# 1:10 creates a sequence of integers from 1 to 10 (inclusive)
# This defines the values over which we will iterate when running the loop
for (i in 1:10) {    
  print(5)           # The guts of the loop go inside the curly braces
                     # print() functions can be helpful to see what's happening in our console
}

# Look at i changing! This is super useful
for (i in 1:10) {
  x <- i * 2
  print(x)
}

# Loops can iterate over more than just sequences of integers
pets <- c("dog", "cat", "monkey", "parrot")
for (pet in pets) {
  print(pet)
}

# We can next loops inside each other (!!!)
# Be careful -- permutations get big fast
for (i in 1:3) {
  for (k in 1:3) {
    print(k)
  }
}

# The previous loops just did their thing but didn't give us anything useful after running
# Very often we want to set up a "container" object to store the results from each loop
# This is where indexes become super useful
container <- c()          # empty vector
for (i in 1:100) {
  container[i] <- i + 5
}
container

# This also works, but is less efficient because R needs to rewrite the entire vector every iteration!
# Sometimes this method can be useful though
container <- rep(NA, 100)
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



###################
### Simulations ###

# Simulations help us understand how our statistical procedures work!
# They also help us solve very hard/impossible math problems

# A section of POLI 30 (our undergraduate introductory methods course) starts with 27 students.
# The probability that a given student fails POLI 30 is 0.08, and outcomes for two students are independent.
# What is the probability that everyone in this section will pass the class?
passed <- rbinom(100000, size = 27, prob = 0.92)  # Takes 100,000 draws from the distribution Binomial(n=27, p=0.92)
mean(passed == 27)                                # Calculates the proportion of these draws that equal 27 (everyone passed)

# What is the probability that at least 4 students will fail the class?
mean(passed <= 27 - 4)                       # Calculates the proportion of these draws that are less than or equal to 23


# You roll 2 die, and I roll one dice. What is the probability that the sum of the 2 numbers that you roll
# is larger than the one number that I roll?
N <- 1000000
d1 <- sample(1:6, N, replace = T)
d2 <- sample(1:6, N, replace = T)
d3 <- sample(1:6, N, replace = T)

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
for (i in 1:N) {
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
hist(rgeom(100, prob = 0.3))

# We want to know something about how the *mean* of these draws is distributed
mean(rgeom(100, prob = 0.3))

# Now let's do this 100,000 more times and save the results
N <- 100000
means_outcomes <- rep(NA, N)
number_draws <- 100     
# Note: N is not the same thing as the number of draws!
# N is the number of iterations in the loop; 
# number.draws is the number of draws from the geometric distribution 
#              that we take in each iteration
for (i in 1:N) {
  draws <- rgeom(number_draws, prob = 0.3)
  means_outcomes[i] <- mean(draws)
}
sd(means_outcomes)

# What do you notice about the shape of the distribution of the mean?
hist(means_outcomes, freq = F)
curve(dnorm(x, mean(means_outcomes), sd(means_outcomes)), add = T, lwd = 3)


# The replicate() function is useful if we just want to run the same function over and over N times
pets <- c("dog", "cat", "monkey", "parrot")
generate_random_pet <- function() {  # Note: functions do not necessarily need any arguments!
  rand_pet <- sample(pets, 1)
}
replicate(10, generate_random_pet())


# Pancake problem!!!
# What is the probability that the other side of a pancake is burnt
# given that you are served a burnt side face up
# Simulate a pancake and return randomly ordered sides
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


