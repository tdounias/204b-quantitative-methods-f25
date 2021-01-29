#################################
##           R Lab 3           ##
##   Simple Hypothesis Tests   ##
#################################


# Note: this lab uses the Assassinations dataset that you used in the first homework. 

# The best way to load this data into R is using the read_fwf function from the readr package
library(readr)
# Try to get used to using the here package as well! It will make your life easier in the long run
library(here)

assassins <- read_fwf(
  here("week-3", "data", "05208-0001-Data.txt"),
  fwf_cols(
    country = c(3, 5),
    year = c(10, 11),
    outcome = 12,
    minority = 14,
    number_killed = 26
  )
)

#################################
## Proportions and Means Tests ##

# We will use the prop.test() function for proportions tests, and the t.test function for means tests
# We can look up how to specify these functions by opening the help menu
?prop.test
?t.test


## Single Sample Proportion Test ##

# Is the success rate of assassination attempts different from 0.5?
# H0: p = 0.5
# HA: p != 0.5
success_count <- sum(assassins$outcome == 1)
n <- nrow(assassins)
prop.test(success_count, n, p = 0.5)

# We can save a test as an object in R
# This can be useful for extracting specific portions of the output
outcome_test <- prop.test(success_count, n, p = 0.5)
outcome_test$conf.int
outcome_test$p.value   # yes!!!!!! *****


# Visualizing what this means:
p <- mean(assassins$outcome == 1)
SE <- sqrt(p * (1 - p) / n)
curve(dnorm(x, mean = p, sd = SE), xlim = c(.2, .6), ylim = c(0, 30), ylab = "Density")
abline(h = 0, lty = 3, lwd = 2)
x1 <- seq(p - 1.96 * SE, p + 1.96 * SE, by = 0.001)
polygon(c(x1, rev(x1)), c(dnorm(x1, mean = p, sd = SE), rep(0, length(x1))), col = "steelblue")
abline(v = 0.5, lwd = 5, col = "violetred4")




### Differences-in-Proportions Tests ###

# Is the success rate for assassinations involving "minority tensions"
# different from the success rate for other assassinations?
# H0: The proportions are the same for both types of assassination
# HA: The proportions are different

# First, we have to do some work to arrange our data into the proper format 
successful <- assassins$outcome == 1                    # Which attempts were successful?
minority <- assassins$minority == 1                     # Which attempts involved minority tensions?
minority_success <- sum(successful[minority])           # Number of successful attempts involving minority tensions
nonminority_success <- sum(successful[!minority])       # Number of successful attempts NOT involving minority tensions
minority_total <- sum(minority)                         # Total number of attempts involving minority tensions
nonminority_total <- sum(!minority)                     # Total number of attempts NOT involving minority tensions

# Now we can specify the prop.test function by passing it two vectors:
#  The first contains the success COUNTS for each of our two groups
#  The second contains the total number of attempts (n) for that group
prop.test(x = c(minority_success, nonminority_success),
          n = c(minority_total, nonminority_total))


# Let's do the same thing for the 1940s vs. 1960s test:
# H0: The proportions are the same for both periods
# HA: The proportions are different
successful <- assassins$outcome == 1
forties <- assassins$year < 50
sixties <- assassins$year >= 60
forties_success <- sum(successful[forties])
sixties_success <- sum(successful[sixties])
forties_total <- sum(forties)
sixties_total <- sum(sixties)

prop.test(x = c(forties_success, sixties_success),
          n = c(forties_total, sixties_total))



### Single Sample Means Test ###

# Is the average number of people killed different from 1?
t.test(assassins$number_killed, mu = 1)


### Differences-in-Means Tests ###

# Is the number of people killed in assassinations involving "minority tensions"
# different from the number killed in other assassinations?
# H0: The means are the same for both types of assassinations
# HA: The means are different

t.test(assassins$number_killed ~ assassins$minority)

# Alternatively, we could give R the *NUMBER* of people
# killed in each attempt for each group, and R will do the rest of the work for us
minority <- assassins$minority == 1
minority_killed <- assassins$number_killed[minority]
nonminority_killed <- assassins$number_killed[!minority]
t.test(x = minority_killed, y = nonminority_killed)



forties <- assassins$year < 50
sixties <- assassins$year >= 60
forties_killed <- assassins$number_killed[forties]
sixties_killed <- assassins$number_killed[sixties]

# H0: The means are the same for both periods
# HA: The means are different
t.test(x = forties_killed, y = sixties_killed)

# If we wanted to do a one-sided test instead, we could specify that as an option
# H0: The average number of people killed in each 1940s attempt is NOT greater than the average killed in each 1960s attempt
#     X_1940s <= X_1960s
# HA: The average number of people killed in each 1940s attempt IS greater than the average killed in each 1960s attempt
#     X_1940s > X_1960s
t.test(x = forties_killed, y = sixties_killed, alternative = "greater")





# This is the bonus section -----------------------------------------------



### BONUS: Graphing Confidence Intervals

# Let's plot the confidence intervals for the sample means and difference in means

# First, we will re-run the t-test models and save the result
forties_mod <- t.test(forties_killed)                  # Single sample means test, 1940s
sixties_mod <- t.test(sixties_killed)                  # Single sample means test, 1960s
dif_mod <- t.test(x = forties_killed, y = sixties_killed)  # Difference in means test

# Now we will extract the estimates and confidence intervals
estimates <- c(forties_mod$estimate, sixties_mod$estimate, forties_mod$estimate - sixties_mod$estimate)
ci <- rbind(forties_mod$conf.int, sixties_mod$conf.int, dif_mod$conf.int)

# Finally, we will plot them on the same graph
plot(x = 1:3, y = estimates, ylim = c(-.1, 1), xlim = c(.5, 3.5), xaxt = "n", pch = 16, cex = 1.2, xlab = "", ylab = "Average Number of People Killed")
segments(1:3, ci[,1], 1:3, ci[,2], lwd = 3)
axis(1, 1:3, labels = c("Mean\n1940s", "Mean\n1960s", "Difference\nin Means"), tick = F)
abline(h = 0, lty = 4)

# You will sometimes see this depicted as a barplot instead
bar <- barplot(estimates[1:2], ylim = c(0, 1), space = .5, names_arg = c("1940s", "1960s"), ylab = "Number Killed") # Make a barplot of only the means for each group
segments(bar[1], estimates[2] + dif_mod$conf.int[1], bar[1], estimates[2] + dif_mod$conf.int[2], lwd = 5)       # Add the confidence interval for the *difference in means* to one of the bars

