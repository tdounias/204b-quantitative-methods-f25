###################################
###           R Lab 4           ###
###    Bivariate Regression     ###
###################################

# Our packages for the week
library(here)
# install.packages("broom")
library(broom)
library(tidyverse)

# Load in .RData files with load()
load(here("week-4", "data", "fish.RData"))

# Take a look at what we're working with
head(FishData)



## Research Question: 
# What is the relationship between Freedom House scores
# and economic growth among countries?



## Linear regression (OLS) with the lm() function

# lm() means "linear model"
# lm(y ~ x, data = data_name)
mod <- lm(growth ~ fhrev, data = FishData)
# growth = a + b * fhrev + e

# bam! science!



## Inspecting the results

# Old fashioned way using summary()
summary(mod)

# The {broom} way returns data frame objects. Very useful!

# broom::tidy() gives variable-level statistics
# (coefs, std.errors, test stats, p.values, interval bounds)
tidy(mod)   
tidy(mod, conf.int = TRUE)

# broom::glance() gives model-level statistics
glance(mod) 

# broom::augment() gives data-level statistics
augment(mod)


# Plot the results!

# Scatter plot of our two variables
plot(FishData$growth ~ FishData$fhrev, 
     xlab = "Freedom House Score", 
     ylab = "GDP Growth", 
     pch = 16,
     col = "steelblue")
# Plotting a fitted line
abline(tidy(mod)$estimate, lwd = 3, col = "black")

# Or in ggplot
ggplot(FishData) +
  aes(x = fhrev, y = growth) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", color = "black", se = F) +
  labs(x = "Freedom House Score", y = "GDP Growth") +
  theme_minimal()


## Discussion Question:
# How did we (or in this case R) know how to draw the fitted line?



## Let's go deeper into the results of our model

# Create a new data frame object with the data-level statistics
# from our linear regression model
aug_fish <- augment(mod)
head(aug_fish)

# Plotting the fitted values
# What are these things?
plot(x = aug_fish$fhrev,
     y = aug_fish$.fitted,
     xlab = "Freedom House Score",
     ylab = "Predicted GDP Growth",
     pch = 16,
     col = "steelblue")


# More useful: plotting residuals
plot(x = aug_fish$fhrev,
     y = aug_fish$.resid,
     xlab = "Freedom House Score",
     ylab = "Residual Value",
     pch = 16,
     col = "steelblue")
abline(h = 0, lwd = 3, lty = 3, col = "red")

# Same thing in ggplot
ggplot(data = aug_fish) +
  aes(x = fhrev, y = .resid) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, size = 1.2, col = "red", linetype = "dashed") +
  labs(x = "Freedom House Score", y = "Residual") +
  theme_minimal()


# Interpreting residuals
plot(aug_fish$fhrev, 
     aug_fish$growth, 
     xlab = "Freedom House Score", 
     ylab = "GDP Growth",
     col = "steelblue",
     pch = 16,
     cex = 0.5)
abline(tidy(mod)$estimate, lwd = 3, col = "black")
# The vertical distances from each point to the fitted line
segments(x0 = aug_fish$fhrev, y0 = aug_fish$growth,
         x1 = aug_fish$fhrev, y1 = aug_fish$.fitted,
         col = "violet")

# In terms from the lecture
Y <- aug_fish$growth           # Our dependent variable in the regression
X <- aug_fish$fhrev            # Our independent variable in the regression
Y_hat <- aug_fish$.fitted      # The "fitted" or predicted values of Y for each X observation
e <- aug_fish$.resid           # The residual value for each X observation

# The distance each X value is from its predicted Y value is its residual!
head(Y - Y_hat)
head(e)


# Residual Sum of Squares
# RSS: The variation in Y that our model does *not* explain
RSS <- sum((aug_fish$.resid)^2)
RSS <- sum(e^2)
RSS
# (This is the "sum of squared residuals" that we are trying to minimize when we select the line!)


# RegSS: The variation in Y that our model *does* explain
# Sometimes this is called ESS or "explained sum of squares"
# How much better is our model compared to one that predicted the mean value of Y for every X?
plot(aug_fish$fhrev, 
     aug_fish$growth, 
     xlab = "Freedom House Score", 
     ylab = "GDP Growth",
     col = "steelblue",
     pch = 16,
     cex = 0.5,
     main = "RegSS")
abline(tidy(mod)$estimate, lwd = 3, col = "black")
# Add line for the mean of GDP Growth (Y)
abline(h = mean(aug_fish$growth), lwd = 3, col = "limegreen", lty = 3)
# The vertical distances from the mean of GDP Growth to the fitted line
segments(x0 = aug_fish$fhrev, y0 = mean(aug_fish$growth),
         x1 = aug_fish$fhrev, y1 = aug_fish$.fitted,
         col = "violet")

# Calculating RegSS in terms from lecture
Y <- aug_fish$growth           # Our dependent variable in the regression
Y_hat <- aug_fish$.fitted      # The "fitted" or predicted values of Y for each X observation
RegSS <- sum((Y_hat - mean(Y))^2)
RegSS

# Total Sum of Squares
# TSS: The total variation in Y
plot(aug_fish$fhrev, 
     aug_fish$growth, 
     xlab = "Freedom House Score", 
     ylab = "GDP Growth",
     col = "steelblue",
     pch = 16,
     cex = 0.5,
     main = "TSS")
abline(h = mean(aug_fish$growth), lwd = 3, col = "limegreen", lty = 3)
segments(x0 = aug_fish$fhrev, y0 = mean(aug_fish$growth),
         x1 = aug_fish$fhrev, y1 = aug_fish$growth,
         col="violet")

# Calculating TSS
Y <- aug_fish$growth           # Our dependent variable in the regression
TSS <- sum((Y - mean(Y))^2)
TSS

# How does it all fit together??
# Discussion Question: what is the relationship between RegSS, RSS, and TSS?
RegSS + RSS == TSS


# What is the proportion of variation in Y that is explained by our model?
RegSS / TSS




## Some other ways to calculate regression lines ##

# Using the formulas that we saw in lecture:
Y <- FishData$growth
X <- FishData$fhrev

beta <- cor(X, Y) * sd(Y) / sd(X)
alpha <- mean(Y) - beta * mean(X)

# Note that these are the same numbers that we get if we use the lm() function
alpha 
beta
tidy(mod)


# Using Matrix Algebra:

# First a refresher on linear algebra and working with matrices
mat_a <- matrix(data = c(1, 2, 3, 4),
                nrow = 2, ncol = 2)
mat_b <- matrix(data = c(5, 6, 7, 8),
                nrow = 2, ncol = 2)
mat_a
mat_b

# We can add and subtract matrices that are the same dimensions
mat_a + mat_b
mat_a - mat_b

# Multiply a matrix by a scalar
mat_a * 3

# Multiply two matrices together using %*%
mat_a %*% mat_b

# Transpose a matrix with t()
mat_a
t(mat_a)

# Invert a matrix with solve()
mat_a
solve(mat_a)

# A matrix multiplied by its inverse gives us the identity matrix
mat_a %*% solve(mat_a)


# Putting it all together to solve the OLS equation

Y <- FishData$growth
X <- cbind(1, FishData$fhrev)

# beta = (X'X)^{-1}X'Y
beta <- solve(t(X) %*% X) %*% t(X) %*% Y
beta
tidy(mod)$estimate

# This is essentially what R is doing when you call the lm() function.

# The advantage of doing it this way is that it is easier to extend it to the multivariate case:
X <- as.matrix(cbind(1, FishData[, c("fhrev", "elf", "income", "britcol", "postcom", "opec", "muslim")]))

betas <- solve(t(X) %*% X) %*% t(X) %*% Y
betas



#####################################################################
#####################################################################

# Bonus section from the previous 204B TA
# Proceed with caution

### Using Brute Force!
y <- FishData$growth
x <- FishData$fhrev
Alphas <- seq(-5, 5, by=.01)
Betas <- seq(-5, 5, by=.01)

# The basic idea: 
# 1. Iterate through each combination of alpha and beta and 
#    record the sum of the squared residuals
# 2. Find the alpha/beta combination that minimizes that sum
results <- matrix(nrow=length(Alphas), ncol=length(Betas))
for(i in 1:length(Alphas)){
  for(j in 1:length(Betas)){
    a <- Alphas[i]
    b <- Betas[j]
    y.hat <- a + b*x
    residuals <- y - y.hat
    sum.squared.residuals <- sum(residuals^2)
    results[i,j] <- sum.squared.residuals
  }
}
rownames(results) <- Alphas
colnames(results) <- Betas
View(results)

min.location <- which(results == min(results), arr.ind = TRUE)

Alphas[min.location[1]]    # the optimal intercept
Betas[min.location[2]]     # the optimal slope
mod

#####################################################################
#####################################################################



## Displaying model output (graphics)

# Save the variable-level results in a table
tidy_mod <- mod %>% 
  tidy(conf.int = T) %>% 
  print()

# Plot using ggplot
ggplot(tidy_mod) +
  aes(x = term, y = estimate) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  coord_flip() +
  labs(x = "Variable", y = "Estimate") +
  theme_minimal()
  
# If the confidence intervals don't overlap 0, our results are ready for the APSR

# Sometimes you will see people use standard errors instead of confidence intervals
# SE * 2 is basically the same distance from the estimate as the CI
ggplot(tidy_mod) +
  aes(x = term, y = estimate) +
  geom_pointrange(
    aes(ymin = estimate - std.error, ymax = estimate + std.error),
    size = 1.5
    ) +
  geom_linerange(
    aes(ymin = (estimate - 2 * std.error), ymax = (estimate + 2 *std.error)),
    size = .75
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  coord_flip() +
  labs(x = "Variable", y = "Estimate") +
  theme_minimal()


## Displaying model output (tables)

# The texreg package is the best way to go
# Forget stargazer!

# install.packages("texreg")
library(texreg)

# To check out the results in the Console use texreg::screenreg
screenreg(mod)

# To generate Latex output use texreg::texreg
# If you are generating a pdf output in RMarkdown this is the way to go
texreg(mod)

# To generate HTML output use texreg::htmlreg
# If you are generating a HTML output in RMarkdown this is the way to go
htmlreg(mod)



# Dope!
plotreg(mod)


# Adding another model side by side is easy!
mod2 <- lm(growth ~ fhrev + opec, data = FishData)

# Put your models into a list
screenreg(list(mod, mod2))

plotreg(list(mod, mod2))
