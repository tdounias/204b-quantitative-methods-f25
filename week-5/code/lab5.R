###################################
###           R Lab 5           ###
###   Multivariate Regression   ###
###################################


# Start by loading in our favorite packages
library(here)
library(tidyverse)
library(broom)


# We are going to use the same Fish data set as last week.
load(here("week-4", "data", "fish.RData"))
head(FishData)


### First, let's talk about dummy variables ###
### (This came up on some of your HW4s)     ###

# Question: Are Muslim-majority countries more likely to
# belong to OPEC compared to other countries?

table(FishData$opec)
table(FishData$muslim)
plot(FishData$muslim, FishData$opec) # AHHHHHHH

# Nevertheless, it's OK to do this:
mod <- lm(opec ~ muslim, data = FishData)
summary(mod)

# In this case, the slope 0.199868 means that
# Muslim-majority countries were .2 more likely
# (20 percentage points more likely) to belong 
# to OPEC, compared to other countries


# How would we interpret the slope in this mode?
# What about the intercept?
mod2 <- lm(growth ~ postcom, data = FishData)
summary(mod2)



### Multivariate Regressions ###

# In order to run a regression on multiple IVs at once, we can use
# the same function as before, and we just separate the IVs with a "+"
# fhrev = b0 + b1 income + b2 elf + e
mod <- lm(fhrev ~ income + elf, data = FishData)
summary(mod)

# Under the hood, R is basically doing this:
Y <- FishData$fhrev
X <- cbind(1, FishData$income, FishData$elf)
solve(t(X) %*% X) %*% t(X) %*% Y



### Preview of the Variance-Covariance Matrix ##

# We will talk about this a lot more in lecture next week, but the
# variance-covariance matrix estimates the covariance between our
# regression coefficients
vcov(mod)

# Note that it is the same thing as this:
# sigma^2 (X'X)^1
(sum(mod$residuals^2)/mod$df.residual) * solve(t(X) %*% X)


# The diagonals are the variances of the coefficients
sqrt(diag(vcov(mod)))
tidy(mod)

# The takeaway: (X'X)^1 matrix is hard to calculate by hand, but it is really
#               important because it gives us both our estimates of the
#               coefficients, and the standard errors for those estimates.
#               This is going to allow us to do hypothesis testing next week

# Betas:
betas <- solve(t(X) %*% X) %*% t(X) %*% Y
betas
tidy(mod)$estimate

# SEs:
ses <- sqrt(diag((sum(mod$residuals^2)/mod$df.residual) * solve(t(X) %*% X)))
ses
tidy(mod)$std.error

# t-values:
t_stats <- (solve(t(X) %*% X) %*% t(X) %*% Y) / (sqrt(diag((sum(mod$residuals^2)/mod$df.residual) * solve(t(X) %*% X))))
t_stats
betas / ses
tidy(mod)$statistic

# p-values:
p_vals <- 2*pt(-abs((solve(t(X) %*% X) %*% t(X) %*% Y) / (sqrt(diag((sum(mod$residuals^2)/mod$df.residual) * solve(t(X) %*% X))))), df=146)
p_vals
2 * pt(-abs(t_stats), df = mod$df.residual)
tidy(mod)$p.value


# Visualizing where p-values come from again:

# The coefficient for the intercept
intercept_t <- t_stats[1]
intercept_t

x = seq(-5, 5, by = 0.01)
plot(
  x = x,
  y = dt(x, df = mod$df.residual),
  type = "l",
  lwd = 3,
  xlab = "t",
  ylab = "Density",
  main = paste("t Distribution \n df =", mod$df.residual)
)
abline(v = abs(intercept_t), lwd = 3, lty = 2, col = "cadetblue")
abline(v = intercept_t, lwd = 3, lty = 2, col = "cadetblue")
x1 <- seq(-5, intercept_t, by = 0.01)
polygon(c(x1, rev(x1)), c(dt(x1, df = mod$df.residual), rep(0, length(x1))), col="cadetblue")
x2 <- seq(abs(intercept_t), 5, by = 0.01)
polygon(c(x2, rev(x2)), c(dt(x2, df = mod$df.residual), rep(0, length(x2))), col="cadetblue")


# The coefficient for beta on elf
elf_t <- t_stats[3]
elf_t

x = seq(-5, 5, by = 0.01)
plot(
  x = x,
  y = dt(x, df = mod$df.residual),
  type = "l",
  lwd = 3,
  xlab = "t",
  ylab = "Density",
  main = paste("t Distribution \n df =", mod$df.residual)
)
abline(v = abs(elf_t), lwd = 3, lty = 2, col = "brown")
abline(v = elf_t, lwd = 3, lty = 2, col = "brown")
x1 <- seq(-5, elf_t, by = 0.01)
polygon(c(x1, rev(x1)), c(dt(x1, df = mod$df.residual), rep(0, length(x1))), col="brown")
x2 <- seq(abs(elf_t), 5, by = 0.01)
polygon(c(x2, rev(x2)), c(dt(x2, df = mod$df.residual), rep(0, length(x2))), col="brown")


### Singular Matrices ###

# Suppose we added a new variable that is equal to 1 minus the
# country's elf score
FishData$one_minus_elf <- 1 - FishData$elf

mod <- lm(fhrev ~ income + elf + one_minus_elf, data = FishData)
summary(mod)

# R cannot estimate this model, and ends up estimating the same model
# that we had before (without the one_minus_elf term)

# The reason is that R is not able to invert the matrix:

Y <- FishData$fhrev
X <- cbind(1, FishData$income, FishData$elf, FishData$one_minus_elf)
t(X) %*% X
det(t(X) %*% X)             # The determinant is zero!
solve(t(X) %*% X)           # So it is impossible to invert this matrix

# The takeaway: You cannot have variables in your model that are 
#               *linear combinations* of other variables in your model


# However, it is ok to do this:

FishData$elf_squared <- FishData$elf ^ 2
mod <- lm(fhrev ~ income + elf + elf_squared, data=FishData)
summary(mod)

X <- cbind(1, FishData$income, FishData$elf, FishData$elf_squared)
det(t(X) %*% X)            # The determinant is greater than zero 
solve(t(X) %*% X)          # So the matrix is invertable this time



# How does this work with categorical variables?
FishData <- FishData %>% 
  mutate(income_level = ntile(income, n = 3),
         income_level = case_when(income_level == 1 ~ "low",
                                  income_level == 2 ~ "middle",
                                  income_level == 3 ~ "high"))

# "high" income level is the reference category
# We interpret the other coefficients in relation to it
mod <- lm(growth ~ income_level, data = FishData)
summary(mod)





