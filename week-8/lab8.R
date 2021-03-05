#################################
##           R Lab 8           ##
##  Post-Estimation Quantities ##
##         & Diagnostics       ##
#################################


library(here)
library(tidyverse)
library(broom)
library(texreg)




######################################
## Using Models to Make Predictions ##

# We will use the county data we've been using for the past few weeks
counties <- read_csv(here("week-7", "data", "counties-clean.csv")) %>% 
  mutate(median_hh_inc = median_hh_inc / 1000,
         rural = if_else(ruralurban_cc >= 7, 1, 0))


# Let's start back at the beginning with a simple bivariate regression
# We build an lm() model object by regressing trump_difference on prop_manuf
simple_mod <- lm(trump_difference ~ prop_manuf, data = counties)
screenreg(simple_mod)

# I will save the intercept and slope
(intercept <- coef(simple_mod)[1] %>% round(3) %>% as.numeric())
(beta_1 <- coef(simple_mod)[2] %>% round(3) %>% as.numeric())

counties %>% 
  ggplot(aes(x = prop_manuf, y = trump_difference)) +
  geom_point(color = "darkcyan", alpha = 0.1, size = 3) +
  geom_smooth(method = "lm", se = F, color = "darkred") +
  annotate(
    geom = "text",
    label = paste("trump_difference =", intercept, "+", beta_1, "* prop_manuf"),
    x = 50,
    y = -23
  ) +
  theme_minimal()

# KEY POINT: The fitted line represents the model's predictions about Y for each value of X
# This allows us to plug ANY number for X into the model and find the predicted Y value at that point

# What is the predicted Trump vote swing for a county with 40% manufacturing?
# Just plug it in and solve!
(predicted_Y <- 1.657 + 0.132 * 40)
(predicted_Y <- intercept + beta_1 * 40)
(predicted_Y <- coef(simple_mod)[1] + coef(simple_mod)[2] * 40)

# We can also eyeball this using the scatter plot

# What about the predicted Trump vote swing for a county with 80% manufacturing? 
# (more than any real county!)
# We can still do this because the line extends forever!
(predicted_Y <- intercept + beta_1 * 80)

# There are no rules!!!!!!!
(predicted_Y <- intercept + beta_1 * 200)
(predicted_Y <- intercept + beta_1 * -1000)

# We have discovered one limit of OLS regression...


# What if we wanted to find the predicted trump_difference values for a bunch of
# different levels of prop_manuf?

# First we need a new data frame holding the hypothetical values we want for X
prediction_data <- data.frame(
  prop_manuf = c(0, 25, 50, 75, 100) # The column must have the same name as the X variable in the model
)

# Now we can feed this into predict() along with our model object
predict(simple_mod, newdata = prediction_data)

# Personally I prefer the broom::augment() function, however
# It always keeps the original input data and has a consistent set of outputs
# The variable .fitted contains the predicted Y values (aka Y hat)
augment(simple_mod, newdata = prediction_data)

# Standard errors and confidence intervals are easy to generate as well!
(predicted_points <- 
  augment(simple_mod, newdata = prediction_data, se_fit = TRUE, interval = "confidence"))

# Let's graph this for fun
predicted_points %>% 
  ggplot(aes(x = prop_manuf, y = .fitted)) +
  geom_point() +
  geom_errorbar(aes(ymin = .lower, ymax = .upper),  # Error bars come straight from our predicted_points data
                width = 1) +
  labs(y = "Predicted Trump Difference") +
  theme_minimal()


# But what if we have a more complicated model?
multi_mod <- lm(trump_difference ~ prop_manuf + white_pct + rural + lesscollege_pct + female_pct,
                data = counties)

augment(multi_mod, newdata = prediction_data)
# What happened!

# It does not make sense to predict trump_difference from only prop_manuf anymore!
# The values of each other independent variable in the model matter too
# So let's create new data with values for each X variable
one_really_specific_county <- data.frame(
  prop_manuf = 60,
  white_pct = 45,
  rural = 1,
  lesscollege_pct = 20,
  female_pct = 55
)

augment(multi_mod, newdata = one_really_specific_county)

# This is a hassle... and who cares about this county???

# There is a better way

# We can hold all other variables in the model at their medians
# and see what happens to the predictions when we change prop_manuf

# First, let's use broom::augment save the values of each variable used in the model
(model_data <- augment(multi_mod))

(prediction_data <- tibble(
  prop_manuf = seq(0, 100, by = 1),           # We generate a sequence of variables across the range of prop_manuf
  white_pct = median(model_data$white_pct),   # All other variables are held constant at a "typical" value
  rural = median(model_data$rural),
  lesscollege_pct = median(model_data$lesscollege_pct),
  female_pct = median(model_data$female_pct)
))

# Now we can make predictions about what happens to trump_difference when we change prop_manuf
# holding all other variables at their median values
(predictions <-
  augment(multi_mod, newdata = prediction_data, se_fit = T, interval = "confidence"))

# Let's plot it!
ggplot(predictions) +
  aes(x = prop_manuf, y = .fitted) +
  geom_ribbon(aes(
    ymin = .lower,
    ymax = .upper,
  ), fill = "darkcyan", alpha = 0.5) +
  geom_line() +
  labs(y = "Predicted Trump Difference") +
  theme_minimal()

# This is called a "Conditional Effects Plot"
# In my opinion it is one of the most powerful ways to visualize the results of your model

# KEY POINT: Regression models are prediction machines first and foremost



# Now let's try something more interesting with our predictions
# The tidyr::crossing() function is incredibly useful for quickly making combinations of variables
crossing(
  letter = c("a", "b", "c"),
  number = c(1, 2)
)

crossing(
  colors = c("red", "blue", "green"),
  shapes = c("square", "triangle", "circle"),
  flavors = c("chocolate", "caramel", "mint")
)

# Let's cross our prediction data with rural c(1, 0)
# This creates a data set with every combination of the dummy rural and integer prop_manuf
# We are still holding all other variables at their medians
prediction_data <- tibble(
  prop_manuf = seq(0, 100, by = 1), 
  white_pct = median(model_data$white_pct),
  lesscollege_pct = median(model_data$lesscollege_pct),
  female_pct = median(model_data$female_pct)
) %>% 
  crossing(rural = c(1, 0)) %>% 
  print()
  
# Generate our predictions
predictions <- augment(multi_mod, newdata = prediction_data, se_fit = T, interval = "confidence")

# Now we plot again
ggplot(predictions) +
  aes(
    x = prop_manuf,
    y = .fitted,
    color = as.factor(rural),
    fill = as.factor(rural)
  ) +
  geom_ribbon(aes(ymin = .lower,
                  ymax = .upper),
              color = NA,
              alpha = 0.3) +
  geom_line() +
  labs(y = "Predicted Trump Difference") +
  theme_minimal()
  
# This shows that rural counties have higher swings towards Trump
# But the slopes are constrained to be equal
# What if we want the slopes to be able to vary?

# First fit a new model with the interaction term
int_mod <- lm(trump_difference ~ rural*prop_manuf + white_pct + lesscollege_pct + female_pct,
              data = counties)
  
# We make the prediction data in the same way
prediction_data <- tibble(
  prop_manuf = seq(0, 100, by = 1), 
  white_pct = median(model_data$white_pct),
  lesscollege_pct = median(model_data$lesscollege_pct),
  female_pct = median(model_data$female_pct)
) %>% 
  crossing(rural = c(1, 0)) %>% 
  print()

# Generate our predictions
predictions <- augment(int_mod, newdata = prediction_data, se_fit = T, interval = "confidence")

# wow amazing!
ggplot(predictions) +
  aes(
    x = prop_manuf,
    y = .fitted,
    color = as.factor(rural),
    fill = as.factor(rural)
  ) +
  geom_ribbon(aes(ymin = .lower,
                  ymax = .upper),
              color = NA,
              alpha = 0.3) +
  geom_line() +
  labs(y = "Predicted Trump Difference") +
  theme_minimal()


# I love this because it makes the interaction so easy to interpret!
# We can directly examine our model's predictions across a range of one variable by values of the interacted variable



# What about visualizing a continuous*continuous interaction?
# Here is that model with prop_manuf*white_pct
int_mod <- lm(trump_difference ~ prop_manuf*white_pct + rural + lesscollege_pct + female_pct,
              data = counties)

# Let's cross our sequence of prop_manuf with different quantiles of white_pct
# If we crossed 100x100 the graph would be awful!
prediction_data <- tibble(
  prop_manuf = seq(0, 100, by = 1),
  lesscollege_pct = median(model_data$lesscollege_pct),
  female_pct = median(model_data$female_pct),
  rural = median(model_data$rural)
) %>% 
  crossing(
    white_pct = c(0, 25, 50, 75, 100)
  ) %>% 
  print()

predictions <- augment(int_mod, newdata = prediction_data, se_fit = T, interval = "confidence")

ggplot(predictions) +
  aes(
    x = prop_manuf,
    y = .fitted,
    color = as.factor(white_pct),
    fill = as.factor(white_pct)
  ) +
  geom_ribbon(aes(ymin = .lower,
                  ymax = .upper),
              color = NA,
              alpha = 0.3) +
  geom_line() +
  labs(y = "Predicted Trump Difference") +
  theme_minimal()


# Don't forget the interactions always go both ways!
# So we can, instead, look at how white_pct changes by levels of prop_manuf
prediction_data <- tibble(
  white_pct = seq(0, 100, by = 1),
  lesscollege_pct = median(model_data$lesscollege_pct),
  female_pct = median(model_data$female_pct),
  rural = median(model_data$rural)
) %>% 
  crossing(
    prop_manuf = c(0, 25, 50, 75, 100)
  ) %>% 
  print()

predictions <- augment(int_mod, newdata = prediction_data, se_fit = T, interval = "confidence")

ggplot(predictions) +
  aes(
    x = white_pct,
    y = .fitted,
    color = as.factor(prop_manuf),
    fill = as.factor(prop_manuf)
  ) +
  geom_ribbon(aes(ymin = .lower,
                  ymax = .upper),
              color = NA,
              alpha = 0.3) +
  geom_line() +
  labs(y = "Predicted Trump Difference") +
  theme_minimal()



# One more way to create these types of plots
# Instead of using predict() or augment(), we are generating samples from a multi-variate distribution of our betas
# This allows us to simulate hypothetical lines, given our uncertainty over our betas
# This is similar to how the Bayesians do it :)

# Start with a simple interaction model
mod <- lm(trump_difference ~ prop_manuf*rural, data = counties)

# Create prediction data 
prediction_data <- tibble(
  constant = 1,                # Because we are matrix multiplying later, we need a column of 1's
  prop_manuf = 0:100
) %>% 
  crossing(rural = c(0, 1)) %>% 
  mutate(`prop_manuf:rural` = prop_manuf * rural) %>% 
  print()

# Sample from the multivariate normal distribution using mvtnorm::rmvnorm()
# You will be doing this a lot in Maximum Likelihood, POLI 271
coef_matrix <- 
  mvtnorm::rmvnorm(
    n = 1000,
    mean = coef(mod),
    sigma = vcov(mod)
  ) %>%
  as_tibble() %>%
  print()

# Multiply each row of our prediction data by the appropriate coefficients we just sampled
prediction_matrix <- prediction_data %>%
  as.matrix() %>%
  (function(x) x %*% t(coef_matrix))

# we have 1000x predictions
dim(prediction_matrix)

# Tidy up the data for graphing
simulation_frame <- prediction_matrix %>%
  as_tibble() %>%
  bind_cols(prediction_data, .) %>%
  gather(key = iteration, value = TrumpDifference, starts_with("V")) %>%
  print() 

# nice
ggplot(simulation_frame) +
  aes(x = prop_manuf, y = TrumpDifference) +
  geom_line(
    data = filter(simulation_frame, rural == 0),
    aes(group = iteration),
    alpha = 0.1, size = 0.1,
    color = "#F8766D"
  ) +
  geom_line(
    data = filter(simulation_frame, rural == 1),
    aes(group = iteration),
    alpha = 0.1, size = 0.1,
    color = "#00BFC4"
  ) +
  theme_minimal()

# The variance in the lines is a nice way to visualize our uncertainty




##########################
## Part II: Diagnostics ##

# (i.e., how to avoid making a huge mistake, 
# and/or reassure Reviewer 2 that Forest, PA isn't driving our results)


# First I need to add county-state values as row names
# And delete duplicate counties??
counties <- counties %>% 
  mutate(county_name = paste0(county, ", ", state_abbrv)) %>% 
  filter(!(county_name %in% c("Bedford, VA", "Fairfax, VA", "Franklin, VA", "Richmond, VA", "Roanoke, VA"))) %>%  
  column_to_rownames(var = "county_name")

# This is the model we will be working with
multi_mod <- lm(trump_difference ~ prop_manuf + white_pct + rural + lesscollege_pct + female_pct,
                data = counties)
screenreg(multi_mod)

# We will also work extensively with the post-estimation quantities from broom:augment()
(model_data <- augment(multi_mod))

# KEY POINT: the values produced by broom:augment() are at the observation-level 
# In contrast to variable-level coefficents
# Or model-level statistics (R^2, Residual Standard Error, etc)


### Plotting Residuals ###

# Why do this?
# This lets us inspect whether the constant-variance assumption holds
# and whether the relationship between Y and X is linear

# We can plot the residuals with respect to only one independent variable at a time
ggplot(model_data) +
  aes(x = prop_manuf, y = .resid) +
  geom_point(color = "violetred4", alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  theme_minimal()

ggplot(model_data) +
  aes(x = white_pct, y = .resid) +
  geom_point(color = "violetred4", alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  theme_minimal()

# A common way to visualize the residuals with respect to all of our IVs simultaneously
# is to put fitted values of Y (aka Y Hat, aka predicted values of Y) on the x-axis
ggplot(model_data) +
  aes(x = .fitted, y = .resid) +
  geom_point(color = "violetred4", alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  theme_minimal()

# There is evidence of heteroskedasticity (more on this next week),
# and possibly a non-linear relationship



### Leverages ###

# Why do this?
# This tells us which observations could *potentially* exert a large influence on our 
# regression results, based on unusual combinations of the observation's independent variable values

# The basic idea:
# The leverages are the diagonal of the "hat" matrix 
# (i.e., the matrix that converts Y into Y hat)

# The hard way to do this:
X_data <- model_data %>% 
  select(prop_manuf, white_pct, rural, lesscollege_pct, female_pct) %>% 
  print()
X <- as.matrix(cbind(1, X_data))
H <- X %*% solve(t(X) %*% X) %*% t(X)
h <- diag(H)

# The easy way to do this:
# model_data$.hat

ggplot(model_data) +
  aes(x = .fitted, y = .hat) +
  geom_point(color = "violetred4", alpha = 0.5) +
  theme_minimal()

# Let's find out which pesky county that is!
ggplot(model_data) +
  aes(x = .fitted, y = .hat) +
  geom_text(aes(label = .rownames), check_overlap = T) +
  theme_minimal()



### Studentized Residuals ###

# Why do this?
# This gives us a version of the residuals that takes into account the way that
# influential outliers pull the regression line towards themselves

# The basic idea:
# Use the leverages (h) that we just calculated above to account for the observation's influence
# Then use the sigma from the regression that leaves out that observation to standardize it

# The hard way to do this:

# Estimate the standard errors
formula <- trump_difference ~ prop_manuf + white_pct + rural + lesscollege_pct + female_pct
sigmas <- c()
for(i in 1:nrow(model_data)) {
  without_i <- lm(formula, data = model_data[-i, ])
  sigmas[i] <- glance(without_i)$sigma
}

# Get the residuals, and standardize them
e <- model_data$.resid
e_star <- e / (sigmas * sqrt(1 - h))
# Note: h is the leverage that we calculated in the previous section

# Now we can make a residuals plot like we did before
plot(model_data$.fitted, e_star, col = "violetred4")
abline(h = 0, lty = 2, lwd = 2)

# The easy way to do this:
# model_data$.std.resid

ggplot(model_data) +
  aes(x = .fitted, y = .std.resid) +
  geom_point(color = "violetred4", alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  theme_minimal()




### DF Beta ###

# Why do this?
# This tells us how much influence each observation *actually* exerts on the slope
# (leverages only take into account the Xs; DF-Betas take into account both Xs and Y)

# The basic idea:
# Calculate the regression slopes twice:
# 1. using a dataset that includes the ith observation
# 2. using a dataset that excludes the ith observation
# Then take the difference between the two versions of beta to estimate
# how much the ith observation affects the beta

# The hard way to do this:
formula <- trump_difference ~ prop_manuf + white_pct + rural + lesscollege_pct + female_pct
D <- matrix(NA, nrow = nrow(model_data), ncol = multi_mod$rank)
with_i <- lm(formula, data = model_data)$coefficients
for(i in 1:nrow(model_data)) {
  without_i <- lm(formula, data = model_data[-i,])$coefficients
  D[i, ] <- with_i - without_i
}
rownames(D) <- model_data$.rownames
colnames(D) <- names(with_i)

# That gives us a n by (k+1) matrix with the difference in each beta when each
# observation is included/excluded in turn
head(D)

# We can plot a histogram of one of the columns of betas to see how much a given
# coefficient changes, depending on which observation is excluded
hist(D[ ,"prop_manuf"])

# We can also plot the DF-Betas for different coefficients against each other
plot(
  NA,
  xlim = c(1.2 * min(D[, "prop_manuf"]), 1.2 * max(D[, "prop_manuf"])),
  ylim = c(1.2 * min(D[, "white_pct"]), 1.2 * max(D[, "white_pct"])),
  col = "white",
  xlab = "prop_manuf",
  ylab = "white_pct"
)
text(D[, "prop_manuf"], D[, "white_pct"], as.character(model_data$.rownames), cex = .8)


# The easy way to do this
dfbetas_mod <- dfbeta(multi_mod)

plot(
  NA,
  xlim = c(1.2 * min(dfbetas_mod[, "prop_manuf"]), 1.2 * max(dfbetas_mod[, "prop_manuf"])),
  ylim = c(1.2 * min(dfbetas_mod[, "white_pct"]), 1.2 * max(dfbetas_mod[, "white_pct"])),
  col = "white",
  xlab = "prop_manuf",
  ylab = "white_pct"
)
text(dfbetas_mod[, "prop_manuf"], dfbetas_mod[, "white_pct"], as.character(model_data$.rownames), cex = .8)




### Cook's Distance ###

# Why do this?
# This is similar to the DF-Betas, but it calculates just a single measure of influence for
# each observation
# AND we can access it using broom::augment() yay!


# The hard way to do this:
y_hat <- model_data$.fitted
e <- model_data$.resid
h <- model_data$.hat
k <- multi_mod$rank
D <- ((e ^ 2) / (k * sum(e ^ 2) / (nrow(model_data) - k))) * (h / (1 - h) ^ 2)
plot(y_hat, D, col="white", xlim=c(0, 7))
text(y_hat, D, model_data$.rownames, cex=.8)


# The easy way to do this:
# model_data$.cooksd
ggplot(model_data) +
  aes(x = .fitted, y = .cooksd) +
  geom_text(aes(label = .rownames), check_overlap = F) +
  theme_minimal()


