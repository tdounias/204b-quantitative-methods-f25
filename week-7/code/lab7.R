######################################
##             R Lab 7              ##
##  Fixed Effects and Interactions  ##
######################################

# Read in the packages for today
library(tidyverse)
library(here)
library(broom)
library(texreg)

# This lab will use the same Counties data set that we created last week
counties <- read_csv(here("week-7", "counties-clean.csv")) %>% 
  mutate(median_hh_inc = median_hh_inc / 1000)    # Let's scale this variable so it is easier to interpret



#####################
### Fixed Effects ###
#####################

# We want to know the relationship between manufacturing employment and Trump vote share
# But when we plot the data by whether the county is in New York or Virginia
# we notice a pattern

ny_va <- counties %>% 
  filter(state_abbrv %in% c("NY", "VA")) %>%         # Filtering out counties not in NY or VA
  mutate(VA = if_else(state_abbrv == "VA", 1, 0))    # Creating a dummy variable for VA

# A positive trend
ny_va %>% 
  ggplot(aes(x = prop_manuf, y = trump_difference)) +
  geom_point(color = "darkcyan", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = F, fullrange = T, color = "darkcyan") +
  theme_minimal() 

# Positive trends, but VA is consistently lower than NY
# Perhaps these states should have their own intercept?
ny_va %>% 
  ggplot(aes(x = prop_manuf, y = trump_difference, color = state_abbrv)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  theme_minimal() 

# Basic model
ny_va_mod <- lm(trump_difference ~ prop_manuf, data = ny_va)
# State fixed-effects model
ny_va_mod_fe <- lm(trump_difference ~ prop_manuf + VA, data = ny_va)

# Can you interpret these coefficients?
screenreg(list(ny_va_mod, ny_va_mod_fe))


# What if we subset to Utah and Virginia instead?
ut_va <- counties %>% 
  filter(state_abbrv %in% c("UT", "VA")) %>% 
  mutate(VA = if_else(state_abbrv == "VA", 1, 0))

# Nice positive trend...
ut_va %>% 
  ggplot(aes(x = prop_manuf, y = trump_difference)) +
  geom_point(color = "darkcyan", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = F, fullrange = T, color = "darkcyan") +
  theme_minimal() 

# But...
ut_va %>% 
  ggplot(aes(x = prop_manuf, y = trump_difference, color = state_abbrv)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  theme_minimal() 

# Basic model
ut_va_mod <- lm(trump_difference ~ prop_manuf, data = ut_va)
# State fixed-effects model
ut_va_mod_fe <- lm(trump_difference ~ prop_manuf + VA, data = ut_va)

# What do we notice here?
screenreg(list(ny_va_mod, ny_va_mod_fe, ut_va_mod, ut_va_mod_fe),
          custom.header = list("NY and VA" = 1:2, "UT and VA" = 3:4))


# Tip: plot your data!


# Now lets look at the whole country
counties %>% 
  ggplot(aes(x = prop_manuf, y = trump_difference)) +
  geom_point(color = "darkcyan", alpha = 0.2, size = 3) +
  geom_smooth(method = "lm", se = F, fullrange = T, color = "darkcyan") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))

# Very useless plot
counties %>% 
  ggplot(aes(x = prop_manuf, y = trump_difference, color = state_abbrv)) +
  geom_point(alpha = 0.2, size = 3) +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  theme_minimal() 
# Maybe plotting is not always the best idea

# Basic model
county_mod <- lm(trump_difference ~ prop_manuf, data = counties)
# State fixed-effects model
county_mod_fe <- lm(trump_difference ~ prop_manuf + state_abbrv, data = counties)

# Uh...... bad!
screenreg(list(county_mod, county_mod_fe))

# There is a better way!

# install.packages("lfe")
library(lfe)

# Let's add some controls
# Why are we adding these specific controls?
county_mod <- 
  lm(
    trump_difference ~ prop_manuf + white_pct + female_pct + 
      age65andolder_pct + median_hh_inc + lesscollege_pct,
    data = counties
  )

# Add state fixed-effects with | state_abbrv
# We could add more fixed-effects if we had multiple years in our data (a panel)
# lfe makes this easy with y ~ x1 + x1 | fe1 + fe2
county_felm <-
  felm(
    trump_difference ~ prop_manuf + white_pct + female_pct + 
      age65andolder_pct + median_hh_inc + lesscollege_pct |
      state_abbrv,
    data = counties
  )

# We can also cluster our standard errors at the state level
# y ~ x1 + x2 | fe1 | 0 | fe1
# This typically makes our standard errors larger, we will cover this more in later classes
county_felm_se <-
  felm(
    trump_difference ~ prop_manuf + white_pct + female_pct + 
      age65andolder_pct + median_hh_inc + lesscollege_pct |
      state_abbrv | 0 | state_abbrv,
    data = counties
  )

# This model is essentially controlling for all time-invariant characteristics that are unique to the state 
# that might affect their trump_difference support in every county
# Another way of thinking about this is that we are allowing each state to have its own "baseline" 
# trump swing measure, and we are exploiting only the variation *within* a given state

screenreg(list(county_mod, county_felm, county_felm_se),
          custom.header = list("FE" = 2:3),
          digits = 4)



# Questions?




###########################################
## Interactions With Two Dummy Variables ##

# Let's build another dummy variable for urban/rural in our Utah and Virginia data
ut_va <- ut_va %>% 
  mutate(rural = if_else(ruralurban_cc >= 7, 1, 0))

# Two ways of creating interactions in lm()
# y ~ x1 + x2 + x1:x2
mod_int <- lm(trump_difference ~ VA + rural + VA:rural, data = ut_va)
screenreg(mod_int)



m <- lm(trump_difference ~ prop_manuf + white_pct, data = counties)

new_dat <- data.frame(
  prop_manuf = c(25, 50, 75),
  white_pct = mean(counties$white_pct, na.rm = T)
)

predict(m, newdata = new_dat, interval = "confidence")


# Or
# y ~ x1 * x2
mod_int <- lm(trump_difference ~ VA*rural, data = ut_va)
screenreg(mod_int)

# How to interpret this?
# trump_difference = -24.1 + 24.6 * VA + 13.9 * rural - 8.0 * VA * rural
#       -24.1: predicted trump vote for Utah and non-rural county
#        24.6: difference in trump vote between Virginia and Utah for non-rural counties
#        13.9: difference in trump vote between rural and non-rural counties for Utah
#         8.0: difference in slope for rural between Utah and Virginia



######################################################################
## Interactions With One Continuous Variable and One Dummy Variable ##

# Let's examine how the manufacturing proportion effects trump support in the two states
mod_int <- lm(trump_difference ~ prop_manuf + VA + prop_manuf:VA, data = ut_va)


plot(
  ut_va$prop_manuf,
  ut_va$trump_difference,
  ylab = "Prop Manufacturing",
  xlab = "Trump Vote Swing",
  pch = 19,
  col = ifelse(ut_va$VA == 1, "forestgreen", "darkorange")
)
abline(
  a = coef(mod_int)[1],
  b = coef(mod_int)[2],
  col = "darkorange",
  lwd = 3
)
abline(
  a = coef(mod_int)[1] + coef(mod_int)[3],
  b = coef(mod_int)[2] + coef(mod_int)[4],
  col = "forestgreen",
  lwd = 3
)


### Why do we have to include the "main effects" in addition to the interaction term?

# Let's see what happens if we omit the main effect for Virginia
wrong_mod <- lm(trump_difference ~ prop_manuf + prop_manuf:VA, data = ut_va)
screenreg(wrong_mod)

# We are forcing each state to have the same intercept!
# Rarely the correct idea
plot(
  ut_va$prop_manuf,
  ut_va$trump_difference,
  ylab = "Democracy",
  xlab = "Log GDP per capita",
  pch = 19,
  col = ifelse(ut_va$VA == 1, "forestgreen", "darkorange"),
  xlim = c(-5, 50)
)
abline(
  a = coef(wrong_mod)[1],
  b = coef(wrong_mod)[2],
  col = "darkorange",
  lwd = 3
)
abline(
  a = coef(wrong_mod)[1],
  b = coef(wrong_mod)[2] + coef(wrong_mod)[3],
  col = "forestgreen",
  lwd = 3
)
abline(v = 0, col = "grey50")

# Make sure to include the main effects in your interaction
plot(
  ut_va$prop_manuf,
  ut_va$trump_difference,
  ylab = "Democracy",
  xlab = "Log GDP per capita",
  pch = 19,
  col = ifelse(ut_va$VA == 1, "forestgreen", "darkorange"),
  xlim = c(-5, 50)
)
abline(
  a = coef(mod_int)[1],
  b = coef(mod_int)[2],
  col = "darkorange",
  lwd = 3
)
abline(
  a = coef(mod_int)[1] + coef(mod_int)[3],
  b = coef(mod_int)[2] + coef(mod_int)[4],
  col = "forestgreen",
  lwd = 3
)
abline(v = 0, col = "grey50")




### F test ###

# Does "State" matter?

# It is tempting to answer this question by looking at whether 
# the main effect on the VA term is significant
screenreg(mod_int)

# But that is the wrong approach. The main effect tells us only whether the *intercept* for
# VA counties is different from the intercept for Utah countries

# In order to determine whether the VA *variable* matters in general, we have to do an F-test:

mod_full <- lm(trump_difference ~ prop_manuf + VA + prop_manuf:VA, data = ut_va)
mod_restricted <- lm(trump_difference ~ prop_manuf, data = ut_va)
anova(mod_restricted, mod_full)




#####################################################
### Interactions between two continuous variables ###

# Here's our model
# We are interacting manufacturing proportion by white percentage
mod_cont <- lm(trump_difference ~ prop_manuf * white_pct + female_pct + 
                 age65andolder_pct + median_hh_inc + lesscollege_pct + state_abbrv, data = counties)
summary(mod_cont)

# This interaction effect is a lot harder to interpret

# Here is the most common way:
# Calculate the "marginal effect" of one of our interacted IVs, conditional on the value
# of the other interacted IV

# When white_pct is 0, the effect of prop_manuf on trump_diff is -0.036; 
# Each 1 unit increase in white_pct increases the effect of prop_manuf on trump_diff by 0.002

# We can think of this marginal effect as "the partial derivative with respect to prop_manuf":
# Our line: Y = -3.824 -  0.036*M + 0.075*W + 0.002*M*W
# Effect of W =    0   -  0.036   +    0    + 0.002*W
#             = -0.036 +  0.002*W


# We can also show this graphically
coefficients <- coef(mod_cont)
variance_covariance <- vcov(mod_cont)

first_iv_name <- "prop_manuf"
second_iv_name <- "white_pct"
interaction_name <- paste(first_iv_name, ":", second_iv_name, sep = "")
first_iv <- augment(mod_cont)$prop_manuf
second_iv <- augment(mod_cont)$white_pct
interaction <- first_iv * second_iv
range <- seq(min(second_iv), max(second_iv), (max(second_iv) - min(second_iv)) / 100)

# Now we can calculate the effect of prop_manuf on trump_difference for each value of white_pct in the range
slopes <- coefficients[first_iv_name] + coefficients[interaction_name] * range

# We can calculate the standard errors for these predictions using the formula in Scott's slides; 
# this will allow us to make a confidence interval
SEs <- sqrt(variance_covariance[first_iv_name, first_iv_name] + 
              (range^2) * variance_covariance[interaction_name, interaction_name] + 
              2 * range * variance_covariance[first_iv_name, interaction_name])
upper <- slopes + 1.96 * SEs
lower <- slopes - 1.96 * SEs
ylim <- c(min(lower), max(upper))

# Now we are ready to plot the marginal effect curve and its confidence interval
plot(
  range,
  slopes,
  type = "l",
  lty = 1,
  ylim = ylim,
  xlab = second_iv_name,
  ylab = paste("Marginal Effect of", first_iv_name),
  main = paste(
    "Effect of ",
    first_iv_name,
    ", conditional on ",
    second_iv_name,
    sep = ""
  )
)
polygon(c(range, rev(range)),
        c(lower, rev(upper)),
        col = "gray80",
        border = NA)
lines(range, slopes, lwd = 2)
abline(0, 0, lty = 3)

# We see that the effect of prop_manuf on trump_difference is around 0.05 when white_pct is 50, 
# And the effect grows steeply as white_pct increases
# but the effect is negative when white_pct < 20



# Here is a package that will let us make these plots easier :)
# install.packages("interplot")
library(interplot)

interplot(mod_cont, "prop_manuf", "white_pct") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Marginal Effect of Proportion Manufacturing") + 
  xlab("White Percentage")

# Adding a histogram for the conditioning variable is very useful
interplot(mod_cont, "prop_manuf", "white_pct", hist = T) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Marginal Effect of Proportion Manufacturing") + 
  xlab("White Percentage")


