#################################
##           R Lab 9           ##
##       Robust SEs and        ##
##   Instrumental Variables    ##
#################################


library(tidyverse)
library(here)
library(broom)

counties <- read_csv(here("week-7", "data", "counties-clean.csv"))



############################
## Robust Standard Errors ##

# Fit a "simple" model
mod_uncorrected <-
  lm(
    trump_difference ~ prop_manuf + white_pct + median_hh_inc + lesscollege_pct + female_pct + rural_pct + hispanic_pct,
    data = counties
  )

# Save our model data
(mod_uncorrected_data <- augment(mod_uncorrected))

# Let's examine our residuals to check for heteroskedasticity
ggplot(mod_uncorrected_data) +
  aes(x = .fitted, y = .resid) +
  geom_point(color = "violetred4", alpha = 0.3) +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
  theme_minimal()

# Some evidence of a wedge


# We can also test for this using a Breusch-Pagan test
# Where the null hypothesis is that our model has homoskedeastic errors
# install.packages("lmtest")
library(lmtest)
bptest(
  trump_difference ~ prop_manuf + white_pct + median_hh_inc + lesscollege_pct + female_pct + rural_pct + hispanic_pct,
  data = counties
)


# If we think we have heteroscedasticity, this may tell us that our model is mis-specified
# If we are really sure that our model is specified correctly, we could adjust our standard errors
# to correct for the heteroscedasticity

# This is the old fashioned method
# I find it a bit fussy
# install.packages("sandwich")
library(sandwich)
coeftest(mod_uncorrected, vcovHC(mod_uncorrected, type = "HC2"))

# A better way is to simply adjust the errors when fitting the model in the first place
# The function lm_robust from the estimatr package is great for this
# install.packages("estimatr")
library(estimatr)
mod_robust <-
  lm_robust(
    trump_difference ~ prop_manuf + white_pct + median_hh_inc + lesscollege_pct + female_pct + rural_pct + hispanic_pct,
    data = counties
  )

tidy(mod_uncorrected)
tidy(mod_robust)


# can also do commarobust() on an existing lm() object
commarobust(mod_uncorrected)




###############################
## Clustered Standard Errors ##

# This is another common adjustment to the errors that account for the way that the errors of some
# observations are correlated with each other
# While robust standard errors were about the diagonals of the error matrix, clustered standard errors
# are about the off-diagonals (the correlations in errors between different observations)

# We cluster *on* a particular categorical variable in our dataset (but not necessarily one that was
# in our regression!). This is essentially saying "we expect that the observations in this group will
# have similar errors."

# Using estimatr::lm_robust() we simply add a clusters = cluster argument when fitting the model
mod_cluster_robust <-
  lm_robust(
    trump_difference ~ prop_manuf + white_pct + median_hh_inc + lesscollege_pct + female_pct + rural_pct + hispanic_pct,
    data = counties,
    clusters = state_abbrv        # Cluster standard errors by State
  )

# In this case, we are clustering on State. The rationale for doing this is that
# some unmeasured, idiosyncratic characteristic about each State may lead all of the 
# counties to be off by a similar amount

# lfe::felm() can also be used to cluster SEs when fitting a model



# What do you notice about these 3 different methods of calculating standard errors?
screenreg(
  list(mod_uncorrected,
       mod_robust,
       mod_cluster_robust),
  ci.force = T,
  custom.model.names = c("Uncorrected", "HC2 Robust", "Cluster Robust")
)







############################
## Instrumental Variables ##


# First, let's make up some data
n <- 100                                                    # Our sample size
(Z <- rpois(n, 40))                                         # Some variable that will affect our IV of interest
true.beta <- 50                                             # The "true" effect of our IV of interest on the DV

(Confound <- rexp(n, 3))                                      # Some confound that affects both our IV and the DV
(X <- 30 + 1.3*Z + 50*Confound + rnorm(n, 0, 5))              # Our IV of interest
(Y <- 1800 + true.beta*X + 1500*Confound + rnorm(n, 0, 150))  # Our DV

# Note: Confound "affects" both X and Y
plot(Confound, X, pch = 16)
plot(Confound, Y, pch = 16)

# We can get a pretty good estimate of the coefficients if we control for the Confound
summary(lm(Y ~ X + Confound))

# But often the confounding variable is difficult or impossible to measure
# Unfortunately, if we leave the confound out of our regression model,
# that will bias our estimate of the effect of X on Y
summary(lm(Y ~ X))

# In our model, X is "doing some of the work" of the Confound, so we are overestimating its effect on Y


# One solution is to get an instrument for X
# This is a randomly-assigned variable that affects X and affects Y *solely* by way of X
# Due to the way we set up this example, Z is such a variable
plot(Z, X, pch = 16, main = "Relationship between Z and X")
abline(coef(lm(X ~ Z)), col = "blue")
plot(Z, Confound, pch = 16, main = "No Relationship between Z and Confound")
abline(coef(lm(Confound ~ Z)), col = "red")
summary(lm(Confound ~ Z))

# For this simple case where we have just one instrument and one X variable, we can estimate
# the "true" effect of X on Y by estimating two equations:
# (1) X = a + b*Z
# (2) Y = a + b*X.hat, where X.hat are the predicted values from equation (1)
first.stage <- lm(X ~ Z)
second.stage <- lm(Y ~ first.stage$fitted.values)
summary(second.stage)

# Even though we left the Confound out of the model, we still get a pretty good estimate
# of the effect of X on Y

# This works because although Confound IS correlated with X,
# Counfound ISN'T correlated with X.hat
plot(Confound, first.stage$fitted.values, pch=16, ylab="X hat")
abline(coef(lm(first.stage$fitted.values ~ Confound)), col="red")


# Instead of plugging X.hat into the lm() function, it is more common to use a function that
# is specially designed for instrumental variable regressions (this gives us the correct standard errors)
# estimatr::iv_robust is great for this
summary(iv_robust(Y ~ X | Z))

# AER::ivreg is the old way
library(AER)
summary(ivreg(Y ~ X | Z ))

# We can also use felm()
summary(felm(Y ~ 1 | 0 | (X ~ Z)))

# Reminder: We specify felm as: 
# felm( main formula | fixed effects variable | first-stage formula if we have an instrument | cluster variable )
# In this case, we move X to the right of the second "|" to symbolize that this is the variable
# that we are estimating in the "first-stage" equation. But then we have to put something just
# to the right of the "~" so that we are still estimating a "second-stage" equation. We already
# used X, so we are going to use "1" here instead. 1 just stands for the constant or intercept.
# And what about that 0? The 0 just means that we aren't estimating any fixed effects in this
# model. If we were estimating fixed effects, we could replace that 0 with the corresponding
# variable name. We don't need to do anything about the cluster variable option in this case.

# Yes, this syntax is weird. Just roll with it. 


# We can re-simulate our data a few times to convince ourselves that this works in general
Z <- rpois(n, 40)
Confound <- rexp(n, 3)
X <- 30 + 1.3*Z + 50*Confound + rnorm(n, 0, 5)              # Our IV of interest
Y <- 1800 + true.beta*X + 1500*Confound + rnorm(n, 0, 150)  # Our DV
summary(iv_robust(Y ~ X | Z))



# This is a cool tool when it works. But it is based on a very specific set of 
# assumptions, and it quickly breaks down if those assumptions are not met

# In particular, we must assume:
## Relevance: The instrument Z is *highly correlated* with X
## Exclusion Restriction: The instrument Z affects Y only through X


# Problem 1: A Weak Instrument
# This means that we have an instrument that is only weakly-correlated with X
Confound <- rexp(n, 3)
Z <- rpois(n, 40)
X <- 30 + 0.3*Z + 50*Confound + rnorm(n, 0, 5)              # Before, the effect of Z on X was 1.3; we will reduce it to just 0.3 and leave everything else the same
Y <- 1800 + true.beta*X + 1500*Confound + rnorm(n, 0, 150)
summary(iv_robust(Y ~ X | Z))                               # Keep in mind that the true Beta should be 50

# Our estimate becomes very unreliable
# To see this, true running the previous couple of lines a few times and note what happens
# to the coefficient for X


# Problem 2: Our Confound is Correlated with the Instrument
# This can happen if there is some other alternative causal pathway other than X by which 
# Z affects Y. Note that Z does not every need to affect the confound for this to be a problem!
Confound <- .05*Z + rexp(n, 3)                              # We are re-specifying Confound so that it is correlated with Z
X <- 30 + 1.3*Z + 50*Confound + rnorm(n, 0, 5)              # We will go back to the version of X that we had before
Y <- 1800 + true.beta*X + 1500*Confound + rnorm(n, 0, 150)
summary(iv_robust(Y ~ X | Z))                               # Keep in mind that the true Beta should be 50

# Now we aren't doing any better than we did when we were just using OLS!
summary(lm(Y ~ X))


# The takeaway:
# Even a small violation of the assumptions behind instrumental variables makes the
# method pretty much worthless

# The "Z is correlated with X" assumption is easy to test
# The "No alternative pathways" assumption (known as the "exclusion restriction")
# is fundamentally untestable and can only be argued

# Instrumental variable analyses live and die on the researcher's ability to argue that
# there are no alternative causal pathways between Z and Y. In order to make this argument
# convincing, you have to know your case / data generating process inside and out

# Even then, there is a 0.98 chance that a few minutes into your conference presentation
# or job talk, someone sitting in the back of the room will propose an
# alternative pathway that you haven't thought of


# Unsolicited career advice: Understand what this method is doing, 
# but don't stake your dissertation or early research agenda on being able to pull it off. 

# (The main exception is if you are doing an "encouragement experiment" or a 
# "fuzzy regression discontinuity." In these cases, the exclusion restriction
# is often quite plausible.)


