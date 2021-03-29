# Modelos aditivos generalizados (GAMs -- generalized additive models)
#GAMs in R by NOAM ROSS
#Carpeta del curso
setwd("C:/Users/Mafer/Documents/GAMs")

################################################################################
##                     Interpreting and Visualizing GAMs                      ##
################################################################################
#In this chapter, you will take a closer look at the models you fit in chapter 1
#and learn how to interpret and explain them. You will learn how to make plots 
#that show how different variables affect model outcomes. Then you will diagnose
#problems in models arising from under-fitting the data or hidden relationships 
#between variables, and how to iteratively fix those problems and get better results.

library(mgcv)
library(gamair)
data("mpg", package="gamair")

########################### Interpreting GAM outputs ###########################

mod_hwy <- gam(hw.mpg ~ s(weight) + s(rpm) + 
                 s(price) + s(comp.ratio) +
                 s(width) + fuel + cylinders,
               data = mpg, method = "REML")

summary(mod_hwy)

######################## Significance and linearity ############################
#It’s time for you to summarize a model and interpret the output.

#Instructions

#Summarize the mod_city4 model, then answer the questions.

library(gamair)
data("mpg", package="gamair")
library(mgcv)
# Fit the model
mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")
# View the summary
summary(mod_city4)

################# Plotting the motorcycle crash model and data #################
#For our first plotting exercise, we’ll add partial residuals to the partial 
#effect plot of a GAM so as to compare the model to the data.

#Plot the model (mod) that uses mcycle data. Add partial residuals to the plot.
#Make a second plot, making partial residuals more visible by changing the shape 
#using the pch argument, and size of the residuals using the cex argument. 
#Set both the pch and cex arguments to 1.

mcycle <- MASS::mcycle

library(mgcv)
# Fit the model
mod <- gam(accel ~ s(times), data = mcycle, method = "REML")

# Make the plot with residuals
plot(mod, residuals = TRUE)

# Change shape of residuals
plot(mod, residuals = TRUE, pch = 1, cex = 1)


################ Plotting multiple auto performance variables ##################

#In plotting GAMs, you sometimes want to look at just parts of a model, or all
#the terms in model. Here you’ll practice selecting which terms to visualize.

#Instructions

#Plot the provided model (mod) that uses the mpg data, using the select term to
#view only the partial effect of price.
#Make another plot, this time showing all terms on a single page, including the
#linear comp.ratio term.

library(gamair)
data("mpg", package="gamair")

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")

# Plot the price effect
plot(mod, select = 3)

# Plot all effects
plot(mod, pages = 1, all.terms = TRUE)

################ Visualizing auto performance uncertainty ######################
#Confidence intervals are a very important visual indicator of model fit. Here 
#you’ll practice changing the appearance of confidence intervals and transforming
#the scale of partial effects plots.

#Instructions

#Plot the model (mod) that uses the mpg data, plotting only the partial effect 
#of weight. Make the confidence interval shaded and "hotpink" in color.
#Make another plot of the weight partial effect, this time shifting the scale by
#the value of the intercept using the shift argument, and including the uncertainty
#of the model intercept using the seWithMean argument.

library(gamair)
data("mpg", package="gamair")

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")

# Plot the weight effect
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink")

# Make another plot adding the intercept value and uncertainty
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE)


######################## Reading model diagnostics #############################

#gam.check() helps you understand whether you have enough basis functions to model
#the data.

#Instructions

#Print diagnostics on model (mod) basis size and plots of model residuals, than 
#answer the question.

library(mgcv)
set.seed(0)
dat <- gamSim(1,n=200)

library(mgcv)
# Fit the model
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")

# Run the check function
gam.check(mod)


################### Fixing problems with model diagnostics #####################
#You can use gam.check() to improve models by updating them based on its results.

#Instructions

#Run the model diagnostics on mod.
#Based on the diagnostics, re-fit the model as mod2, changing the number of basis
#functions (k) for failing smooths.
#Run the model diagnostics on mod2 to ensure you have fixed the issue.

set.seed(0)
dat <- mgcv::gamSim(1,n=600, scale=0.6, verbose=FALSE)

library(mgcv)
# Fit the model
mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = dat, method = "REML")

# Check the diagnostics
gam.check(mod)

# Refit to fix issues
mod2 <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 10) + s(x3, k = 3),
            data = dat, method = "REML")

# Check the new model
gam.check(mod2)


################## Examining overall concurvity in auto data ###################
#Let’s take a look at concurvity in the fuel efficiency model variables.

#Instructions

#Inspect the overall concurvity of mod, than answer the question.

library(gamair)
set.seed(0)
data("mpg", package="gamair", verbose=FALSE)

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

# Check overall concurvity
concurvity(mod, full = TRUE)

################## Examining concurvity between auto variables #################
#Now, let’s look at concurvity between model variables.

#Instructions

#Inspect the pairwise concurvity for variables in mod, then answer the question.

library(gamair)
set.seed(0)
data("mpg", package="gamair", verbose=FALSE)

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

# Check pairwise concurvity
concurvity(mod, full=FALSE)
