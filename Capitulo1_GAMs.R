# Modelos aditivos generalizados (GAMs -- generalized additive models)
#GAMs in R by NOAM ROSS
#Carpeta del curso
setwd("C:/Users/Mafer/Documents/GAMs")

################################################################################
##                Introduction to Generalized Additive Models                 ##
################################################################################

#In this chapter, you will learn how Generalized additive models work and how to
#use flexible, nonlinear functions to model data without over-fitting. You will 
#learn to use the gam() function in the mgcv package, and how to build multivariate
#models that mix nonlinear, linear, and categorical effects to data.


################# Motorcycle crash data: linear approach #######################

#In this first exercise, you will fit a linear model to a data set and visualize
#the results to see how well it captures relationships in the data. The data set,
#stored in a data frame named mcycle, contains measurement of acceleration of a
#crash-test dummy head during a motorcycle crash. It contains measurements of 
#acceleration (g) in the accel column and time (milliseconds) in the times column.

#Instructions

#Use the head() and plot() functions to look at the data frame named mcycle.
#Use the lm() function to fit a model to the mcycle data where the accel variable 
#is a linear function of times.
#Visualize the model fit using the provided call to termplot().

mcycle <- MASS::mcycle

#paso 1
head(mcycle)
plot(mcycle)

#paso 2
lm_mod = lm(accel ~ times, data = mcycle)

#paso 3
termplot(lm_mod, partial.resid = TRUE, se = TRUE)


################ Motorcycle crash data: non-linear approach ####################

#Now you will fit a non-linear model to the same mcycle data using the gam() 
#function from the mgcv package.

#Instructions

#Load the mgcv package.
#Fit a model to the mcycle data where accel has a smooth, nonlinear relation to
#times using the gam() function.
#Visualize the model fit using the provided call to plot().

#paso 1
library(mgcv)

#paso 2
gam_mod = gam(accel ~ s(times), data = mcycle)

#paso 3
plot(gam_mod, residuals = TRUE, pch=1)

########################## Parts of non-linear function ########################

#GAMs are made up of basis functions that together compose the smooth terms in 
#models. The coef() function extracts the coefficients of these basis functions 
#the GAM model object.

#Instructions

#Extract the model coefficients of the gam_mod object which is available in your
#workspace. Then determine how many basis functions compose the smooth curve of 
#the mode.

coef(gam_mod)

####################### Basis Functions and Smoothing ##########################
#Smoothing Syntax

#Setting a fixed smoothing parameter
gam(y ~ s(x), data = dat, sp = 0.1)
gam(y ~ s(x, sp = 0.1), data = dat)

#Smoothing via restricted maximum likelihood
gam(y ~ s(x), data = dat, method = "REML") #"Restricted Maximum Likelihood" method


#Setting number of basis functions
gam(y ~ s(x, k = 3), data = dat, method = "REML")
gam(y ~ s(x, k = 10), data = dat, method = "REML")

#Use the defaults
gam(y ~ s(x), data = dat, method = "REML")


#################### Setting complexity of the motorcycle model ################
#The number of basis functions in a smooth has a great impact on the shapes a 
#model can take. Here, you’ll practice modifying the number of basis functions 
#in a model and examining the results.

#Instructions

#Fit a GAM with 3 basis functions to the mcycle data, with accel as a smooth 
#function of times.
#Fit the same GAM again, but this time with 20 basis functions.
#Use the provided plot() functions to visualize both models.

mcycle <- MASS::mcycle

library(mgcv)

# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data=mcycle)

# Visualize the GAMs
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

################ Using smoothing parameters to avoid overfitting ###############
#The smoothing parameter balances between likelihood and wiggliness to optimize 
#model fit. Here, you’ll examine smoothing parameters and will fit models with 
#different fixed smoothing parameters.

#Instructions

#View the value of the smoothing parameter (λ) of the provided gam_mod model by 
#extracting the sp value from the model.
#Fit two models to the mcycle data with accel as a smooth function of times and
#a smoothing parameter of:
#0.1
#0.0001
#Visualize both models.

mcycle <- MASS::mcycle

library(mgcv)

# Extract the smoothing parameter
gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
gam_mod$sp

# Fix the smoothing parameter at 0.1
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models
par(mfrow = c(2,1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)


###################### Complexity and smoothing together #######################
#The number of basis functions and the smoothing parameters interact to control
#the wiggliness of a smooth function. Here you will see how changing both together
#affects model behavior.

#Instructions

#Fit a GAM models to the mcycle data with accel as a smooth function of times 
#with 50 basis functions and a smoothing parameter of 0.0001.
#Visualize the model.

mcycle <- MASS::mcycle

library(mgcv)

# Fit the GAM
gam_mod_sk <- gam(accel ~ s(times, k=50), sp = 0.0001, data=mcycle)

# Visualize the model
plot(gam_mod_sk, residuals = TRUE, pch = 1)

####################### Multiple Regression with GAMs ##########################
library(gamair)
data("mpg", package="gamair")


#Multiple Smooths (1)
model <- gam(hw.mpg ~ s(weight), data = mpg,
             method = "REML")
plot(model, residuals = TRUE, pch = 1)

#Multiple Smooths (2)
model <- gam(hw.mpg ~ s(weight), data = mpg,
             method = "REML")
model2 <- gam(hw.mpg ~ s(weight) + s(length), data = mpg,
              method = "REML")
plot(model2, residuals = TRUE, pch = 1)
#Categorical Terms (1)
model3 <- gam(hw.mpg ~ s(weight) + fuel, data = mpg,
              method = "REML")
plot(model3, residuals = TRUE, pch = 1)
#Categorical Terms (2)
model4 <- gam(hw.mpg ~ s(weight, by = fuel), data = mpg,
              method = "REML")
plot(model4, residuals = TRUE, pch = 1)
#Categorical Terms (3)
model4b <- gam(hw.mpg ~ s(weight, by = fuel) + fuel, data = mpg,
               method = "REML")
plot(model4b, residuals = TRUE, pch = 1)

##################### Multivariate GAMs of auto performance ####################
#GAMs can accept multiple variables of different types. In the following exercises,
#you’ll work with the mpg dataset available in the gamair package to practice 
#fitting models of different forms.

#Instructions

#Use the head() and str() functions to examine the mpg data set.
#Fit a GAM to these data to predict city.mpg as the sum of smooth functions of 
#weight, length, and price.
#Use the plot() function provided to visualize the model.

library(gamair)
data("mpg", package="gamair")

library(mgcv)

#paso 1
head(mpg)
str(mpg)

#paso 2
mod_city = gam(city.mpg ~ s(weight) + s(length) + s(price), data = mpg, method = "REML")

#paso 3
plot(mod_city, pages = 1)


############## Adding categorical to the auto performance model ################
#Now you’ll include categorical variables in your model. Categories are inherently
#linear, so you’ll model them as linear terms.

#Instructions

#Fit a GAM to the mpg data, modeling city.mpg as a sum of smooth functions of 
#weight, length, and price, and also include the categorical terms fuel, drive,
#and style.
#Use the plot() function provided to visualize the model.

library(gamair)
data("mpg", package="gamair")

library(mgcv)

# Fit the model
mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style, 
                 data = mpg, method = "REML")

# Plot the model
plot(mod_city2, all.terms = TRUE, pages = 1)


############### Category-level smooths for different auto types ################
#Now you extend your models to include different smooths for different levels of
#categorical terms.

#Instructions

#Fit a model to predict city fuel efficiency (city.mpg) with smooth terms of 
#weight, length, and price, but make each of these smooth terms depend on the 
#drive categorical variable using by= in the smooth terms.
#Include a separate linear term for the drive variable.

library(gamair)
data("mpg", package="gamair")

library(mgcv)

# Fit the model
mod_city3 <- gam(city.mpg ~ s(weight, by = drive) + s(length, by=drive) + 
                   s(price, by = drive) + drive, 
                 data = mpg, method = "REML")

# Plot the model
plot(mod_city3, pages = 1)

