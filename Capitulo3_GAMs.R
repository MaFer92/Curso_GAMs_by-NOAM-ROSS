# Modelos aditivos generalizados (GAMs -- generalized additive models)
#GAMs in R by NOAM ROSS
#Carpeta del curso
setwd("C:/Users/Mafer/Documents/GAMs")

################################################################################
##                       Spatial GAMs and Interactions                        ##
################################################################################
#In this chapter, you will extend the types of models you can fit to those with
#interactions of multiple variables. You will fit models of geospatial data by 
#using these interactions to model complex surfaces, and visualize those surfaces 
#in 3D. Then you will learn about interactions between smooth and categorical 
#variables, and how to model interactions between very different variables like 
#space and time.

################## Modeling soil pollution in the Nertherlands #################

#Let’s take a closer look at the meuse data and use it to fit your first 2-D model.

#Instructions

#Inspect the meuse data with the head() and str() functions.

library(mgcv)
data(meuse, package="sp")

# Inspect the data
head(meuse)
str(meuse)

#Fit a GAM model to the data that predicts the concentration of cadmium in the 
#soil using an interaction of x and y coordinates.
#Inspect your model with the summary() and coef() functions.

library(mgcv)
data(meuse, package="sp")

# Fit the model
mod2d <- gam(cadmium ~ s(x, y), data = meuse, method = "REML")

# Inspect the model
summary(mod2d)
coef(mod2d)


############## Adding more variables to predict soil pollution #################

#Now let’s add additional predictors to the model with spatial interactions.

#Instructions 
#Fit another model to predict cadmium in the soil, this time including smooths 
#for the effect of elevation (elev) and distance from the river (dist) in addition
#to an x, y surface.

library(mgcv)
data(meuse, package="sp")

# Fit the model
mod2da <- gam(cadmium ~ s(x, y) + s(elev) + s(dist), 
              data = meuse, method = "REML")

# Inspect the model
summary(mod2da)

################ Plotting and interpreting GAM interactions ####################
plot(mod2d)
#3D
plot(mod2d, scheme = 1, col="purple")
#heatmap
plot(mod2d, scheme = 2)


########################### Plotting the model surface #########################
#Let’s explore the different visualization schemes available in mgcv’s plot() function.

#We’ll use the model you built in the last exercise (mod2da).

#Instructions

#Plot the interaction terms of mod2da as a contour plot.

library(mgcv)
data(meuse, package="sp")
mod2da <- gam(cadmium ~ s(x, y) + s(dist) + s(elev),
              data = meuse, method = "REML")

# Contour plot
plot(mod2da, pages = 1)

#Run the plot() function so interaction terms are displayed as 3D surfaces.

library(mgcv)
data(meuse, package="sp")
mod2da <- gam(cadmium ~ s(x, y) + s(dist) + s(elev),
              data = meuse, method = "REML")

# 3D surface plot
plot(mod2da, scheme = 1, pages = 1)

#Run the plot() function so interaction terms are displayed as colored heat maps
#on a sigle page.

library(mgcv)
data(meuse, package="sp")
mod2da <- gam(cadmium ~ s(x, y) + s(dist) + s(elev),
              data = meuse, method = "REML")

# Colored heat map
plot(mod2da, scheme = 2, pages = 1)

############################## Customizing 3D plots ############################
#Uncertainty is easy to see in plots of univariate smooths, but more challenging
#to represent in 2D relationships. Here we’ll visualize uncertainty in a geospatial
#interaction, using the model mod2d from exercise 2.

#Instructions

#Use vis.gam() to make a 3D perspective plot of the x, y relationship in the model,
#using the se argument to make confidence interval surfaces at +/- 2 standard errors.

data(meuse, package="sp")
library(mgcv)
mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Make the perspective plot with error surfaces
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp", se = 2)

#Now make another version of the same plot, rotated 135 degrees to view it from
#another angle.

data(meuse, package="sp")
library(mgcv)
mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Rotate the same plot
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "persp", se = 2, theta = 135)

######################### Extrapolation in surface plots #######################
#When making predictions from the models, it is important to understand how far
#from the range of your data you are extrapolating. With multivariate smooths,
#the shape of the areas supported by data may be complex. Here you’ll make plots
#that compare extrapolations to support in the data.

#One again we’ll use mod2d from exercise 2.

#Instructions 
#Make a contour plot of the model using vis.gam(), extrapolating out from the data 5%.

data(meuse, package="sp")
library(mgcv)
mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Make plot with 5% extrapolation
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.05)

# Overlay data
points(meuse)

#Make a contour plot of the model using vis.gam(), extrapolating out from the data 10%.

data(meuse, package="sp")
library(mgcv)
mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Make plot with 10% extrapolation
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "contour", too.far = 0.1)

# Overlay data
points(meuse)

#Make a contour plot of the model using vis.gam(), extrapolating out from the data 25%.
#Overlay the meuse data on top of your visualization as points.

data(meuse, package="sp")
library(mgcv)
mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Make plot with 25% extrapolation
vis.gam(mod2d, view = c("x", "y"),
        plot.type = "contour", too.far = 0.25)

# Overlay data
points(meuse)


################## Soil pollution in different land uses #######################
#The meuse data set has a factor variable, landuse, which specifies the type of 
#land use or cover at the location where soil was sampled.

#Instructions

#Using a categorical-continuous interaction (e.g., the by = form), fit a model to
#the meuse data that predicts copper levels as a function of dist, with different
#smooths for each level of landuse.
#Include a separate term for varying intercepts for each level of landuse.
#Print the model summary.

library(mgcv)
data(meuse, package="sp")

# Fit a model with separate smooths for each land-use level
mod_sep <- gam(copper ~ s(dist, by = landuse) + landuse,
               data = meuse, method = "REML")
# Examine the summary
summary(mod_sep)

#Instructions

#Fit a model with a factor-smooth interaction between dist and landuse variables 
#using the bs = "fs" formulation.
#Print the model summary.

library(mgcv)
data(meuse, package="sp")

# Fit a model with factor-smooth interaction
mod_fs <- gam(copper ~ s(dist, landuse, bs = "fs"),
              data = meuse, method = "REML")

# Examine the summary
summary(mod_fs)

##################### Plotting pollution in different land uses ################
#You can observe the differences between different continuous-categorical interaction
#types by visualizing them. Here, you’ll look at the different ways the by-type 
#and factor-smooth-type interactions are plotted, an see how the approaches fit
#the models differently.

#Both the models (mod_sep and mod_fs) from the previous exercise are available 
#in your workspace.

#Instructions

#Plot both models using the plot() function, using the pages argument to keep 
#all terms on one plot.

data(meuse, package="sp")
library(mgcv)
mod_sep <- gam(copper ~ s(dist, by=landuse), data=meuse, method = "REML")
mod_fs <- gam(copper ~ s(dist, landuse, bs="fs"), data=meuse, method = "REML")

# Plot both the models with plot()
plot(mod_sep, pages = 1)
plot(mod_fs, pages = 1)

#Plot both models making 3D perspective plots with vis.gam().
data(meuse, package="sp")
library(mgcv)
mod_sep <- gam(copper ~ s(dist, by=landuse), data=meuse, method = "REML")
mod_fs <- gam(copper ~ s(dist, landuse, bs="fs"), data=meuse, method = "REML")

# Plot both the models with vis.gam()
vis.gam(mod_sep, view = c("dist", "landuse"), plot.type = "persp")
vis.gam(mod_fs, view = c("dist", "landuse"), plot.type = "persp")

################ Pollution models with multi-scale interactions ################
#The meuse dataset contains some predictor variables that are on the same scale
#(x, y), and some that are on different scales (elev, dist, om). In a previous 
#exercise, you fit a model where you predicted cadmium pollution as a function of 
#location and elevation:

#mod <- gam(cadmium ~ s(x, y) + s(elev),data = meuse, method = "REML")

#In this exercise, you’ll build a model that allows multiple variables to interact 
#despite these different scales using a tensor smooth, te().

#Instructions

#Convert this to a model where x, y, and elev all interact in a single te() term, 
#varying on their own scales.
#Then summarize the model and visualize it with plot().

library(mgcv)
data(meuse, package="sp")

# Fit the model
tensor_mod <- gam(cadmium ~ te(x, y, elev), 
                  data = meuse, method = "REML")

# Summarize and plot
summary(tensor_mod)
plot(tensor_mod, pages = 1)

#################### Teasing apart multi-scale interactions ####################
#In the previous exercise, you fit the following model:
  
#tensor_mod <- gam(cadmium ~ te(x, y, elev),data = meuse, method = "REML")

#In this exercise, you will fit a model with smooths and tensor interactions to 
#separate out the independent and interacting effects of variables.

#Instructions

#Convert the above model such that x and y interact on the same scale, the effect
#elev is a separate smooth, and the interaction of all three on different scales
#is a separate term.
#Summarize and plot the model.

library(mgcv)
data(meuse, package="sp")

# Fit the model
tensor_mod2 <- gam(cadmium ~ s(x, y) + s(elev) + ti(x, y, elev), 
                   data = meuse, method = "REML")
# Summarize and plot
summary(tensor_mod2)
plot(tensor_mod2, pages = 1)
