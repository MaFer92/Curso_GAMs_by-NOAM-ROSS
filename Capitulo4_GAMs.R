# Modelos aditivos generalizados (GAMs -- generalized additive models)
#GAMs in R by NOAM ROSS
#Carpeta del curso
setwd("C:/Users/Mafer/Documents/GAMs")

################################################################################
##                    Logistic GAMs for Classification                        ##
################################################################################
#In the first three chapters, you used GAMs for regression of continuous outcomes.
#In this chapter, you will use GAMs for classification. You will build logistic
#GAMs to predict binary outcomes like customer purchasing behavior, learn to 
#visualize this new type of model, make predictions, and learn how to explain the
#variables that influence each prediction.

######################### Classifying purchasing behavior ######################
#Let’s fit some GAMs to the csale data, which has the binary purchase outcome variable.

#After you fit the model, answer the following question:
  
#What does the log_mod model estimate the probability of a person making a purchase 
#who has mean values of all variables?
  
#Instructions

#Fit a logistic GAM predicting whether a purchase will be made based only on a 
#smooth of the mortgage_age variable.

library(mgcv)
csale <- readRDS("csale.rds")

# Examine the csale data frame
head(csale)
str(csale)

# Fit a logistic model
log_mod <- gam(purchase ~ s(mortgage_age), data = csale,
               family = binomial,
               method = "REML")

# Calculate the probability at the mean
plogis(coef(log_mod)[1])


###################### Purchase behavior with multiple smooths #################
#In this exercise, you will fit a logistic GAM that predicts the outcome (purchase) 
#in csale, using all other variables as predictors.

#Instructions

#Fit a logistic GAM on all variables.
#Print the summary of the model.

csale <- readRDS("csale.rds")
library(mgcv)

# Fit a logistic model
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) +
                  s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# View the summary
summary(log_mod2)

############### Visualizing influences on purchase probability #################
#Let’s try plotting and interpreting the purchasing behavior model from the last 
#lesson. You’ll step through several iterations of plots of log_mod2, moving from 
#raw plots on the fitting scale towards plots on the response scale with more 
#natural interpretations.

#Instructions

#Plot all partial effects of log_mod2 on the log-odds scale. Put all plots on a 
#single page.

csale <- readRDS("csale.rds")
set.seed(0)
library(mgcv)
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Plot on the log-odds scale
plot(log_mod2, pages = 1)

#Instructions

#Convert the plots to the probability scale using the trans argument.

csale <- readRDS("csale.rds")
set.seed(0)
library(mgcv)
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Plot on the probability scale
plot(log_mod2, pages = 1, trans = plogis)

#Instructions

#Convert the plots to probability centered on the intercept with the shift argument.
csale <- readRDS("csale.rds")
set.seed(0)
library(mgcv)
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Plot with the intercept
plot(log_mod2, pages = 1, trans = plogis,
     shift = coef(log_mod2)[1])

#Instructions

#Add intercept-related uncertainty to the plots using the seWithMean argument.

csale <- readRDS("csale.rds")
set.seed(0)
library(mgcv)
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Plot with intercept uncertainty
plot(log_mod2, pages = 1, trans = plogis,
     shift = coef(log_mod2)[1], seWithMean = TRUE)

################## Predicting purchase behavior and uncertainty ################
#The log_mod2 purchase behavior model lets you make predictions off credit data. 
#In this exercise, you’ll use a new set of data, new_credit_data, and calculate 
#predicted outcomes and confidence bounds.

#Instructions

#Use the model log_mod2 to calculate the predicted purchase log-odds, and 
#standard errors for those predictions, for the observations in new_credit_data.

library(mgcv)
csale <- readRDS("csale.rds")
new_credit_data <- readRDS("new_credit_data.rds")
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Calculate predictions and errors
predictions <- predict(log_mod2, newdata = new_credit_data,
                       type = "link", se.fit = TRUE)

# Inspect the predictions
predictions

#Instructions
#Using your predictions and standard errors, calculate high and low confidence
#bounds for the log-odds of purchase for each observation.
#Calculate the high confidence bound as two standard errors above the mean 
#prediction, and the low confidence bound as two standard errors below the mean
#prediction.

library(mgcv)
csale <- readRDS("csale.rds")
new_credit_data <- readRDS("new_credit_data.rds")
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Calculate predictions and errors
predictions <- predict(log_mod2, newdata = new_credit_data,
                       type = "link", se.fit=TRUE)

# Calculate high and low predictions intervals
high_pred <- predictions$fit + 2*predictions$se.fit
low_pred <- predictions$fit - 2*predictions$se.fit


#Instructions
#Convert your calculated confidence bounds from the log-odds scale to the
#probability scale.

library(mgcv)
csale <- readRDS("csale.rds")
new_credit_data <- readRDS("new_credit_data.rds")
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Calculate predictions and errors
predictions <- predict(log_mod2, newdata = new_credit_data,
                       type = "link", se.fit=TRUE)

# Calculate high and low predictions intervals
high_pred <- predictions$fit + 2*predictions$se.fit
low_pred <- predictions$fit - 2*predictions$se.fit

# Convert intervals to probability scale
high_prob <- plogis(high_pred)
low_prob <- plogis(low_pred)

high_prob
low_prob

######################### Explaining individual behaviors ######################
#In the final exercise of this chapter, you will use the model log_mod2 to predict
#the contribution of each term to the prediction of one row in new_credit_data, 
#then use this to determine which variable has the greatest influence on an output.

#Instructions

#Predict the effect of each model term on the output for the first row of 
#new_credit_data.

library(mgcv)
csale <- readRDS("csale.rds")
new_credit_data <- readRDS("new_credit_data.rds")
log_mod2 <- gam(purchase ~ s(n_acts) + s(bal_crdt_ratio) +
                  s(avg_prem_balance) + s(retail_crdt_ratio) +
                  s(avg_fin_balance)  + s(mortgage_age) + s(cred_limit),
                data = csale,
                family = binomial,
                method = "REML")

# Predict from the model
prediction_1 <- predict(log_mod2,
                        newdata = new_credit_data[1, ],
                        type = "terms")

# Inspect
prediction_1

