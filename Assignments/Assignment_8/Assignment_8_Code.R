# This is the new RScript that I will be working in for Assignment 8 #
# After looking through the example problem I then went on to run the assignmnet tasks detailed in the assignment 8 document #
# The first thing I had to do was library all of the different packages I needed to peform the tasks #

library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

# Next I had to load in the mushroom dataset #

mushroom <- read.csv("../../Data/mushroom_growth.csv")

# glimpse allows me to see the data set that I just loaded in to the Rproject #

glimpse(mushroom)

# Next I had to explore the data and how it responds to various predictors which I did using ggplot #
# First I wanted to look at temperature #
# Then I decided to look at humidity # 
# Also decided to make one for light as the predictor#

ggplot(mushroom, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(mushroom, aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(mushroom, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# The next step outlined in the assignment was to generate 4 different models for the GrowthRate #
# The four I decided to go with are displayed below # 
# The first is a simple linear model comparing one predictor #
# The second was a multiple regression with another predictor #
# The third was a model including the interaction term between the two predictors #
# The final model is a complex model that combines multiple different predictors #

mod1 <- lm(GrowthRate ~ Temperature, data = mushroom)
mod2 <- lm(GrowthRate ~ Temperature + Humidity, data = mushroom)
mod3 <- lm(GrowthRate ~ Temperature * Humidity, data = mushroom)
mod4 <- lm(GrowthRate ~ Temperature + Humidity + Light + Nitrogen, data = mushroom)

# The next step was to calculate the mean squared error for each of the models # 
# The script below calculates a residual for each data point and then squares it #
# After squaring it the mean of all of that data is calculated #
# The c combines all of the calculated means #

c(
  MSE_Mod1 = mean(mod1$residuals^2),
  MSE_Mod2 = mean(mod2$residuals^2),
  MSE_Mod3 = mean(mod3$residuals^2),
  MSE_Mod4 = mean(mod4$residuals^2)
)

# I then had to select which of the models was the best and in my case it was model 4 #
# I decided to create an object called best_model to use as my object #

best_model <- mod4

# Then I needed to add predictions by using new hypothetical values # 
# This first script of code generates a new data frame with the hypothetical values #
#Then I generated predictions on that data using my best model #
# Then I created a data set that combined the hypothetiocal inputs and the predictions into one dataframe # 

newdf <- data.frame(
  Temperature = c(15, 20, 25, 30, 35),
  Humidity    = c("High", "Low", "Low", "High", "Low"),
  Light       = c(5, 10, 15, 20, 25),
  Nitrogen    = c(2, 4, 6, 8, 10)
)

hyp_preds <- predict(best_model, newdata = newdf)

hyp_df <- data.frame(
  newdf, 
  GrowthRate = hyp_preds, 
  PredictionType = "Hypothetical"
)

# The next step was to combine the two data sets #
# One thing I had to do first was to tag the actual data as real and then bind the two data frames #

orig_df <- mushroom
orig_df$PredictionType <- "Real"

fullpreds <- bind_rows(orig_df, hyp_df)

# The final script of code I had to write was to generate the predictions alongside the real data #
# I decided to do this using ggplot and ploitting the actual data points of temperature versus GrowthRate #
# Then also plotting the simulated growth rates which were calculated using my best fitting model #

ggplot(fullpreds, aes(x = Temperature, y = GrowthRate, color = PredictionType)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Real vs. Hypothetical Mushroom Growth Predictions",
    x = "Temperature",
    y = "Growth Rate"
  )

# The next step in the assignment was to answer the questions at the end #

# My answer to the first question is yes there can definiltey be meaningless response values especially because all of models were linear models. 
# Linear models assume that the data will follow along that linear line to demonstrate patterns. #
# However there are definitely constraints and certain interactions in the real world that do not fall along that linear model. #
# Sometimes these linear models fail to account for biological limits found in the real world. # 
# My model is in linear form and therefore depending on the values being generated especially in my example where the predicted values had temperatures not even in the actual data set. #
# These limitations can make some of the predicted response values meaningless. #

# My answer to question 2 would be that yes I did find multiple relatioinships that do not appear linearly. #
# More specifically the temperature and humidity graphs. #
# Both of these graphs when observed did not seem to have linear graphs produced and this is likely because of the natural limitations that are placed on temperature and humidity. #
# Here is a link to a solid website for dealing with nonlinear relationships https://www.geeksforgeeks.org/r-language/non-linear-regression-in-r/. # 

# The final step to this assignment was to generate code to model the data found in non_linear_ relationships with a linear model #
# This is the code I came up with #

non_linear_data <- read.csv("../../Data/non_linear_relationship.csv")
mod_nonlinear <- lm(response ~ predictor + I(predictor^2), data = non_linear_data)

# First I had to load in the data set #
# Then the I(x^2) allows the model to capture the curve #