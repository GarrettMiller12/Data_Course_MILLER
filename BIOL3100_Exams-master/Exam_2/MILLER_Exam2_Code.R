# I would first want to load in tidyverse, ggplot, and modelr # 

library(tidyverse)
library(gglot2)
library(modelr)

# Then I loaded in the data set csv file #

unicef_data <- read_csv("unicef-u5mr.csv")

# This command allows me to look at the data to see how I may need to edit it to make it in tidy format for plotting #

glimpse(unicef_data)

# I then needed to get to the portion of tidying up the data and the first thing I decided to do was use pivot longer to translate the wide data into long data #
# This means instead of there being a whole bunch of columns with a different value in each, it takes all of those values and places them in a column for the year and one for the u5mr values#
# The names_prefix = U5MR removes the u5mr text before the year value so that the resulting number in the year column are solely all of the years in their number form #

tidy_unicef <- unicef_data %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "year",
    names_prefix = "U5MR.", 
    values_to = "u5mr"
  )

# Then I needed to mutate the data so that the year was a numeric value and ensure the u5mr values were numeric as well # 

tidy_unicef <- tidy_unicef %>%
  mutate(
    year = as.numeric(year),           
    u5mr = as.numeric(u5mr)            
  )

# Now that the data has been turned ot tidy form it is ready to be plotted using ggplot #
# ggplot creates the image and I dictate which data set to use with tidy_unicef and each line is grouped together by country name #
# I then set the aes with year being the x axis and the u5mr value being the y axis # \
# geomline plots lines for each of the graphs with the color being specified as black #
# Facet wrap createes the five different graphs based on which continent the country is a part of #
# The x and y axis are labeled based upon what values they are representing # 

ggplot(tidy_unicef, aes(x = year, y = u5mr, group = CountryName)) +
  geom_line(color = "black", linewidth = 0.5) +
  facet_wrap(~ Continent) +
  theme_bw() +
  labs(
    x = "Year",
    y = "U5MR"
  )

# I decided to add code for saving the image with the correct title #

ggsave("MILLER_Plot_1.png", width = 8, height = 5, dpi = 300)

# After saving that image with the correct label I moved onto the next plot in the assignment #
# I first had to calculate the mean u5mr for each continent for each year #
# Group_by tells R to group by the continent and the year #
# Then summarize tells R to calculate the mean for the u5mr and .groups removes any of the grouping this calculate may perform #

continent_means <- tidy_unicef %>%
  group_by(Continent, year) %>%
  summarize(Mean_U5MR = mean(u5mr, na.rm = TRUE), .groups = "drop")

# Now that R has calculated that we can use ggplot to create the plot of these values #
# Using aes for the year on the x and the mean u5mr on the y with color separated for each continent #
# Also included a geom_line with a little bit wider linewidth for this graph #
# Labels are standard and accurate for the graph being produced #

ggplot(continent_means, aes(x = year, y = Mean_U5MR, color = Continent)) +
  geom_line(linewidth = 1.2) + 
  theme_bw() +
  labs(
    x= "Year",
    y = "Mean_U5MR",
    color = "Continent"
  )

# I added code for saving this image too with the correct label #

ggsave("MILLER_Plot_2.png", width = 8, height = 5, dpi = 300)

# After saving that plot with the correct name it was then time to create the three models #
# The first model only accounted for the year #
# To do this I used the lm script with fits a simpler linear regression with only using the year as a predictor of cjild mortality rates #

mod1 <- lm(u5mr ~ year, data = tidy_unicef)

# I then created mod 2 using the same idea but accounting for year and continent #
# The + sign between year and continent tells R to use both predictors side by side # 

mod2 <- lm(u5mr ~ year + Continent, data = tidy_unicef)

# I then created the third and final model using the same lm script but this time multiplying year and Continent #
# The * tells R to account for each of the effects of year and Continent, but also their interaction term # 

mod3 <- lm(u5mr ~ year * Continent, data = tidy_unicef)

# Now that all the models are built I need to compare them to one another #
# To do this I used the anova function which tests the groups against one another for statistical differences or random ones #
# The best overall model in this format will want to have a small p value #
# Looking at the results the comparison from mod1 to mod2 shows a great increase in reduction of random noise #
# However from mod2 to mod3 there is even more reduction witha tiny p-value which means we reduced variance in a statistically significant way #
# Because of this I would rank mod3 as the best model because it accounts for the most variation, while maintaining that the variation is significant to the findings of the model #

anova(mod1, mod2, mod3)

# After comparing them I needed to graph all three of them together #
# To do this I first need to gather the predictions from each of the models into a data grid #

models_grid <- tidy_unicef %>%
  data_grid(year, Continent, CountryName) %>%
  gather_predictions(mod1, mod2, mod3)

# Then it was time to plot all 3 #
# I used very similar code to the other plots only changing the data being used, how they were faceted, the x and y axis', and how they were grouped #
# The final code looked like this #

ggplot(models_grid, aes(x = year, y = pred, color = Continent)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model) +
  theme_bw() +
  labs(
    title = "Model predictions",
    x = "Year",
    y = "Predicted U5MR",
    color = "Continent"
  )

# I decided to save this image as well and just labeled it as plot 3 #

ggsave("MILLER_Plot_3.png", width = 8, height = 5, dpi = 300)

# I decided not to do the extra portion of this exam so this is the last step #
