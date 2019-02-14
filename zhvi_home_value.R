#This will be the Zillow Home value script and dataset

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

home_values <- read_csv("Metro_Zhvi_AllHomes.csv")

#So this file also uses a completely different method of naming its cities,
#It is under RegionName and its the city followed by state acronym, so New York, NY
#This means I have to redo the city list to add the states

#In this set Minneapolis and St Paul are considered the same for house value
#But Minneapolis and St Paul are seperate for the rent data... I really would
#love to know how this made sense to the data scientists at Zillow.  I know they 
#have some well paid data professionals so I am sure there is a completely logical
#reason for this however this does seem highly counter-intuitive. 
city_list <- c("San Francisco, CA","Boston, MA", "Chicago, IL",
               "Los Angeles-Long Beach-Anaheim, CA",
               "Washington, DC",
               "Seattle, WA","Atlanta, GA", 
               "Austin, TX", "Raleigh, NC",
               "Durham, NC", "Philadelphia, PA",
               "Dallas-Fort Worth, TX", "Denver, CO", 
               "Detroit, MI", "Minneapolis-St Paul, MN", "San Diego, CA",
               "Houston, TX", "New York, NY")

filtered_home_values <- home_values %>% filter(RegionName %in% city_list)

#removing columns 1 and 3 because they are not what I need
filtered_home_values <- filtered_home_values[,-c(1,3)]
#gathering data into a tidy format
tidy_filtered_home_values <-filtered_home_values %>% 
                          gather(key = date, value = rent, - 1)
#Ordering by city
tidy_filtered_home_values <- tidy_filtered_home_values[order(tidy_filtered_home_values$RegionName),]

#visualizing all cities
tidy_filtered_home_values %>% ggplot(aes(x = date, y = rent, color = RegionName)) + 
                              geom_point()