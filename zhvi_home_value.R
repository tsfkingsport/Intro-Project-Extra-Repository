#This will be the Zillow Home value script and dataset

library(dplyr)
library(ggplot2)
library(tidyverse)

home_values <- read_csv("Metro_Zhvi_AllHomes.csv", stringsAsFactors = FALSE)

#So this file also uses a completely different method of naming its cities,
#It is under RegionName and its the city followed by state acronym, so New York, NY
#This means I have to redo the city list to add the states
city_list <- c("San Francisco, CA","Boston, MA", "Chicago, IL",
               "Los Angeles-Long Beach-Anaheim, CA",
               "Washington, DC",
               "Seattle, WA","Atlanta, GA", "Austin, TX", "Raleigh, NC",
               "Durham, NC", "Philadelphia, PA",
               "Dallas-Fort Worth, TX", "Denver, CO", 
               "Detroit, MI", "Minneapolis-St Paul, MN", "San Diego",
               "Houston, TX", "New York, NY")