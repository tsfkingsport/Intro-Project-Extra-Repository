library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

full_rent_df <- read_csv("City_MedianRentalPrice_AllHomes.csv")

city_list <- c("San Francisco","Boston", "Chicago","Los Angeles", "Washington",
               "Seattle","Atlanta", "Austin", "Raleigh", "Durham", "Philadelphia",
               "Dallas", "Denver", "Detroit", "Minneapolis", "Saint Paul", "San Diego",
               "Houston")
#This creates a small tibble of median rent over 20 cities. There are 3 cities with 
#Washington in the title but luckily they are the last two rows in the tibble
rent_tibble <- full_rent_df %>% filter(RegionName  %in% city_list)
#removing last 2 rows in the tibble
rent_tibble <- rent_tibble[1:18, ]

#THIS ONE WORKS YAY!!!!
names(rent_tibble) <-gsub("X","", names(rent_tibble))

#THIS ONE WORKS, I got rid of the . in the column names and replaced them with
#a - to make it fit the ymd() function.  After that instead of doing the 
#overly complicated crap I was doing before I just typed in the names of 
#the columns and truncated = 3 to make it work. Without truncate only a handful
#of column names work and I don't know what is so special about those columns
#as opposed to the rest of the columns. THey all appear to be the same format to me
names(rent_tibble) <-gsub("\\.","-", names(rent_tibble))

#The next line breaks my graphs and the whole script for some reason, Trying something else now
#rent_tibble <- ymd(names(rent_tibble), truncated = 3)

#Now going to try gathering the data so that its more tidy using date as the key
#and rent as the value
Z <- rent_tibble[,-c(2:5)]
Z<- gather(Z, key = "date", value = "rent", - 1)
#I think I made it work I am just using Z and Y as placeholder variables before
#I clean everything up. 
rent_df<-Z[order(Z$RegionName),]

#changing date column from character to datetime, also looking at type and structure

rent_df$date <- ymd(rent_df$date, truncated = 2)
typeof(rent_df$date)
str(rent_df)



rent_df %>% ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()

write.csv(rent_df, "excluding_NYC_MedianRentalPrice.csv")


#Graphing the first few cities on their own
rent_df[c(1:540),] %>% ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()

#Decided that using the excel sheet to find out when the cities ended in the csv
#was simpler then trying to use which and graphing 2 cities at a time. 
rent_df[c(541:1080),] %>% ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()

rent_df[c(1081:1512),] %>% ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()

rent_df[c(1513:1944),] %>% ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()


#This does not work, Need to research further. Something about using multiple | for or statments.  
#I can use one of them to graph 2 cities but more then that it breaks down and I am not sure why
#I know that there is a difference between | and || but I am not sure what
#that difference is or what it means in practical terms. 

#rent_df[which(rent_df$RegionName =="Denver" || 
     #           rent_df$RegionName =="Detroit") || 
    #      rent_df$RegionName =="Durham"||
   #     rent_df$RegionName =="Houston"] %>% 
#  ggplot(aes(x = date, y = rent, colour = RegionName)) +
#  geom_point()



#Getting an alphabetic list of region names
unique(rent_df$RegionName)






str(rent_df$RegionName)
#Making linear models, starting with Atlanta
Atlanta_lm <- lm(formula = rent ~ date, rent_df, subset = rent_df$RegionName =="Atlanta")

#Found the issue.  I was not including the colum name in my subset for the linear model.  OOPS

rent_df[which(rent_df$RegionName =="Atlanta"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()


plot(Atlanta_lm) #The residuals are zero across the graph, that can't be right
summary(Atlanta_lm)

#Now I am doing data exploration for each city.  I am repeating the same command
#Over and over again but I want to be able to go back and easily bring up a graph
#of an individual city and look at them one at a time. I am sure there is a way
#to do this using purrr and creating calls to a function using a function
#that has one argument as the city, or cities, that I want to look at.  
#If I wanted to make an optimized R script I would do that but this is 
#more for simplicity then optimization. 


#Austin, strong linear increase
rent_df[which(rent_df$RegionName =="Austin"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()

#Boston, increase over time but mostly flat for multiple years
rent_df[which(rent_df$RegionName =="Boston"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()


#Chicago, drops near 2010, linear over time but has some flat lines and some drops

rent_df[which(rent_df$RegionName =="Chicago"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()

#Dallas, strong, fairly consistent linear increase
rent_df[which(rent_df$RegionName =="Dallas"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()


#Denver, general increase but a few periods of very slow increase, almost a decrease at times
rent_df[which(rent_df$RegionName =="Denver"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()


#Detroit, is a fucking basket case, sharp drop between 2010 and 2013, recent increase but 
#it has multiple flat lines for rent.
rent_df[which(rent_df$RegionName =="Detroit"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()

#Durham, decrease from 2010 to 2013, likely due to housing crisis but steady 
#increase afterwards
rent_df[which(rent_df$RegionName =="Durham"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()


#Houston, data starts at 2014 and has a sharp increase to 2016 then flattens
#a bit before rising again
rent_df[which(rent_df$RegionName =="Houston"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()


#Los Angeles, 2014 start again, mostly smooth and sharp linear increase with
#an elbow at 2016
rent_df[which(rent_df$RegionName =="Los Angeles"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth()

#Minneapolis, starts 2013, decrease to about 2015then flat line with a spike,
#the smooth line shows a general increase, the dots show more chaos
rent_df[which(rent_df$RegionName =="Minneapolis"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")

#Philadelphia, dots are wild up and down, line has decrease to 2015, increase
#to 2017 then slight drop with a larger grey area around smooth line
rent_df[which(rent_df$RegionName =="Philadelphia"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")

#Raleight, dots flat for different sections, line shows slow trend up with stagnation around 2010
rent_df[which(rent_df$RegionName =="Raleigh"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")

#Saint Paul, flatline around 2012 to 2014 then slow increase that picks up 
#pace around 2016.  Dots fairly smooth
rent_df[which(rent_df$RegionName =="Saint Paul"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")

#San Diego, harsh drop until 2013 then slow and steady increase. DOts wild
#at the beginning
rent_df[which(rent_df$RegionName =="San Diego"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")

#San Francisco, Very expensive, general in crease but flattened out around 2016
#with a brief drop in prices before normalizing 
rent_df[which(rent_df$RegionName =="San Francisco"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")

#Seattle, line is flat 2011 to 2013 and 2017 on. Between that smooth increase 
#dots have some variation not much
rent_df[which(rent_df$RegionName =="Seattle"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")

#Washington, sharp spike up around 2010 to 2013 and then smooth steady increase
#I don't think any other graph looks quite like that.  Interesting, did not
#expect an urban areas rent to increase so sharply during that time
rent_df[which(rent_df$RegionName =="Washington"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()+ geom_smooth(color = "blue")