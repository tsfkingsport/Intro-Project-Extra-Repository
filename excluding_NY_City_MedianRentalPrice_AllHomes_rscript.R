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

rent_df$date <- ymd(rent_df$date, truncated = 2)


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

str(rent_df$RegionName)
#Making linear models, starting with Atlanta
Atlanta_lm <- lm(formula = rent ~ date, rent_df, subset = rent_df =="Atlanta",
                 na.action = na.exclude)

rent_df[which(rent_df$RegionName =="Atlanta"),] %>% 
  ggplot(aes(x = date, y = rent, colour = RegionName)) +
  geom_point()


plot(Atlanta_lm) #The residuals are zero across the graph, that can't be right
summary(Atlanta_lm)








#rent_df[ c(1:3),] %>% ggplot(aes(x = date, y = rent, colour = RegionName)) +
#  geom_point()

#Trying to see how to convert my data into something other then a list.
#I think data.table has the answer I am looking for but I can explore that later

#Y <- as.data.frame(Y)
#typeof(Y)
