library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
#install.packages("xts")
#library(xts)
full_rent_df <- read.csv("City_MedianRentalPrice_AllHomes.csv", stringsAsFactors =FALSE)


new_york_rent <- full_rent_df %>% filter(grepl("New York", full_rent_df$Metro))
# The function below this line only returns one value...wtf?
#new_york_rent <- full_rent_df %>% filter(RegionName =="New York")

city_list <- c("San Francisco","Boston", "Chicago","Los Angeles", "Washington",
               "Seattle","Atlanta", "Austin", "Raleigh", "Durham", "Philadelphia",
               "Dallas", "Denver", "Detroit", "Minneapolis", "Saint Paul", "San Diego",
               "Houston")
#This creates a small tibble of median rent over 20 cities. There are 3 cities with 
#Washington in the title but luckily they are the last two rows in the tibble
rent_tibble <- full_rent_df %>% filter(RegionName  %in% city_list)
#removing last 2 rows in the tibble
rent_tibble <- rent_tibble[1:18, ]
#attaching the values associated with New York metro area
rent_tibble2 <- rent_tibble %>% rbind(new_york_rent)
#Now I want to figure out how to make the columns into date time with lubridate
#The one below isn't working out for me
#rent_tibble2 %>% select(which(substr(colnames(.), 1, 1)=="X"))
names(rent_tibble2) %>% substring((which(substr(colnames(.), 1, 1)=="X")), 2)

names(rent_tibble2) %>% substring((substr(colnames(.), 1, 1)=="X"), 2)




#transposing the data so the dates are the individual rows
rent_tibble3<- t(rent_tibble2)
#changing the column names to the first row of the new tibble, still now entirely sure
#what the difference between a dataframe and a tibble is. I'm sure I've seen it in a 
#datacamp course I just can't remember it
colnames(rent_tibble3) = rent_tibble3[1,]
#removing the first row due to it being redundant
rent_tibble3 <- rent_tibble3[-1,]
#removing the first four rows so all I am left with is numbers
rent_tibble4 <- rent_tibble3[-c(1,2,3,4), ]

head(rent_tibble4)
rent_df <- as.data.frame(rent_tibble4)
rent_df %>% glimpse()
#running into a lot of problems trying to graph the time series charts.  
ggplot(data = rent_df, aes(x = chicago)) + geom_point()
#Getting the error x must be a numberic vetor or matrix, going to try to amke a matrix
#and then see what happens
dotchart(x = rent_df)
#An attempt to make as.numeric not chop off zeroes
options(digits = 15)
#I can't figure out a way to get as.numeric to stop chopping off zeroes. It's rather
#frustrating. 
chicago_matrix <- as.numeric(rent_df$Chicago, digits = 8)
#This creates a plot but since as.numeric chops off a bunch of zeroes the graph does
#not work
plot(chicago_matrix)
plot(rent_df)
#Everything after this is attempting to make graphing a time series work. 
#I have yet to make a single fully functioning graph that represents what I want it to.
#plot.xts(rent_df)
chicago_no_na <- na.omit(rent_df[,1:18]) 
#plot.xts(chicago_no_na, y = chicago)
typeof(rent_df$Chicago)

#Got this from the Springboard forum after asking for help online
chicago_ts <-
  full_rent_df %>%
  filter(substr(RegionName, 1, 7) == "Chicago") %>% #Do I need to do the substr part or just region == "Chicago"?
  select(which(substr(colnames(.), 2, 3) == "20")) %>% # more advanced method of column selection then just bracket notation
  t(.) %>%   #Now that I look at what the instructor did I am not sure the transposing was necessary but I think it was to make the data clean
  as.data.frame() %>%
  rename(rent = names(.)[1]) %>% #Don't know why he did this or what it accomplishes or why
  mutate(time_qtr = row(.)) #This created another column that basically just has row numbers in it.  

chicago_lm <-lm(rent~time_qtr, data = chicago_ts)
plot(chicago_lm)
summary(chicago_lm)

ggplot(chicago_ts, aes(x = time_qtr, y = rent)) + geom_point()+
stat_smooth(method = "lm", col = "blue")
