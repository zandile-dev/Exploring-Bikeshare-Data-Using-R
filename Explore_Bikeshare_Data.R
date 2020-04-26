# Set working directory
setwd("~/Nanodegree/R/Project Dataset")

# activate packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(styler)

# Read in the data
chi <- read.csv("chicago.csv", header = TRUE)
nyc <- read.csv("new-york-city.csv", header = TRUE)
wash <- read.csv("washington.csv", header = TRUE)

# explore datasets
names(chi)
head(chi)
names(nyc)
head(nyc)
names(wash)
head(wash)

# Inspect the data types of the data sets
str(chi)
str(nyc)
str(wash)

# checking for nulls
is.null(chi)
is.null(nyc)
is.null(wash)



# Add a new column city name to distinguish different cities
chi_label <- data.frame(City_Name = rep("Chicago", nrow(chi)), chi[])
nyc_label <- data.frame(City_Name = rep("New York City", nrow(nyc)), nyc[])
wash_label <- data.frame(City_Name = rep("Washington", nrow(wash)), wash[])

# combine datasets#
data.combined <- bind_rows(chi_label, nyc_label, wash_label)

# explore dataset
summary(data.combined)
str(data.combined)

# checking for missing values
any(is.na(data.combined))
sum(is.na(data.combined))
colSums(is.na(data.combined))

#which User Types are more in the dataset?
table(data.combined$User.Type)
#The most users of the bikeshare sysem are subscriber(728824) followed by Customers(170483)

<<<<<<< HEAD
#Determine which city has the longest trip duration 
||||||| ee04261
#Determine which city has the the longest trip duration 
=======
#Determine which city has the the longest trip duration
>>>>>>> refactoring
by(data.combined$Trip.Duration,data.combined$City_Name, summary)
#the median and mean count for Washington is higher indicating that the average trip
#duration was higher in Washington

#Plot trip duration
ggplot(aes(x=Trip.Duration), data=data.combined) +
  geom_histogram(binwidth=30) +
  ggtitle('Histogram of Trip Durations') +
  labs(x = "Trip Duration") +
  scale_x_continuous(limits=c(0,1500))


#Plot a histogram of trip duration and facet by city_Name
ggplot(aes(x=Trip.Duration), data=data.combined) +
  geom_histogram(binwidth=30) +
  ggtitle('Histogram of Trip Durations') +
  labs(x = "Trip Duration") +
  scale_x_continuous(limits=c(0,1500)) +
 facet_wrap(~City_Name)

#determine popular trip days
as.Date(data.combined$Start.Time)

wday(data.combined$Start.Time, label = TRUE, abbr = FALSE)

data.combined %>%
  mutate(wday = wday(Start.Time, label = TRUE)) %>%
  ggplot(aes(x = wday, fill=wday)) +
  geom_bar()+
  labs(
    title = "Most common trip days",
    x = "Week_Day",
    y = "Count" ) +
  theme_minimal()

#from the data it can be seen that the most popular day of trips is Wednesday


#what is the distribution of bike rides per day?
datetime <- ymd_hms(data.combined$Start.Time)
hour(datetime) <- hour(datetime) + 1
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

a <-data.combined %>%
  mutate(Start.Time = update(datetime, yday = 1)) %>%
  ggplot(aes(Start.Time)) +
  geom_freqpoly(binwidth = 300)

a

#Facet by City
a+facet_wrap(~City_Name, nrow = 3)

#From the graph it can be seen that the most bike rides occur from 6 in the morning and again from 6pm


#what are the top ten most popular start and end station in each city?


#New York
#Extract start and stations
ny_start_station <- nyc['Start.Station']
ny_end_station <- nyc['End.Station']
head(ny_start_station)
head(ny_end_station)

#Combine the data
ny_station_combo <- cbind(ny_start_station,ny_end_station) #combine data into a single table
glimpse(ny_station_combo) #take a look to see if it is as we expect

#Let's tidy up our new table
ny_stations_no_duplicates <-  ny_station_combo %>%
  group_by(Start.Station, End.Station)  %>% #group data
  tally()  %>%  #this counts the number of times a certain combination appears in the list
  arrange(desc(n)) %>% #arrange the most frequently occurring combinations first
  glimpse() # take a look at our table

ny_stations_no_duplicates <- distinct(ny_stations_no_duplicates) #remove any duplicate data
head(ny_stations_no_duplicates) # take a look at our table
nrow(ny_stations_no_duplicates) #just to see if there was any data removed

#Let's create a table that contains only the top 10 station combinations
ny_top_ten <- ny_stations_no_duplicates[1:10,] #we want the first 10 rows and all the columns in our new table
head(ny_top_ten)

#Let's start constructing our bar chart
#We need to create a "key" to use as the input for the x-axis because the names of the station combinations are to long to display nicely on the graph
nrow(ny_top_ten)
nyc_key <- data.frame(c(seq(1,10,by=1))) #create an extra column that contains integers from 1 to 10
ny_top_ten <- bind_cols(nyc_key,ny_top_ten) #bind this new column to existing ones
glimpse(ny_top_ten)
head(ny_top_ten)

#There are a few random columns we need to delete
ny_top_ten <- select(ny_top_ten, -c(V1,V2,V3)) #remove columns labeled V1, V2, V3
head(ny_top_ten)

#Lets rename our columns to make it more readable
colnames(ny_top_ten)[1] <- "Key"
colnames(ny_top_ten)[2] <- "Start_Station"
colnames(ny_top_ten)[3] <- "End_Station"
colnames(ny_top_ten)[4] <- "Count"

#lets see how our table looks
head(ny_top_ten) #this looks great and like we expect

#We need to combine or unite the Start.Station and End.Station columns into a single column
# because we want our graph's legend to display the station names
ny_top_ten <- ny_top_ten %>%
  select(Key, Start_Station, End_Station, Count) %>%
  unite(Start_to_end_Station, c(Start_Station,End_Station))

#So the we are going to only use the "key" column and the "count column for our graph
ny_plot <- ggplot(data = ny_top_ten, aes(x = Key, y = Count, fill = Start_to_end_Station))+
  geom_bar(stat = "identity")+
  labs(
    title = "The 10 most popular station combinations for New York City",
    x = "Start and End Station Combination",
    y = "Count")+
  theme_bw()
ny_plot

#Washington
#Extract start and stations
wash_start_station <- wash['Start.Station']
wash_end_station <- wash['End.Station']
head(wash_start_station)
head(wash_end_station)

#Combine the data
wash_station_combo <- cbind(wash_start_station,wash_end_station) #combine data into a single table
glimpse(wash_station_combo) #take a look to see if it is as we expect

#Let's tidy up our new table
wash_stations_no_duplicates <-  wash_station_combo %>%
  group_by(Start.Station, End.Station)  %>% #group data
  tally()  %>%  #this counts the number of times a certain combination appears in the list
  arrange(desc(n)) %>% #arrange the most frequently occurring combinations first
  glimpse() # take a look at our table

wash_stations_no_duplicates <- distinct(wash_stations_no_duplicates) #remove any duplicate data
head(wash_stations_no_duplicates) # take a look at our table
nrow(wash_stations_no_duplicates) #just to see if there was any data removed

#Let's create a table that contains only the top 10 station combinations
wash_top_ten <- wash_stations_no_duplicates[1:10,] #we want the first 10 rows and all the columns in our new table
head(wash_top_ten)

#Let's start constructing our bar chart
#We need to create a "key" to use as the input for the x-axis because the names of the station combinations are to long to display nicely on the graph
nrow(wash_top_ten)
wash_key <- data.frame(c(seq(1,10,by=1))) #create an extra column that contains integers from 1 to 10
wash_top_ten <- bind_cols(wash_key,wash_top_ten) #bind this new column to existing ones
glimpse(wash_top_ten)
head(wash_top_ten)

#There are a few random columns we need to delete
wash_top_ten <- select(wash_top_ten, -c(V1,V2,V3)) #remove columns labeled V1, V2, V3
head(wash_top_ten)

#Lets rename our columns to make it more readable
colnames(wash_top_ten)[1] <- "Key"
colnames(wash_top_ten)[2] <- "Start_Station"
colnames(wash_top_ten)[3] <- "End_Station"
colnames(wash_top_ten)[4] <- "Count"

#lets see how our table looks
head(wash_top_ten) #this looks great and like we expect

#We need to combine or unite the Start.Station and End.Station columns into a single column
# because we want our graph's legend to display the station names
wash_top_ten <- wash_top_ten %>%
  select(Key, Start_Station, End_Station, Count) %>%
  unite(Start_to_end_Station, c(Start_Station,End_Station))

#So the we are going to only use the "key" column and the "count column for our graph
wash_plot <- ggplot(data = wash_top_ten, aes(x = Key, y = Count, fill = Start_to_end_Station))+
  geom_bar(stat = "identity")+
  labs(
    title = "The 10 most popular station combinations for Washington",
    x = "Start and End Station Combination",
    y = "Count")+
  theme_bw()
wash_plot


#Chicago
#Extract start and stations
chi_start_station <- chi['Start.Station']
chi_end_station <- chi['End.Station']
head(chi_start_station)
head(chi_end_station)

#Combine the data
chi_station_combo <- cbind(chi_start_station,chi_end_station)
glimpse(chi_station_combo)

#Let's tidy up our new table
chi_stations_no_duplicates <-  chi_station_combo %>%
  group_by(Start.Station, End.Station)  %>%
  tally()  %>%
  arrange(desc(n)) %>%
  glimpse()

chi_stations_no_duplicates <- distinct(chi_stations_no_duplicates)
head(chi_stations_no_duplicates)
nrow(chi_stations_no_duplicates)

#Let's create a table that contains only the top 10 station combinations
chi_top_ten <- chi_stations_no_duplicates[1:10,]
head(chi_top_ten)

#Let's start constructing our bar chart
nrow(chi_top_ten)
chi_key <- data.frame(c(seq(1,10,by=1)))
chi_top_ten <- bind_cols(chi_key,chi_top_ten)
glimpse(chi_top_ten)
head(chi_top_ten)

#There are a few random columns we need to delete
chi_top_ten <- select(chi_top_ten, -c(V1,V2,V3))
head(chi_top_ten)

#Lets rename our columns to make it more readable
colnames(chi_top_ten)[1] <- "Key"
colnames(chi_top_ten)[2] <- "Start_Station"
colnames(chi_top_ten)[3] <- "End_Station"
colnames(chi_top_ten)[4] <- "Count"

#lets see how our table looks
head(chi_top_ten)

#We need to combine or unite the Start.Station and End.Station columns into a single column
chi_top_ten <- chi_top_ten %>%
  select(Key, Start_Station, End_Station, Count) %>%
  unite(Start_to_end_Station, c(Start_Station,End_Station))

#So the we are going to only use the "key" column and the "count column for our graph
chi_plot <- ggplot(data = chi_top_ten, aes(x = Key, y = Count, fill = Start_to_end_Station))+
  geom_bar(stat = "identity")+
  labs(
    title = "The 10 most popular station combinations for Chicago",
    x = "Start and End Station Combination",
    y = "Count")+
  theme_bw()
<<<<<<< HEAD
chi_plot
||||||| ee04261
chi_plot
=======
chi_plot

#End of Code
>>>>>>> refactoring
