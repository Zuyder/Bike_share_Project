install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(skimr)
setwd("C:/Users/opiso/Desktop/All Data")
getwd()

## STEP 1 : Loading the CSV files 

df1 <- read.csv("C:/Users/opiso/Desktop/All Data/202006-divvy-tripdata.csv")
df2 <- read.csv("C:/Users/opiso/Desktop/All Data/202007-divvy-tripdata.csv")
df3 <- read.csv("C:/Users/opiso/Desktop/All Data/202008-divvy-tripdata.csv")
df4 <- read.csv("C:/Users/opiso/Desktop/All Data/202009-divvy-tripdata.csv")
df5 <- read.csv("C:/Users/opiso/Desktop/All Data/202010-divvy-tripdata.csv")
df6 <- read.csv("C:/Users/opiso/Desktop/All Data/202011-divvy-tripdata.csv")
df7 <- read.csv("C:/Users/opiso/Desktop/All Data/202012-divvy-tripdata.csv")
df8 <- read.csv("C:/Users/opiso/Desktop/All Data/202101-divvy-tripdata.csv")
df9 <- read.csv("C:/Users/opiso/Desktop/All Data/202102-divvy-tripdata.csv")
df10 <- read.csv("C:/Users/opiso/Desktop/All Data/202103-divvy-tripdata.csv")
df11 <- read.csv("C:/Users/opiso/Desktop/All Data/202104-divvy-tripdata.csv")
df12 <- read.csv("C:/Users/opiso/Desktop/All Data/202105-divvy-tripdata.csv")

##  STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

all_trips <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
View(all_trips)
str(all_trips)

# Inspection of the data set
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
summary(all_trips)
## removing lat and long columns
all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))
View(all_trips)
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

# We then add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Adding a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
# Inspect the structure of the columns
str(all_trips)
# Converting "ride_length" from Factor to numeric for easier analysis
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
# Removing "bad" data then creating a new data frame since data was removed.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#Step 4 : Descriptive analysis
## to check the distribution of riders
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarize(count = length(member_casual), '%Rides' = (length(member_casual) / nrow(all_trips_v2)) * 100 )
## to plot the distribution of riders
all_trips %>%
  ggplot(aes(member_casual, fill = member_casual)) + 
  geom_bar() + labs(x= "User type", y= "No of rides", 
                    title = "Ride distribution by user Type")
## To get the mean,median,max,max and First and Third quartiles
summary(all_trips_v2$ride_length)
# Compare a few measures of central tendencies for members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

View(all_trips_v2)
## Distribution of users
# See the average ride time by each day for members vs casual users

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()						
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)	
##To plot
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

## Ride distribution by month

all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(),
            .groups = "drop")
all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_ride = n(),
            .groups = "drop")%>%
  ggplot(aes(month, number_of_ride, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "The number of rides by month",
       x = "Month", 
       y = "Number of rides")

### Ride distribution by day of week

all_trips_v2 %>%
  group_by(member_casual, day_of_week)%>%
  summarise(number_of_ride = n(),
            .groups = "drop")%>%
  ggplot(aes(day_of_week, number_of_ride, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides by Weekday", x = "Weekday", y = "Number of rides")

### Ride distribution by time of day

all_trips_v2 %>% 
  mutate(hour = hour(started_at)) %>%
  group_by(member_casual, hour) %>%
  summarise(no_of_rides = n(),
            .groups = "drop") %>%
  ggplot(aes(hour, no_of_rides, color = member_casual)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::label_number_si(),
                     breaks = seq(0, 300000, 25000)) +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  labs(title = "Number of rides by hour of the day", 
       x = "Hour of day", y = "No of rides")

### More on ride length data
all_trips_v2 %>% 
  group_by(member_casual, month) %>%
  summarize(mean = mean(ride_length),
            .groups = "drop") %>%
  ggplot(aes(month, mean, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = seq(0, 28, 2)) +
  labs(title = "The average ride length by month", 
       x = "Month", y = "Average (min)")

### Bike use analysis
all_trips_v2 %>% 
  group_by(rideable_type) %>%
  summarise(number_of_rides = n())
#the viz
all_trips_v2 %>% 
  group_by(rideable_type) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(rideable_type, number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_y_continuous(labels = scales::label_number_si(),
                     breaks = seq(0, 3000000, 1000000)) +
  labs(title = "Utilization of bikes", 
       x = "Rideable type",
       y = "Bike usage")

### Bike usage by user category
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(),
            .groups = "drop") %>%
  arrange(rideable_type)
#viz
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(),
            .groups = "drop") %>%
  ggplot(aes(member_casual, number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "Bike usages by member status", 
       x = "Member Status", 
       y = "Bike usages")

# bike type and average ride length

all_trips_v2 %>% 
  group_by(rideable_type) %>%
  summarise(mean = mean(ride_length),
            .groups = "drop") %>%
  arrange(rideable_type)
# to viz
all_trips_v2 %>% 
  group_by(rideable_type) %>%
  summarise(mean = mean(ride_length),
            .groups = "drop") %>%
  ggplot(aes(rideable_type, mean)) +
  geom_col(position = "dodge") +
  labs(title = "Average ride length by bike type",
       y = "Average ride length",
       x = "Bike Type")
# bike type and average ride time by user_type
all_trips %>% 
  group_by(member_casual, rideable_type) %>%
  summarise(mean = mean(ride_length),
            .groups = "drop") %>%
  arrange(rideable_type)
#to viz
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>%
  summarise(mean = mean(ride_length),
            .groups = "drop") %>%
  ggplot(aes(rideable_type, mean, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average ride length by bike and user status", 
       y = "Average ride length",
       x = "Bike type")

### Analysis of docking stations
stations<- all_trips_v2 %>%
  gather(key, station_name, start_station_name, end_station_name) %>%
  distinct(station_name)
print(paste("The number of docking stations are", nrow(stations)))

### Most popular station
users_station <- all_trips_v2 %>%
  gather(key, station_name, start_station_name, end_station_name) %>%
  group_by(member_casual, station_name) %>%
  summarise(no_of_trips = n()/2,
            .groups = "drop_last") %>%
  arrange(desc(no_of_trips)) %>%
  slice(1:10)
#to check for annual subscribers
users_station %>% 
  tail(10)
# for casual riders
users_station %>% 
  head(10)


