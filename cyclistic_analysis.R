data_2023_01 <- read.csv("202301-divvy-tripdata.csv")
data_2023_02 <- read.csv("202302-divvy-tripdata.csv")
data_2023_03 <- read.csv("202303-divvy-tripdata.csv")
data_2023_04 <- read.csv("202304-divvy-tripdata.csv")
data_2023_05 <- read.csv("202305-divvy-tripdata.csv")
data_2023_06 <- read.csv("202306-divvy-tripdata.csv")
data_2023_07 <- read.csv("202307-divvy-tripdata.csv")
data_2023_08 <- read.csv("202308-divvy-tripdata.csv")
data_2023_09 <- read.csv("202309-divvy-tripdata.csv")
data_2023_10 <- read.csv("202310-divvy-tripdata.csv")
data_2023_11 <- read.csv("202311-divvy-tripdata.csv")
data_2023_12 <- read.csv("202312-divvy-tripdata.csv")

install.packages("tidyverse")
install.packages("DescTools")
library(tidyverse)
library(lubridate)
library(hms)
library(data.table)
library(dplyr)
library(DescTools)

#---Creating Data Frame---

# merge files into a single dataframe
data_2023 <- rbind(data_2023_01, data_2023_02, data_2023_03, data_2023_04, data_2023_05, data_2023_06, data_2023_07, data_2023_08, data_2023_09, data_2023_10, data_2023_11, data_2023_12)

# remove monthly files from the environment
remove(data_2023_01, data_2023_02, data_2023_03, data_2023_04, data_2023_05, data_2023_06, data_2023_07, data_2023_08, data_2023_09, data_2023_10, data_2023_11, data_2023_12)

# duplicate df
data_2023_2 <- data_2023

#---Cleaning---

# remove duplicate rows
data_2023_2 <- distinct(data_2023_2)

# remove rows with null values
data_2023_2 <- na.omit(data_2023_2)

# remove unneeded columns
data_2023_2 <- data_2023_2 %>%  
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

# total number of rides
round(nrow(data_2023_2), digits = -5)

#---Creating Columns---

# create column ride_length
data_2023_2 <- data_2023_2 %>%
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")))

# remove rows where ride_length is <= zero
data_2023_2 <- data_2023_2[!(data_2023_2$ride_length <=0),]

# create column day_of_week 
data_2023_2$day_of_week <- wday(data_2023_2$started_at)

# create columns for month
data_2023_2$month <- format(as.Date(data_2023_2$started_at), "%m") 

# create a column for ride season classification
data_2023_2 <- data_2023_2 %>% mutate(season = 
                                        case_when(month == "01" ~ "Winter",
                                                  month == "02" ~ "Winter",
                                                  month == "03" ~ "Spring",
                                                  month == "04" ~ "Spring",
                                                  month == "05" ~ "Spring",
                                                  month == "06" ~ "Summer",
                                                  month == "07" ~ "Summer",
                                                  month == "08" ~ "Summer",
                                                  month == "09" ~ "Fall",
                                                  month == "10" ~ "Fall",
                                                  month == "11" ~ "Fall",
                                                  month == "12" ~ "Winter")
)

#---Analysis---

# count member types
data_2023_2 %>% count(member_casual)

# total rides by bike type
data_2023_2 %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

# min, max, median, mean of ride_length
summary(data_2023_2$ride_length)

# mode of day_of_week using Mode function
mode_day_of_week <- Mode(data_2023_2$day_of_week)
cat("Mode of day_of_week:", mode_day_of_week, "\n")

# average ride_length for members and casual riders
pivot_table_1 <- data_2023_2 %>%
  group_by(member_casual) %>%
  summarize(Average_ride_length = mean(ride_length, na.rm = TRUE))

# average ride_length for users by day_of_week
pivot_table_2 <- data_2023_2 %>%
  group_by(day_of_week) %>%
  summarize(Average_ride_length = mean(ride_length, na.rm = TRUE))

# number of rides for users by day_of_week
pivot_table_3 <- data_2023_2 %>%
  group_by(day_of_week) %>%
  summarize(Count_of_rides = n())

#---Visualization---

#converts values from scientific notation 
options(scipen = 999)

# Print the pivot tables
print("Average ride_length for members and casual riders")
print(pivot_table_1)

print("Average ride_length for users by day_of_week")
print(pivot_table_2)

print("Number of rides for users by day_of_week")
print(pivot_table_3)

# plot average ride_length by member type
data_2023_2 %>% 
  group_by(member_casual) %>% 
  dplyr::summarise(avg_ride_length = mean(ride_length, na.rm = TRUE)) %>% 
  ggplot(aes(x = member_casual, y = avg_ride_length, fill = member_casual)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = "Comparison of Average Ride Length for Annual Members and Casual Riders")

# plot bicycle type by number of rides
data_2023_2 %>%
  group_by(rideable_type, member_casual) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x=rideable_type, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position='dodge') +
  theme_bw()+
  labs(title="Number of Trips by Bicycle Type", x="Bicycle Type", y="Number of Rides")

# plot avg_ride_length by day of week
data_2023_2 %>% 
  group_by(member_casual, day_of_week) %>%
  summarize(avg_ride_length = mean(ride_length, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(day_of_week, levels = 1:7), y = avg_ride_length, fill = member_casual, color = member_casual)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  labs(title = "Average Ride Length by Day of Week", x = "Day of Week", y = "Average Ride Length") +
  scale_x_discrete(labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# plot number of rides by season
data_2023_2 %>% 
  group_by(member_casual, season) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x= season, y=count_trips, fill=member_casual, color=member_casual)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme_bw()+
  labs(title ="Number of Rides by Season", x = "Season", y = "Number of Rides")
