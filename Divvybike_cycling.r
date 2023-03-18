install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)

getwd()
setwd("D:\\Google Data Analytics\\casestudy\\original data")

m1_2022 <- read_csv("202201-divvy-tripdata.csv")
m2_2022 <- read_csv("202202-divvy-tripdata.csv")
m3_2022 <- read_csv("202203-divvy-tripdata.csv")
m4_2022 <- read_csv("202204-divvy-tripdata.csv")
m5_2022 <- read_csv("202205-divvy-tripdata.csv")
m6_2022 <- read_csv("202206-divvy-tripdata.csv")
m7_2022 <- read_csv("202207-divvy-tripdata.csv")
m8_2022 <- read_csv("202208-divvy-tripdata.csv")
m9_2022 <- read_csv("202209-divvy-tripdata.csv")
m10_2022 <- read_csv("202210-divvy-tripdata.csv")
m11_2022 <- read_csv("202211-divvy-tripdata.csv")
m12_2022 <- read_csv("202212-divvy-tripdata.csv")

colnames(m1_2022)
colnames(m2_2022)

# Inspect the dataframes and look for incongruencies
#checking columns data type
str(m1_2022)
str(m2_2022)
str(m3_2022)
str(m4_2022)

# Convert ride_id and rideable_type to character so that they can stack correctly
m1_2022 <-  mutate(m1_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
                   
m2_2022 <-  mutate(m2_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

m3_2022 <-  mutate(m3_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m4_2022 <-  mutate(m4_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m5_2022 <-  mutate(m5_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m6_2022 <-  mutate(m6_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m7_2022 <-  mutate(m7_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m8_2022 <-  mutate(m8_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m9_2022 <-  mutate(m9_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m10_2022 <-  mutate(m10_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m11_2022 <-  mutate(m11_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

m12_2022 <-  mutate(m12_2022, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 


# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(m1_2022, m2_2022, m3_2022, m4_2022,
                       m5_2022, m6_2022, m7_2022, m8_2022,
                       m9_2022, m10_2022, m11_2022, m12_2022)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#Inspect the new table that has been created
colnames(all_trips)   #List of column names
#nrows(all_trips)
dim(all_trips)       #Dimensions of the data frame?
head(all_trips)      #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)       #See list of columns and data types (numeric, character, etc)
summary(all_trips)   #Statistical summary of data. Mainly for numerics

# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)
#-----------------------------
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Step 4 descriptive analysis
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

write.csv(all_trips_v2, file = "final_data.csv")



