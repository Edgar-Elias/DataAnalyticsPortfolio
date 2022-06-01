### Bike Company Yearly Rider's Analysis ###

# The purpose of this script is to consolidate downloaded bike data into a single data frame and then conduct simple analysis.
# Help answer the key question: “In what ways do members and casual riders use Cyclistic bikes differently?”


library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(dplyr) #helps manipulate data
library(knitr) #helps filter out data
getwd() #displays your working directory
setwd("/Users/EdgarElias/Desktop/CaseStudy/Tripdata") #sets working directory to simplify calls to data 

# Collecting Data
# Upload tripdata sets (csv files) 
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")
jun_2021 <- read_csv("202106-divvy-tripdata.csv")
jul_2021 <- read_csv("202107-divvy-tripdata.csv")
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")
oct_2021 <- read_csv("202110-divvy-tripdata.csv")
nov_2021 <- read_csv("202111-divvy-tripdata.csv")
dec_2021 <- read_csv("202112-divvy-tripdata.csv")
jan_2022 <- read_csv("202201-divvy-tripdata.csv")
feb_2022 <- read_csv("202202-divvy-tripdata.csv")
mar_2022 <- read_csv("202203-divvy-tripdata.csv")

# Wrangling and combining data into a single file
# Make sure column names match 
colnames(apr_2021)
colnames(may_2021)
colnames(jun_2021)
colnames(jul_2021)
colnames(aug_2021)
colnames(sep_2021)
colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)

# Inspect data frames for incongruencies
str(apr_2021)
str(may_2021)
str(jun_2021)
str(jul_2021)
str(aug_2021)
str(sep_2021)
str(oct_2021)
str(nov_2021)
str(dec_2021)
str(jan_2022)
str(feb_2022)
str(mar_2022)

#C ombine individual trip data frames into one large data frame containing all info
all_trip_data <- bind_rows(apr_2021, may_2021, jun_2021, jul_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, jan_2022, feb_2022, mar_2022)

# Inspect all_trip_data
colnames(all_trip_data) #List of column names
nrow(all_trip_data) #How many rows are in data frame?
dim(all_trip_data) #Dimensions of the data frame?
head(all_trip_data) #See the first 6 rows of data frame. 
tail(all_trip_data) #See the last parts of data frame.
str(all_trip_data) #See list of columns and data types EX: numeric, character, etc.
summary(all_trip_data) #Statistical summary of data for numeric info

#Create copy of combined data frame to work on cleaning and transforming the data starting with removing duplicate rides by checking ride_id 
all_trip_data_v2 <- all_trip_data[!duplicated(all_trip_data$ride_id), ]


#Remove unused columns (latitudes, longitudes, start and end station id's)
all_trip_data_v2 = subset(all_trip_data_v2, select = -c(start_station_id, end_station_id, start_lat, end_lat, start_lng, end_lng))

#Change "started_at" column to "date" and add columns for matching month, day, year, day_of_the_week of each trip/ride
all_trip_data_v2$month <- format(as.Date(all_trip_data_v2$started_at), "%b") #adds month column
all_trip_data_v2$day <- format(as.Date(all_trip_data_v2$started_at), "%d") #adds day column
all_trip_data_v2$year <- format(as.Date(all_trip_data_v2$started_at), "%y") #adds year
all_trip_data_v2$day_of_week <- format(as.Date(all_trip_data_v2$started_at), "%a") #adds character day of week


#Create a column called “ride_length.” Calculating the length of each ride by subtracting the column “started_at” from the column “ended_at”.
all_trip_data_v2$ride_length <- difftime(all_trip_data_v2$ended_at, all_trip_data_v2$started_at, unit = "min")

#Remove column started_at, ended_at
all_trip_data_v2 = subset(all_trip_data_v2, select = -c(started_at, ended_at))


#Inspect structure of new all_trip_data columns
str(all_trip_data_v2)

#Change ride_length to a num data type  
all_trip_data_v2$ride_length <- as.numeric(all_trip_data_v2$ride_length)
# Ensure ride_length is a numeric type
is.numeric(all_trip_data_v2$ride_length)


#Remove trips with no trip duration and false station names like HQ (ride length <= 0 min)
all_trip_data_v2 <- all_trip_data_v2[!(all_trip_data_v2$start_station_name == "HQ QR" | all_trip_data_v2$ride_length<=0), ]

#Remove NA rows from "member_casual" column
all_trip_data_v2 <- all_trip_data_v2[!is.na(all_trip_data_v2$member_casual), ]

#Remove outlier trips with trip duration longer than 48hrs (ride length > 2880 min)
all_trip_data_v2 <- all_trip_data_v2[!(all_trip_data_v2$ride_length > 2880), ]

#Prepare for Analysis
#Statistic view of all_trip_data_v2
summary(all_trip_data_v2)
str(all_trip_data_v2)  

sum(str_count(all_trip_data_v2$member_casual, "casual")) #total sum of casual riders
sum(str_count(all_trip_data_v2$member_casual, "member")) #total sum of member riders
summary(all_trip_data_v2$ride_length) #summary of ride_length 

#Retrieve the top 5 starting station names to see where most rides start from April 2021 - March 2022
top_5_start_station_names <- sort(table(all_trip_data_v2$start_station_name), decreasing=TRUE)[1:5]
knitr::kable(top_5_start_station_names,
             col.names = c("Starting Station Name", "Number of rides"), 
             caption = "Top 5 Starting Stations (April 2021 - March 2022)")

#Retrieve the top 5 station names by user type
members_only <- all_trip_data_v2[!(all_trip_data_v2$member_casual == "casual"),]
casuals_only <- all_trip_data_v2[!(all_trip_data_v2$member_casual == "member"),]
top_5_members_start <- sort(table(members_only$start_station_name), decreasing=TRUE)[1:5]
top_5_members_end <- sort(table(members_only$end_station_name), decreasing=TRUE)[1:5]
top_5_casuals_start <- sort(table(casuals_only$start_station_name), decreasing=TRUE)[1:5]
top_5_casuals_end <- sort(table(casuals_only$end_station_name), decreasing=TRUE)[1:5]
knitr::kable(top_5_members_start,
             col.names = c("Starting Station Name", "Number of Rides"), 
             caption = "Annual Members, Top 5 Starting Stations (April 2021 - March 2022)")
knitr::kable(top_5_members_end,
             col.names = c("Ending Station Name", "Number of Rides"), 
             caption = "Annual Members, Top 5 Ending Stations (April 2021 - March 2022)")
knitr::kable(top_5_casuals_start,
             col.names = c("Starting Station Name", "Number of Rides"), 
             caption = "Casual Users, Top 5 Starting Stations (April 2021 - March 2022)")
knitr::kable(top_5_casuals_end,
             col.names = c("Ending Station Name", "Number of Rides"), 
             caption = "Casual Users, Top 5 Ending Stations (April 2021 - March 2022)")


#Analyze bike type by rider type
member_bike_type <- table(members_only$rideable_type) 
casual_bike_type <- table(casuals_only$rideable_type)
knitr::kable(member_bike_type, 
             col.names = c("Bike Type", "Number of Rides"), 
             caption = "Annual Members, Total Rides by Bike Type (April 2021 - March 2022)")
knitr::kable(casual_bike_type, 
             col.names = c("Bike Type", "Number of Rides"), 
             caption = "Casual Users, Total Rides by Bike Type (April 2021 - March 2022)")

#Prepare for visualizations
#Visualization showing the number of rides per day of week by type of user
all_trip_data_v2$day_of_week <- ordered(all_trip_data_v2$day_of_week, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
all_trip_data_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(mapping = aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Cyclistic: Number of Rides per Day of Week", subtitle = "According to User Type, April 2021-March 2022", caption = "Data courtesy of Motivate International Inc.", x = "Day of Week", y = "Number of Rides", fill = "User Type") +
  scale_y_continuous(labels = scales::comma)

#Visualization showing the average ride duration per day of week by type of user
all_trip_data_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Cyclistic: Average Ride Duration per Day of Week", subtitle = "According to User Type, April 2021-March 2022", caption = "Data courtesy of Motivate International Inc.", x = "Day of Week", y = "Average Ride Duration (minutes)", fill = "User Type")

#Visualization showing the number of rides per month by user type while reordering months starting with May
all_trip_data_v2$month <- ordered(all_trip_data_v2$month, levels=c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))
all_trip_data_v2 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(mapping = aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Cyclistic: Number of Rides per Month", subtitle = "According to User Type, April 2021-March 2022", caption = "Data courtesy of Motivate International Inc.", x = "Month", y = "Number of Rides", fill = "User Type") + scale_y_continuous(labels = scales::comma)
  
#Visualization showing the average ride duration per month by user type
all_trip_data_v2 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Cyclistic: Average Ride Duration per Month", subtitle = "According to User Type, April 2021-March 2022", caption = "Data courtesy of Motivate International Inc.", x = "Month", y = "Average Ride Duration (minutes)", fill = "User Type") 

# Export file to use in Tablaeu
write.csv(all_trip_data_v2,"/Users/EdgarElias/Desktop/CaseStudy/cleaned_all_trip_data_v2.csv", row.names = FALSE)

#Recap of Analysis and Visualization¶
#I determined that Streeter Dr. & Grand Ave, Clark St & Elm St, Lake Shore Dr & Monroe St, Theater on the Lake, and Lake Shore Dr & North Blvd were the most frequented starting stations for all Cyclistic riders from May 2020 - April 2021.
#More specifically, annual members most frequently used the Clark St & Elm St, Broadway & Barry Ave, Wells St & Concord Ln, Dearborn St & Erie St, and St. Clair & Erie St stations. Casual users most frequently used the Streeter Dr. & Grand Ave, Lake Shore Dr & Monroe St, Millennium Park, Theater on the Lake, and Michigan Ave & Oak St stations.
#I determined that both annual members and casual users overwhelmingly preferred docked bikes. Casual riders used electric bikes more than classic bikes by a small margin. Inversely, annual members used classic bikes more than electric bikes by a small margin.
#Weekends have higher numbers of casual users compared to weekdays, and annual members prefer weekdays. Casual users have significantly longer rides than annual members every day of the week.
#Most casual users rent bikes in July-September, and there are very few casual users in December-February. However, casual users rode longer in the summer and also in February, and annual members’ ride lengths are fairly consistent year-round. Annual members outnumber casual users in every month.

#Application of Insights
#Cyclistic can set up promotions or advertisements around the most frequently used starting and ending stations, adjust prices on bike types to encourage more/less usage of a certain type of bike, adjust prices and advertise specifically for certain days of the week and certain months that are geared towards the casual rider. Further research could be done as to where to advertise these concepts based on where casual riders are most likely to see them (at the physical stations, on travel websites, at tourist locations, etc.)

#Additional data on where the casual riders live would be very useful to focus the advertising efforts accordingly. Also, I think using the geographic coordinates would be effective to determine the general regions of the stations and trips. Specifically, if we know exactly what tourist stops the casual rider will make throughout their journey (specific restaurants, events, parks, etc.) then we can focus advertising at these locations.


#Top 3 Recommendations based on Analysis
#Cyclistic can set up promotions or advertisements around the most frequently used starting and ending stations used by casual riders. Since casual riders typically have longer ride durations than annual members, they could be notified that buying an annual membership could save them money on their trips.
#Cyclistic can adjust prices on bike types to encourage more/less casual usage of a certain type of bike. Since casual users favor docked bikes more than the others, Cyclistic could offer more of these types of bikes, or offer a discount on the annual membership price for renting electric or classic bikes.
#Cyclistic can increase advertisement towards casual riders specifically for weekends and summer months. Cyclistic can encourage casual riders that they will save money and enjoy perks if they purchase annual memberships. First-Ride Promotions can be offered to new annual members during the week and during winter months.
#Cyclistic can offer summer pass membership or weekend pass memberships to convert casual riders to members. This type of pass can be used as a bridge to convert casuals to full year members. Offer a tier system of casual, summer/weekend pass, year pass with each having its own perks based on tier of membership.

tinytex::install_tinytex()
