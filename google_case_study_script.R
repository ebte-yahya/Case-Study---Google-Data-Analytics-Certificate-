# Google Data Analytics Certificate Course 8 Case Study (08/01/2022)

### How Does a Bikeshare Navigate Speedy Success?
## Cyclistic Analysis Case Study

## Business Objective = determine how casual riders and annual
## members use the bikes differently, and design a new marketing 
## strategy to convert casual users to annual members

# installing + loading necessary packages

install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("janitor")
install.packages("[sych")
install.packages("scales")

library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(psych)
library(scales)

# loading filtered data into objects by month 

jul21 <- read_csv("jul21.csv")
aug21 <- read_csv("aug21.csv")
sep21 <- read_csv("sep21.csv")
oct21 <- read_csv("oct21.csv")
nov21 <- read_csv("nov21.csv")
dec21 <- read_csv("dec21.csv")
jan22 <- read_csv("jan22.csv")
feb22 <- read_csv("feb22.csv")
mar22 <- read_csv("mar22.csv")
apr22 <- read_csv("apr22.csv")
may22 <- read_csv("may22.csv")
jun22 <- read_csv("jun22.csv")



### Cleaning the Data

# combining all of the datasets into one (joining)

year_of_trips <- rbind(jul21, aug21, sep21, oct21, nov21, dec21, jan22, 
                       feb22, mar22, apr22, may22, jun22)

colnames(year_of_trips)


# removing rows with NA values
colSums(is.na(year_of_trips))

# remove any duplicates
year_cleaned <- distinct(year_of_trips)

# removing rows with start and/or end times that don't make sense
year_cleaned <- year_cleaned %>% 
  filter(started_at < ended_at)

# change column names to be more understandable
year_cleaned <- rename(year_cleaned, bike_type = rideable_type, 
                       start_time = started_at,
                       end_time = ended_at,
                       customer_type = member_casual)

# remove NA, empty, and missing values
drop_na(year_cleaned)
remove_empty(year_cleaned)  
remove_missing(year_cleaned)

# creating a Duration column that calculates (end_time - start_time) in seconds
year_cleaned <- year_cleaned %>% 
  mutate (trip_duration = end_time - start_time)

# creating Date columns (date, month, day, year) from the start_time column 
year_cleaned <- year_cleaned %>% 
  mutate(date = as.Date(start_time), day_of_week = format(as.Date(date), "%A"), 
         month = format(as.Date(date), "%b_%y"), 
         year = format(as.Date(date), "%Y"))

# remove rides that are longer than 24 hours (Cyclistic considers these bikes stolen)
# and rides that have a negative duration 

year_cleaned <- year_cleaned %>% 
  filter(!(trip_duration < 0 | trip_duration > 86400))

# changing the values in customer_type to be more readable ("casual" --> "Casual User", 
# "member" --> "Annual Member")
year_cleaned$customer_type <- factor(year_cleaned$customer_type, levels = c("casual", "member"), 
                                     labels = c("Casual Users", "Annual Members"))


### Data Analysis --- Identifying Trends

# order the months and days of the week for future analysis
year_cleaned$month <- ordered(year_cleaned$month, 
                              levels = c("Jul_21", "Aug_21", "Sep_21",
                                         "Oct_21", "Nov_21", "Dec_21",
                                         "Jan_22", "Feb_22", "Mar_22",
                                         "Apr_22", "May_22", "Jun_22"))

year_cleaned$day_of_week <- ordered(year_cleaned$day_of_week, 
                                    levels = c("Sunday", "Monday", 
                                               "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday"))


# differences between annual members and casual riders based on trip duration
# (mean, median, max, min)
year_cleaned %>% 
  group_by(customer_type) %>% 
  summarize(min_duration = min(trip_duration), max_duration = max(trip_duration),
            median_duration = median(trip_duration), mean_duration = mean(trip_duration)) %>%
  ggplot()
  


# average trip length by month
year_cleaned %>% 
  group_by(month) %>% 
  summarise(mean_ride_length = mean(trip_duration))

# average trip length by member type
year_cleaned %>% 
  group_by(customer_type) %>% 
  summarise(mean_ride_length = mean(trip_duration))
# casual customers tend to ride for longer periods of time, on average, as compared
# to annual members


# average ride length and total number of rides by day of the week
year_cleaned %>% 
  group_by(day_of_week) %>% 
  summarise(avg_duration = mean(trip_duration), 
            number_of_rides = n())

# average number of rides per month
year_cleaned %>% 
  group_by(month) %>% 
  summarise(avg_duration = mean(trip_duration), 
            number_of_rides = n())

# average ride length by day of week, casual vs annual members
view(aggregate(year_cleaned$trip_duration ~ year_cleaned$customer_type +
            year_cleaned$day_of_week, FUN=mean))


# average ride length by month, casual vs annual members
view(aggregate(year_cleaned$trip_duration ~ year_cleaned$customer_type +
            year_cleaned$month, FUN=mean))

# analysis of ride length by customer type + weekday
year_cleaned %>% 
  group_by(customer_type, day_of_week) %>% 
  summarize(number_of_rides = n(), 
            average_duration = mean(trip_duration),
            median_duration = median(trip_duration),
            max_duration = max(trip_duration),
            min_duration = min(trip_duration))

# analysis of ride length by customer type + month
year_cleaned %>% 
  group_by(customer_type, month) %>% 
  summarize(number_of_rides = n(), 
            average_duration = mean(trip_duration),
            median_duration = median(trip_duration),
            max_duration = max(trip_duration),
            min_duration = min(trip_duration)) 


### Data Visualization

# total number of bike rides  (casual vs annual)
total_users <- year_cleaned %>% 
  group_by(customer_type) %>% 
  summarize(total_count = n()) %>% 
  ggplot() + geom_col(aes(x=customer_type, y=total_count, fill=customer_type)) +
  labs(title = "Total Number of Bike Rides, Casual Users vs. Annual Members", 
       subtitle = "July 2021 - June 2022", x = "Customer Type", 
       y = "Total Number", caption = "Google Capstone: Ebtehal Yahya") + 
  scale_y_continuous(labels = comma, breaks=seq(0, 3500000, by=500000)) + 
  theme_minimal() + 
  scale_fill_manual(values=c("#0492C2", "#800000"), labels =c("Casual", "Member"))
  
total_users + theme(plot.title = element_text(face="bold", hjust = 0.5), 
                    plot.subtitle = element_text(hjust = 0.5), 
                    strip.text = element_text(size = 11, face="bold")) 
ggsave("total_number_of_rides.png")


# total ride duration (casual vs annual)
total_duration <- year_cleaned %>% 
  group_by(customer_type) %>% 
  summarize(total_duration = sum(trip_duration)) %>% 
  ggplot() + geom_col(aes(x=customer_type, y=((total_duration/60))/60, fill=customer_type)) +
  labs(title = "Total Duration of Bike Rides, Casual Users vs. Annual Members", 
       subtitle = "July 2021 - June 2022", x = "Customer Type", 
       y = "Total Duration (hours)", caption = "Google Capstone: Ebtehal Yahya") + 
  scale_y_continuous(labels = comma, breaks=seq(0, 1200000, by=200000)) +
  theme_minimal() +
  scale_fill_manual(values=c("#0492C2", "#800000"), labels =c("Casual", "Member"))

total_duration + theme(plot.title = element_text(face="bold", hjust = 0.5), 
                       plot.subtitle = element_text(hjust = 0.5), 
                       strip.text = element_text(size = 11, face="bold")) 
ggsave("total_duration.png")


# analysis of ride length by customer type + month
month_duration <- year_cleaned %>% 
  group_by(customer_type, month) %>% 
  summarize(average_duration = mean(trip_duration)) %>% 
  ggplot() + geom_col(aes(x=month, y = (average_duration/60), fill=customer_type)) +
  facet_wrap(~customer_type) +
  labs(title = "Average Ride Duration by Month for Casual Users and Annual Members",
       subtitle = "July 2021 - June 2022", x = "Month", y = "Average Duration (min)", 
       caption = "Google Capstone: Ebtehal Yahya", fill="Customer Type") +
  ylim(0, 30) +
  theme_minimal() +
  scale_fill_manual(values=c("#0492C2", "#800000")) 

month_duration + theme(plot.title = element_text(face="bold", hjust = 0.5), 
                       plot.subtitle = element_text(hjust = 0.5), 
                       strip.text = element_text(size = 11, face="bold"))  
ggsave("avg_duration_bymonth.png")


# analysis of ride length by customer type + weekday
weekday_duration <- year_cleaned %>% 
  group_by(customer_type, day_of_week) %>% 
  summarize(average_duration = mean(trip_duration)) %>% 
  ggplot() + geom_col(aes(x=day_of_week, y = (average_duration/60), fill=customer_type)) +
  facet_wrap(~customer_type) +
  labs(title = "Average Ride Duration by Day of the Week for Casual Users and Annual Members",
       subtitle = "July 2021 - June 2022", x = "Day of the Week", y = "Average Duration (min)", 
       caption = "Google Capstone: Ebtehal Yahya", fill="Customer Type") +
  ylim(0, 30) +
  theme_minimal() + 
  scale_fill_manual(values=c("#0492C2", "#800000"))

weekday_duration + theme(plot.title = element_text(face="bold", hjust = 0.5), 
                         plot.subtitle = element_text(hjust = 0.5), 
                         strip.text = element_text(size = 11, face="bold")) 
ggsave("avg_duration_byweekday.png")


# number of riders by month, casual users vs annual members
month_riders <- year_cleaned %>% 
  group_by(customer_type, month) %>% 
  summarize(number_of_riders = n()) %>% 
  ggplot() + geom_col(aes(x=month, y = number_of_riders, fill=customer_type)) +
  facet_wrap(~customer_type) + 
  scale_y_continuous(labels = comma, breaks=seq(0, 400000, by=100000)) + 
  labs(title = "Average Number of Rides by Month for Casual Users and Annual Members",
       subtitle = "July 2021 - June 2022", x = "Month", y = "Average Number of Rides", 
       caption = "Google Capstone: Ebtehal Yahya", fill="Customer Type") +
  theme_minimal() + 
  scale_fill_manual(values=c("#0492C2", "#800000"))

month_riders + theme(plot.title = element_text(face="bold", hjust = 0.5), 
                     plot.subtitle = element_text(hjust = 0.5), 
                     strip.text = element_text(size = 11, face="bold")) 
ggsave("avg_rides_bymonth.png")
  

# number of riders by day of the week, casual users vs annual members
weekday_riders <- year_cleaned %>% 
  group_by(customer_type, day_of_week) %>% 
  summarize(number_of_riders = n()) %>% 
  ggplot() + geom_col(aes(x=day_of_week, y = number_of_riders, fill=customer_type)) +
  facet_wrap(~customer_type) + 
  scale_y_continuous(labels = comma, breaks=seq(0, 500000, by=100000)) + 
  labs(title = "Average Number of Rides by Day of the Week for Casual Users and Annual Members",
       subtitle = "July 2021 - June 2022", x = "Day of the Week", y = "Average Number of Rides", 
       caption = "Google Capstone: Ebtehal Yahya", fill="Customer Type") +
  theme_minimal() + 
  scale_fill_manual(values=c("#0492C2", "#800000"), labels =c("Casual", "Member"))

weekday_riders + theme(plot.title = element_text(face="bold", hjust = 0.5), 
                       plot.subtitle = element_text(hjust = 0.5), 
                       strip.text = element_text(size = 11, face="bold")) 
ggsave("avg_rides_byweekday.png")

### Trends

# 1. There were blank% more bike rides taken by annual members than
# casual users from July 2021 - June 2022
# 2. Casual users tend to take longer bike rides, and have a peak
# in usership during:
# - the weekends (Friday-Sunday)
# - the summer months (May-Aug)
# 3. Annual members tend to take shorter bike rides, presumably for 
# commute to and from work and have a peak in usership during: 
# - the middle of the week (Tuesday-Thursday)
# - the spring-fall (May-Oct)
# 4. Both casual ridesr and annual members usership drops during the 
# winter, but the number of casual riders decreases dramatically whereas 
# annual membership remains more consistent throughout the year 


### Suggestions + Recommendations

# 1. Develop a new tier of membership -- monthly passes
## As it stands, you can only purchase daily passes or an annual membership.
## Institute a monthly subscription plan to entice more customers to join, 
## which will increase revenue and offer customers more flexibility should 
## they wish to join, but do not want to sign on for a full year. 
# 2. Run a promotion during the Winter months (Dec-Feb)
## Offer a special 3-month deal at a discounted price to boost usership during
## this seasonal lull. In order to achieve the long-term goal of converting 
## more people to annual members, once the 3-month period is finished, offer 
## these customers an annual membership with a discounted first year. 
# 3. Advertuse the service towards casual users as a great method for 
# commute and more regular activities
## Majority of annual members use the bikes as a way to get to/from work, so
## letting casual users know and adveristing how that can benefit them (save
## money, help the environment, etc.) can assist in converting them 
