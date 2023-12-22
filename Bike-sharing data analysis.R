#  This code uses the dplyr and the tidyr packages for data manipulation and summarization.

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

#We are using Divvy_Trips_2019_Q1.csv prefiltered at Google Sheets: calculated ride_length	and day_of_week.

# Read data from a CSV file
cyclistic_data <- read.csv("Divvy_Trips_2019_Q1.csv")

# Convert time to numeric (total seconds)
cyclistic_data$numeric_ride_length <- as.numeric(sapply(strsplit(cyclistic_data$ride_length, ":"), function(x) {
  as.numeric(x[1]) * 3600 + as.numeric(x[2]) * 60 + as.numeric(x[3])
}))

# Calculate the mean of ride_length
mean_ride_length <- mean(cyclistic_data$numeric_ride_length)

# Calculate the max of ride_length
max_ride_length <- max(cyclistic_data$numeric_ride_length)

# Calculate the mode of day_of_week (using table and which.max)
mode_day_of_week <- as.numeric(names(table(cyclistic_data$day_of_week))[which.max(table(cyclistic_data$day_of_week))])

# Create a pivot table for average ride_length for members and casual riders
pivot_table_avg_ride_length <- cyclistic_data %>%
  group_by(usertype) %>%
  summarise(avg_ride_length = mean(numeric_ride_length, na.rm = TRUE))

# Create a pivot table for average ride_length for users by day_of_week
pivot_table_avg_ride_length_by_day <- cyclistic_data %>%
  group_by(day_of_week, usertype) %>%
  summarise(avg_ride_length = mean(numeric_ride_length, na.rm = TRUE))

# Create a pivot table for the number of rides for users by day_of_week
pivot_table_rides_by_day <- cyclistic_data %>%
  group_by(day_of_week) %>%
  summarise(num_rides = n())

# Display the results
print(pivot_table_avg_ride_length)
print(pivot_table_avg_ride_length_by_day)
print(pivot_table_rides_by_day)

# Export the descriptive statistics to a CSV file
summary_data <- data.frame(
  Mean_Ride_Length = mean_ride_length,
  Max_Ride_Length = max_ride_length,
  Mode_Day_of_Week = mode_day_of_week
)
