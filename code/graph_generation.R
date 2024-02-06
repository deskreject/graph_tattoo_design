#.........
# Author: Alexander Staub
# created at: 2024-02-06
# description: Wrangling mood data and plotting data for a tattoo
#..........

# installing and loading packages
#if (!require(dplyr)) install.packages("dplyr"); library(dplyr)  
#if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
#if (!require(readr)) install.packages("readr"); library(readr)
#if (!require(tidyr)) install.packages("tidyr"); library(tidyr)
#if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)  
#if (!require(here)) install.packages("here"); library(here)  

#load libraries
library(dplyr) # data manipulation
library(lubridate) # date manipulation
library(readr) # read data
library(tidyr) # data wrangling
library(ggplot2) # plotting
library(here) # file paths

#............
# renv stuff
#............

#renv::status()

#save new lockfile
#renv::snapshot()

#load lockfile
#renv::restore()

####------------------------------------ Load and wrangel data ------------------------------------####

#load the 
mood_data <- read_csv(here("data", "mood_meter_data.csv"))


glimpse(mood_data)

#remove all rows with NAs for mood_7_day_rolling
mood_data_cleaned <- mood_data %>% 
  filter(!is.na(Mood_7_day_rolling)) %>%
  select(date,
         Mood_7_day_rolling,
         mood_28_day_rolling) %>% 
  rename(mood_7_day_rolling = Mood_7_day_rolling)

#add an indicator variable for each year
mood_data_cleaned <- mood_data_cleaned %>% 
  mutate(year = year(date)) %>% 
  # remove 2019 and 2024
  filter(year != 2019 & year != 2024)

#create a table that I can use to join that counts the years in the dataset from 0 to the last year in the dataset
years <- mood_data_cleaned %>% 
  select(year) %>% 
  distinct() %>% 
  arrange(year) %>% 
  mutate(number = 0:3)


#join the years table to the mood_data_cleaned table
mood_data_cleaned <- mood_data_cleaned %>% 
  left_join(years, by = "year") %>%
  #add the constant to the mood_7_day_rolling and mood_28_day_rolling variable
  mutate(mood_7_day_rolling_incremented = mood_7_day_rolling + number,
         mood_28_day_rolling_incremented = mood_28_day_rolling + number)

#create a date variable that doesn't include the year
mood_data_cleaned <- mood_data_cleaned %>% 
  mutate(date_no_year = format(date, "%m-%d"))
#### ------------------------------ plotting ------------------------------------####

#plot the mood_7_day_rolling by year as a line plot
mood_data_cleaned %>% 
  ggplot(aes(x = date_no_year, y = mood_7_day_rolling, group = year, color = factor(year))) +
  geom_line() +
  labs(title = "Mood over time",
       x = "Date",
       y = "Mood",
       color = "Year") +
  theme_minimal()

#same plot but with the incremented mood_28_day_rolling
mood_data_cleaned %>% 
  ggplot(aes(x = date_no_year, y = mood_28_day_rolling, group = year, color = factor(year))) +
  geom_line() +
  labs(title = "Mood over time",
       x = "Date",
       y = "Mood",
       color = "Year") +
  theme_minimal()

#do the same plot with the mood_7_day_rolling_incremented
mood_data_cleaned %>% 
  ggplot(aes(x = date_no_year, y = mood_7_day_rolling_incremented, group = year, color = factor(year))) +
  geom_line() +
  labs(title = "Mood over time",
       x = "Date",
       y = "Mood",
       color = "Year") +
  theme_minimal()

#do the same plot with the mood_28_day_rolling_incremented
mood_data_cleaned %>% 
  ggplot(aes(x = date_no_year, y = mood_28_day_rolling_incremented, group = year, color = factor(year))) +
  #change the color of the lines to be on a gray scale
  #make the color for the 2023 year red
  scale_color_manual(values = c("2020" = "black",
                                "2021" = "black",
                                "2022" = "black",
                                "2023" = "red")) +
  geom_line(aes(color = factor(year)), linewidth = 0.8) +
  # add constants as y intercept lines dashed
  geom_hline(yintercept = 0:3, linetype = "dashed") +
  # reduce the x tiks to the month level without using x_scale_date
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", "07-01", "08-01", "09-01", "10-01", "11-01", "12-01")) +
  #remove the y tiks
  scale_y_continuous(breaks = NULL) +
  #add year 2020-2023 as a label
  geom_text(aes(x = "02-01", y = 0, label = "2020", color = "blue"), hjust = 2, vjust = 0) +
  geom_text(aes(x = "02-01", y = 1, label = "2021", color = "blue"), hjust = 2, vjust = 0) +
  geom_text(aes(x = "02-01", y = 2, label = "2022", color = "blue"), hjust = 2, vjust = 0) +
  geom_text(aes(x = "02-01", y = 3, label = "2023", color = "red"), hjust = 2, vjust = 0) +
  
  labs(title = "", x = "", y = "", color = "") + # Remove unnecessary labels 
  theme_minimal() +
  theme(legend.position = "none")

#do exactly the same plot for mood_7_day_rolling_incremented
mood_data_cleaned %>% 
  ggplot(aes(x = date_no_year, y = mood_7_day_rolling_incremented, group = year, color = factor(year))) +
  scale_color_grey() +
  scale_color_manual(values = c("2023" = "red")) +
  geom_line() +
  geom_hline(yintercept = 0:3, linetype = "dashed") +
  scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", "07-01", "08-01", "09-01", "10-01", "11-01", "12-01")) +
  scale_y_continuous(breaks = NULL) +
  geom_text(aes(x = "02-01", y = 0, label = "2020", color = "blue"), hjust = 2, vjust = 0) +
  geom_text(aes(x = "02-01", y = 1, label = "2021", color = "blue"), hjust = 2, vjust = 0) +
  geom_text(aes(x = "02-01", y = 2, label = "2022", color = "blue"), hjust = 2, vjust = 0) +
  geom_text(aes(x = "02-01", y = 3, label = "2023", color = "red"), hjust = 2, vjust = 0) +
  labs(title = " ",
       x = " ",
       y = " ",
       color = " ") +
  theme_minimal()

#### ------------------------------- Analysis ------------------------------------####

#regression out of interest
ols_simple <- lm(mood ~ lag(mood) + Meditation + Sport + creative_hours + other_negative_incidents + to_dos + rain_snow + cold_below_10 + sleep_in_h + sleep_quality, data = mood_data)

summary(ols_simple)
