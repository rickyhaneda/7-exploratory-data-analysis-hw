# Worked a little bit with Naomi and Krista

library(tidyverse)
library(DataExplorer)
library(rstatix)

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")

# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

library(fs) 

read_weather <- function(station) {
  read_csv(path("us-weather-history", paste0(station, ".csv"))) %>%
    mutate(
      station = station,
      date = as.Date(date)
    )
}

glimpse(read_weather("KCLT"))


# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, 
#> you will need to do so in the code with bind_rows()
#> Save the resulting dataset to "ds"

ds <- map(stations, read_weather) %>% 
  bind_rows()

glimpse(ds)

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use rstatix::freq_tables() to check that there are 365 days of data for each city 
#> and save the results to a new tibble
#> Write a logical statement that would trigger a warning if any city did not have 365 days


ds <- ds %>% 
  mutate(city = factor(station, levels = stations, labels = cities))

city_counts <- ds %>%
  freq_table(city)

city_counts

check_365 <- function(data) {
  counts <- data %>% freq_table(city)
  
  if (any(counts$n != 365) || nrow(counts) != length(cities)) {
    warning("At least one city does not have 365 days of data")
  }
  
  counts
}

check_365(ds)

# QUESTION 4
#> In question 3, all of the cities do have 365 days of data, so how would you know
#> if your check worked? Create a test to mess up the dataset to make sure that your
#> check does throw an error if the conditions aren't met


# I decided to drop 5 rows from Charlotte, so it should warn

ds_bad <- ds %>%
  filter(!(city == "Charlotte" & row_number() <= 5))

check_365(ds_bad)   # expect a warning, Charlotte at 360

#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

glimpse(ds)

# QUESTION 5
#> Find a function that lets you pull out the month from the date 
#> and make "month" a factor as a new variable in ds

ds <- ds %>%
  mutate(month = factor(month(date, label = TRUE)))
glimpse(ds)

# QUESTION 7
#> Use rstatix to check if there are any outliers for actual_mean_temp.
#> Save the outliers to a new tibble.
#> In a piped statement, filter out those outliers from ds and then use
#> get_summary_stats() to get the mean and SE for actual_mean_temp
#> by month.
outliers <- ds %>%
  identify_outliers(actual_mean_temp)

outliers   # checking how many rows came back

ds %>%
  anti_join(outliers, by = c("station", "date")) %>%
  group_by(month) %>%
  get_summary_stats(actual_mean_temp, type = "mean_se")

# As far as I could get, this hw took me a long time!

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this


# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month


# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> filtered for that particular month.
#> Your ggplot will need both geom_line() and geom_scatter().
#> Use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month


