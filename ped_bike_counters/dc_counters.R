# PLAN 372 Exercise 2: Bicycle counter data
# In this exercise, we'll be working with data from automated bicycle counters in North Carolina,
# from the North Carolina Non-Motorized Volume Data Portal (https://itre.ncsu.edu/focus/bike-ped/nc-nmvdp/)

# As always, we'll start by loading libraries
# You shouldn't need to use install.packages(), as these packages should already be installed
library(tidyverse)
library(lubridate)

# Next, we can load our data. Use read_csv to load the data.
data = read_csv("ped_bike_counters/data/arlington_data.csv")

# Lets look at the first part of our data
View(head(data))

# That doesn't look right - the column names are in the third row, with additional information
# about the file above it. We can skip those rows when loading.
# This happens sometimes with CSV files, especially those that were exported from Excel.
data = read_csv("ped_bike_counters/data/arlington_data.csv", skip=3)

