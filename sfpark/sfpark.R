# Do some exploratory data analysis of the SFpark data
# SFpark is the parking demand management program in San Francisco.
# The data we're using are entries and exits from public parking
# garages from 2011-2013, available from
# https://www.sfmta.com/getting-around/drive-park/demand-responsive-pricing/sfpark-evaluation

# First, we need to load the libraries we will be using
# If you get an error that a library was not found,
# run install.packages("tidyverse")
library(tidyverse)  # library for dealing with tabular data
library(lubridate)  # library for handling dates

# first, we read our data into a table
# The data are packaged as a CSV (comma separated values) file, which is
# just a text file where each line is a row, and the columns are
# separated by commas.
data = read_csv("sfpark/data/sfpark.csv")

# now, lets look at the first few rows of our data to see what we're looking at
# [1:20,] means rows 1-20 (before the comma) and all columns
# (because there is nothing after the comma)
View(data[1:20,])

# we have six columns:
#   date: the date the data were recorded
#   facility: the garage where the data were recorded
#   district: the district (neighborhood) where the garage is
#   usage_type: the type of payment (e.g. monthly pass, transient/hourly)
#   entries, exits: the number of cars that entered and exited the garage that day that used that type of payment

# what are the garages we have?
unique(data$facility)

# Exercise: where in San Francisco are they (in which districts)?

# In your own words, what does a row of the data represent?

# How many cars enter a garage on an average day
mean(data$entries)

# Exercise: That may be skewed by outliers. What is the median?

###########################
# Grouped data analysis   #
# aka split-apply-combine #
###########################

# Grouped data analysis is a very common pattern - rather than a mean
# over the entire dataset, we may want a mean by groups. For instance,
# the median being so different from the mean suggests outliers - perhaps
# one very large garage. Let's look at the mean entries by garage

group_by(data, facility) |>
  summarize(mean_entries=mean(entries))

# But this could be misleading. Does each row represent a single day at a single garage?
# What are we actually computing the mean of above?

# We can use the group_by and summarize functions to create a dataset
# that shows _total_ entries and exits for each day, and create a new
# table with this information.
total_entries = group_by(data, date, facility) |>
  summarize(entries=sum(entries), exits=sum(exits))

# Look at the data we have now
View(total_entries[1:20,])

# Now we can calculate the mean entries by garage, using this new dataset

##################
# Filtering data #
##################
# Maybe we don't want to look at the entire city, but only at garages
# in the Civic Center neighborhood. We can filter the data and repeat
# the above analysis.
civic_center_data = filter(data, district == "Civic Center")

# now, repeat the above analysis to compute total entries by day, and take the daily
# average, using only data from Civic Center

# use group_by and summarize to compute means

# Repeat the process, looking only at garages in the Mission

###################
# Handling dates  #
###################
# Another dimension of this dataset that we have not explored
# is the date column. Before we can work with dates, however,
# we have to parse the column.

# Every column in a table has a type (for instance, integer,
# decimal number, string). read_csv tries to guess the best
# type for each field. We can see what types are used in our
# table by printing the table to the console. The types are printed
# just below the column names.
total_entries

# read_csv has read the date string as 
# These columns only store the letters, and don't know that they represent dates. Before
# we can use the dates, we need to parse the dates.
# to parse dates, we will use the lubridate library: https://lubridate.tidyverse.org/
# the transmute function will overwrite the date column
# the mdy function parses dates in month/day/year format
total_entries = mutate(total_entries, date=mdy(date))

# display the table again, to check the types
total_entries

# Let's look at the mean entries by year, to look for trends over time
# First, we need to extract the year from the date column, and assign it to a new
# column. We previously used mutate for this; this line uses
# $ notation which is another way of doing the same thing.
total_entries$year = year(total_entries$date)

# make sure it worked
unique(total_entries$year)

# now, look at mean entries by garage and year
group_by(total_entries, facility, year) |>
  summarize(entries=mean(entries))

# The year_totals table is in "long" format - one row for each
# facility for each year. It would be easier to read in "wide" format:
# one row for each facility and one column for each year.
# the column names are taken from the field named in names_from,
# and the values from the field named in values_from
# now, look at mean entries by garage and year
group_by(total_entries, facility, year) |>
  summarize(entries=mean(entries)) |>
  pivot_wider(names_from=year, values_from=entries)

# Exercise: repeat the above, but get the per-month mean rather than per year,
# to show seasonal trends
# You should have a table with rows for each garage and columns for January, February, etc.


########################
# Weekdays vs weekends #
########################
# Some garages may be more popular on weekdays than on weekends. We need to repeat the above
# analysis, but we want a mean for weekends and a mean for weekdays. Lubridate does not have a
# weekend function, so we need to use the day of week function to make a new weekend column.
# First, we can create a day of week column.
total_entries$day_of_week = wday(total_entries$date, label=T)

# Look at how the days of week are abbreviated
unique(total_entries$day_of_week)

# now, we can recode that to weekday/weekend
total_entries$weekend = recode(total_entries$day_of_week, "Mon"="weekday", "Tue"="weekday", "Wed"="weekday", "Thu"="weekday",
                               "Fri"="weekday", "Sat"="weekend", "Sun"="weekend")

# make sure that there are no missing values
# is.na determines if a value is missing (NA), any determines if any value was found
# to be missing by is.na, ! reverses the result, and stopifnot will stop program execution
# and display an error if the input is not True
stopifnot(!any(is.na(total_entries$weekend)))

# calculate the means
group_by(total_entries, facility, weekend) |>
  summarize(entries=mean(entries)) |>
  pivot_wider(names_from=weekend, values_from=entries)

# Now, compute means by season

#######################
# Plotting            #
#######################
# This data would be more interesting if it were on a graph. To make graphs in R,
# most people use the ggplot2 library, which is part of tidyverse.

# Let us create a dataset that shows how many entries to all garages there were on each day
citywide_entries = group_by(total_entries, date) |> summarize(entries=sum(entries))

# Create a plot using the total_entries table. We define an "aesthetic" that the date
# will be the x axis, and the number of entries will be the y axis
ggplot(citywide_entries, aes(x=date, y=entries)) +
  # and add a line plot to it
  geom_line()

# wow, that's a mess - there's too much day-to-day variation to understand trends.
# let's group the data by month and year, and plot again
# the floor_date function returns the beginning of whatever period you put in - in this
# case, the beginning of the month
citywide_entries$month_year = floor_date(citywide_entries$date, unit="month")
monthly_entries = group_by(citywide_entries, month_year) |> summarize(entries=sum(entries))
ggplot(monthly_entries, aes(x=month_year, y=entries)) +
  geom_line()

# What month has the highest parking demand? Why do you think that is?

# This is an interesting plot, but it would be more interesting to see it by garage.
# We can do this by using data that is not summed up to citywide level, an then telling
# ggplot to group by the facility

total_entries$year_month = floor_date(total_entries$date, unit="month")
garage_month_entries = group_by(total_entries, facility, year_month) |>
  summarize(entries=sum(entries))

# look at the result of that call
garage_month_entries[1:20,]

# now, plot the data but tell ggplot to group by facility, use separate colors
# for each facility
ggplot(garage_month_entries, aes(x=year_month, y=entries, group=facility, color=facility)) +
  geom_line()

# which garages have more of a seasonal trend? why?
# exercise: look at weekly trends rather than monthly trends

# exercise: group by district instead of by facility (more of a challenge!)

###############
# Usage types #
###############

# The data include several usage types - monthly passes, transient users, etc.
# Exercise: first, extract all of the unique values of the usage_type column to
# see what the possibilities are.

# How many entries are from each of the usage types? What usage type is most common?

# Are the patterns of usage types different on the weekdays vs. the weekends?

# Make a plot of the monthly trend in number of entries by usage type.
