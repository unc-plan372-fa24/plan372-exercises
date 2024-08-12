# PLAN 372 Exercise 2: Data cleaning
# In this exercise, we'll be working with data from automated bicycle counters in North Carolina,
# from the North Carolina Non-Motorized Volume Data Portal (https://itre.ncsu.edu/focus/bike-ped/nc-nmvdp/)
# We have hourly data from December 2021-July 2022, on a large number of bicycle paths and lanes throughout North Carolina.

# This dataset is messier than the SFpark data; we'll have to do some cleaning in order
# to use it.

# load libraries
library(tidyverse)
library(lubridate)

# read the data, and look at the first 20 rows
data = read_csv("ped_bike_counters/data/ncnmvdp.csv")
View(data[1:20,])

# hmm, that looks weird - where are all the column names? It looks like the first two
# rows of the file have the date of the extract. With read_csv, we can skip the first
# two rows, and start with the third that has the column names.
data = read_csv("ped_bike_counters/data/ncnmvdp.csv", skip=2)
View(data[1:20,])

# First, let's graph monthly bike and pedestrian numbers for the American Tobacco Trail
# in Downtown Durham and crossing I-40. We need to
# figure out where the data for these are.
# We know each sensor is in a column, let's print out the column names to see
# which column the data might be in
names(data)

# These column names are going to be a pain to work with. We can rename them easily.
data = rename(data,
              att_downtown_bike="ATT Downtown, Bicycles (Q2) ATT Downtown, Bicycles",
              att_downtown_ped="ATT Downtown, Pedestrians (Q2) ATT Downtown, Pedestrians",
              att_i40_bike="ATT I-40 Bridge, Bicycles (Q2) ATT I-40 Bridge, Bicycles",
              att_i40_ped="ATT I-40 Bridge, Pedestrians (Q2) ATT I-40 Bridge, Pedestrians"
              )

# The data currently have hourly records. We want a monthly plot.
# First, create a column with the month that we can group by. To do that, we parse
# the date, then use floor_date to get the beginning of the month.
# lubridate has date parsing functions names things like mdy_hms for
# month/day/year, hours:minutes:seconds format. Inspect the Time column and figure
# out which function to use here.
data$Time = mdy_hm(data$Time)

# Use the floor_date function to get the start of each month
data$month_year = floor_date(data$Time, unit="month")

# Unlike the SFpark data, different trails are in different columns here, so we
# do not need to include them in the group_by. Group by groups rows together.
monthly_bike_ped = group_by(data, month_year) |>
  summarize(
    att_downtown_bike=sum(att_downtown_bike),
    att_downtown_ped=sum(att_downtown_ped),
    att_i40_bike=sum(att_i40_bike),
    att_i40_ped=sum(att_i40_ped)
  )

monthly_bike_ped

# Now we can plot the data
# Unlike the SFpark data where we had to tell ggplot to group and plot different rows
# differently, here we want to plot several columns
# leaving out the y axis as several different variables are used for the y axis
ggplot(monthly_bike_ped, aes(x=month_year)) +
  # select a specific column for the line plot. All unique names in "color" will
  # be assigned a color and also labeled
  geom_line(aes(y=att_downtown_bike, color="American Tobacco Trail, downtown, bikes")) +
  geom_line(aes(y=att_downtown_ped, color="American Tobacco Trail, downtown, pedestrians")) +
  geom_line(aes(y=att_i40_bike, color="American Tobacco Trail, I-40, bikes")) +
  geom_line(aes(y=att_i40_ped, color="American Tobacco Trail, I-40, pedestrians"))


# There are some gaps in our chart - why might this be?

# These data are collected by sensors out in the world, which can break, run out
# of battery, etc. In R, missing data are usually represented by an NA value (some
# datasets may use 0, -1, -999, etc to signify missing data). When we do math with an
# NA value, the result is NA - to avoid you accidentally using data that you thought
# was not missing but is. So if any hourly period is missing during the entire month,
# the entire month count will be considered NA. R has an is.na function to check for NAs.

# How many of the ATT I-40 bridge pedestrian observations are NA?
sum(is.na(data$att_i40_ped))

# How about the bike observations
sum(is.na(data$att_i40_bike))

# Are the same observations missing for both?

all(is.na(data$att_i40_ped) == is.na(data$att_i40_bike))


# When are these missing data points? We can use a histogram to find out
missings = group_by(data, month_year) |>
  summarize(proportion_missing = mean(is.na(att_i40_bike)))
ggplot(missings, aes(x=month_year, y=proportion_missing)) +
  geom_col()

# Exercise: Repeat the same analysis of how many values are NA for downtown.
# Plot the results. If the pedestrian and bike missingness are different, make separate plots.

sum(is.na(data$att_downtown_ped))
sum(is.na(data$att_downtown_bike))
all(is.na(data$att_downtown_ped) == is.na(data$att_downtown_bike))

missings = group_by(data, month_year) |>
  summarize(proportion_missing = mean(is.na(att_downtown_bike)))
ggplot(missings, aes(x=month_year, y=proportion_missing)) +
  geom_col()

missings = group_by(data, month_year) |>
  summarize(proportion_missing = mean(is.na(att_downtown_ped)))
ggplot(missings, aes(x=month_year, y=proportion_missing)) +
  geom_col()


# What should we do?
# In the case of the I-40 sensor, the missing data is a small amount of data, so we
# might reasonably just replace these records with zeros. This will result in the
# readings being a little lower during the March 2022, but we can live with that for now.
# In other cases, various "imputation" techniques can be used to try to guess
# what the missing data might have been. Getting into these is beyond the scope of this exercise.
# We'll just apply the same technique to the downtown sensor, and understand that lower demand from
# pedestrians downtown during December 2021 may be due to data quality issues.

# We can remove missing data with the replace_na function from tidyverse. Here, I am
# putting the data into new variables, with an _complete suffix. We could also give them
# the same names if we wanted to overwrite the original data with the data where the NA
# values are replaced with 0. This would overwrite the data in memory, but does not change
# the file on your disk-so if you ran the read_csv line above again, you would have the original
# dataset.
data = mutate(
  data,
  att_i40_bike_complete=replace_na(att_i40_bike, 0),
  att_i40_ped_complete=replace_na(att_i40_ped, 0),
  att_downtown_bike_complete=replace_na(att_downtown_bike, 0),
  att_downtown_ped_complete=replace_na(att_downtown_ped, 0)
  )

# Exercise: Plot the data again. Are the gaps gone?

monthly_bike_ped = group_by(data, month_year) |>
  summarize(
    att_downtown_bike_complete=sum(att_downtown_bike_complete),
    att_downtown_ped_complete=sum(att_downtown_ped_complete),
    att_i40_bike_complete=sum(att_i40_bike_complete),
    att_i40_ped_complete=sum(att_i40_ped_complete)
  )

ggplot(monthly_bike_ped, aes(x=month_year)) +
  # select a specific column for the line plot. All unique names in "color" will
  # be assigned a color and also labeled
  geom_line(aes(y=att_downtown_bike_complete, color="Downtown, bikes")) +
  geom_line(aes(y=att_downtown_ped_complete, color="Downtown, pedestrians")) +
  geom_line(aes(y=att_i40_bike_complete, color="I-40, bikes")) +
  geom_line(aes(y=att_i40_ped_complete, color="I-40, pedestrians"))

# Which part of the trail is more popular?
# Our graphs so far separate out bicycles and pedestrians. Let's create new columns
# for total users for I-40 at the bridge and downtown.
data = mutate(data, att_i40_all=att_i40_bike + att_i40_ped,
              att_downtown_all=att_downtown_bike + att_downtown_ped)

# Exercise: Make a line graph showing the total traffic on the two parts of the trail
# You will need to create columns att_i40_all_complete and att_downtown_all_complete

data = mutate(data,
              att_downtown_all_complete=replace_na(att_downtown_all, 0),
              att_i40_all_complete=replace_na(att_i40_all, 0)
              )

monthly_totals = group_by(data, month_year) |>
  summarize(
    att_downtown_all_complete=sum(att_downtown_all_complete),
    att_i40_all_complete=sum(att_i40_all_complete)
  )



ggplot(monthly_totals, aes(x=month_year)) +
  geom_line(aes(y=att_downtown_all_complete, color="Downtown")) +
  geom_line(aes(y=att_i40_all_complete, color="I-40"))

##############################
# Plotting: other geometries #
##############################
# So far, we have used only geom_line. But there are many other geom_ options.
# For instance, geom_col makes a bar plot, geom_histogram a histogram, and geom_boxplot
# a boxplot.

# Let's make a bar plot by time of day, for both sensors combined
data$hour = hour(data$Time)
hour_totals = group_by(data, hour) |>
  summarize(total = mean(att_downtown_all + att_i40_all))

ggplot(hour_totals, aes(x=hour, y=total)) +
  geom_col()

# Well, that didn't work. Why? Look at the warning message from the ggplot command,
# and the hour_totals dataset.
# We can remove the missing values by adding na.rm=T to the mean call
# You can add na.rm=T to most R function calls to tell it to ignore NA values


hour_totals = group_by(data, hour) |>
  summarize(total = mean(att_downtown_all + att_i40_all, na.rm=T))
  

ggplot(hour_totals, aes(x=hour, y=total)) +
  geom_col()

# Question - what do the numbers on the y axis represent?
# Exercise: modify the graph to show pedestrians per hour.

hour_totals = group_by(data, hour) |>
  summarize(ped = mean(att_downtown_ped + att_i40_ped, na.rm=T))

ggplot(hour_totals, aes(x=hour, y=ped)) +
  geom_col()

# Exercise: create a plot that shows average activity by day of week (hint: the wday function is helpful)

# label=T gives us the names of the days, not the numbers
data$dayofweek = wday(data$Time, label=T)
day_totals = group_by(data, dayofweek) |>
  summarize(total = mean(att_downtown_all + att_i40_all, na.rm=T))

ggplot(day_totals, aes(x=dayofweek, y=total)) +
  geom_col()

#############################
# Filtering by day and time #
#############################
# Due to weather variation and weekly schedules, there is likely to be a large
# spread in the number of trail users by day. These are likely to be very large
# seasonally, but even within a shorter time period they are likely to exist.
# Let's create a boxplot of daily pedestrian trail usage in January and February
# of 2022. There were several major snowstorms (by Triangle standards anyhow) during these
# months, so we should see a fairly wide spread.

# First, we need to filter our data to just our time period of interest. We can do
# this using the filter function we used in the SFpark example, but with multiple
# conditions and using inequality rather than equality conditions. We need to create
# date objects to compare to, which we do using the ymd_hms function and a literal
# string (in double quotes) that will be parsed as a date.
jan_feb = filter(data, Time >= ymd_hms("2022-01-01 00:00:00") & Time <= ymd_hms("2022-02-28 23:59:59"))

# it's always good to look at the data and make sure your filtering worked
summary(jan_feb$Time)


# now, we need to create a date field so we can get daily totals
jan_feb$date = date(jan_feb$Time)

day_totals = group_by(jan_feb, date) |>
  summarize(total=sum(att_downtown_all, na.rm=T))

ggplot(day_totals, aes(y=total)) +
  geom_boxplot()

# Exercise: repeat the above analysis for the I-40 bridge, additionally looking only at
# the daytime hours between 7 AM and 5 PM

jan_feb_day = filter(jan_feb, hour(Time) >= 7 & hour(Time) <= 17)

daytime_totals = group_by(jan_feb_day, date) |>
  summarize(total=sum(att_i40_all, na.rm=T))

ggplot(daytime_totals, aes(y=total)) +
  geom_boxplot()

