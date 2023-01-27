# Residential building permits
# Often we have multiple datasets that we want to analyze together. If there are column(s)
# that match between the datasets, we can use a "join" to create a single dataset from
# the two datasets. If the datasets have the same columns and represent different sets
# of records, we can concatenate them.

# The Census Bureau collects monthly information on the number of residential
# building permits issued by county in the Building Permit Survey: https://www.census.gov/construction/bps/
# In this exercise, we will use this data to evaluate how construction has trended
# over time in different parts of the country. We have data from 2000-2021

# We are also curious how a county's economic structure affects construction. The
# USDA Economic Research Service publishes county typologies which reflect the economic
# structure of every county in the US. We will use the 2004 data:
# https://www.ers.usda.gov/data-products/county-typology-codes.aspx

# Load libraries
library(tidyverse)
library(lubridate)

# load data
# The data are in four files: three with building permit data (one per decade),
# and one with the county typologies.

permits_2000s = read_csv("data/res_permits_2000s.csv")
permits_2010s = read_csv("data/res_permits_2010s.csv")
permits_2020s = read_csv("data/res_permits_2020s.csv")
county_types = read_csv("data/county_types.csv")

# First we need to put together all of the permits files into a single table.
# We want to put these together "vertically" - i.e. all the records for
# the 2010s will be their own rows separate from the 2000s or 2020s
# before we do this, we should check that the tables have the same format, using the
# View function

View(permits_2000s[1:20,])
View(permits_2010s[1:20,])
View(permits_2020s[1:20,])

# We can use the bind_rows function for this (i.e. "binding" many rows into a single table)
permits = bind_rows(permits_2000s, permits_2010s, permits_2020s)

# Now we need to put together the permit data with county typologies. We don't want
# to just add the county typologies to the end with bind_rows. We want to add the county
# type to each row of this dataset. For this we need to do a join. We will use a left join,
# because we don't want to lose any records from the permits dataset.

# First, we need to evaluate which columns match between the two datasets
View(county_types[1:20,])

# Now, we do a left join
data = left_join(permits, county_types, by=c("state_fips", "county_fips"))

# look at the data to see if it worked
View(data[1:20,])

# it's always a good idea to check and see if there is any missing due, due to
# rows in the permits dataset that did not match rows in the county_types dataset
sum(is.na(data$econdep))

# see if we can figure out what's going on with these
nas = filter(data, is.na(econdep))
unique(nas$county_name)

# these look like they're not actually counties. see how much building is actually happening
# in these places
sum(nas$total_buildings)
sum(data$total_buildings)

# This is not very many buildings. We can just remove them for this analysis
data = filter(data, !is.na(econdep))

# The data is split by the number of units in the structure. Create a total units
# variable
data = mutate(data, total_units = units_1unit + units_2unit + units_3or4unit + units_5plusunit)

# Exercise: make a plot of monthly and annual new construction, by economic
# typology (econdep columns). This will require using lubridate to parse the
# survey_date field, as well as group_by, summarize, and ggplot.

# Think about how this plot might be misleading.
# Let's scale by total number of housing units. This is available in the housing_units.csv,
# from the 2000 Census.
# Exercise: read that file and use a left join to join it to the data

# Now, make those plots again, but scaled by total housing units
# monthly plot

# annual plot


# Why does the "service" line get cut off?

# Figure out which county is causing the problem, and look it up on Wikipedia to find out why
# Hint: it's in Colorado. You can figure this out by looking at the state_fips field, and then
# using the lookup table at https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
# to see that FIPS code 08 is Colorado.


# Remove the NAs from the analysis

# Recreate the plots

# Do these plots tell a different story than the unscaled plots?
