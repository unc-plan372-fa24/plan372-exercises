# We can export data from QGIS as SVG and analyze it using the same tools we've
# used in R in the past.

library(tidyverse)

# read in your data file that you saved from qgis
data = read_csv("chapel_hill_flooding_fromqgis.csv")

# examine the data
View(data[1:20,])

# It's hard to do this in QGIS, but it's a good idea to look at our join field and make
# sure everything was unique
data$PIN[duplicated(data$PIN)]

# it's probably a good idea to go back to QGIS to see if these are simply duplicates,
# or something else.

# it looks like these aren't duplicates, but parcels that are split into multiple pieces e.g. by a road
# The permits dataset, though, has records for the entire parcel. We'll get an incorrect
# answer if we use this, because we'll be essentially duplicating the development that occurred
# for each part of a parcel. So we should group the parcels and sum up the areas, but not sum
# up the development costs, so we have one row per parcel that accurately reflects development
parcels = group_by(data, PIN) %>%
  summarize(
    Shapearea=sum(Shapearea),  # we want to sum up the areas of the parts of the parcel
    parcel_permits_total_construction_cost=first(parcel_permits_total_construction_cost),
    
    # if any part of the parcel is in the flood area, we want to call it a flood area
    floodarea=any(floodarea==1)
  )


# use the filter function to remove non-Orange County parcels by filtering
# for non-NA n_permits
parcels = filter(parcels, !is.na(parcel_permits_total_construction_cost))

# now, use group_by and summarize to compute development intensity inside and
# outside the flood zone. Think about whether you are computing development intensity
# per square foot, or per square foot per lot





