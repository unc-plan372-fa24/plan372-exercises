# We are going to use data on library locations, in combination with Census
# data from assignment 3, to understand whether there are demographic differences in
# the neighborhoods served by libraries.
library(tidyverse)
library(sf)  # The sf package provides functions for working with spatial data

# first, we need to read in data. We'll start by reading the Census data from Assignment
# 3, to get the hang of working with spatial datasets in R
# To read a shapefile or other spatial data format in R, we use the read_sf function.
# Fill in the path to the file.
census = read_sf("r_gis/hw3-data/orange_durham_wake_block_groups.shp")

# Spatial data in R is represented exactly the way the tabular data we've used in the past
# is. If we View the dataset, we see a table with all of the attributes, just like
# we did before. There is one extra column, geometry, which contains the spatial information
# about each row/feature.
View(census[1:20,])

# to make a map with R, we use ggplot, just like we did for graphics in the past.
# however, since maps usually use multiple layers in multiple tables, we specify
# the data in each geom_ call, rather than in the ggplot() call.
ggplot() +
  geom_sf(data=census)

# There is no demographic information with the census data. That you'll have to get
# from the separate CSV file. We can read this using read_csv like we did before
census_demographics = read_csv("r_gis/hw3-data/triangle_census.csv")

# we can just use a left join to associate this with the geographic information we already
# read. Before we do that, it's a good idea to make sure the join columns are unique in
# both datasets, so we don't wind up with duplicated rows.
any(duplicated(census$GEOID))
any(duplicated(census_demographics$GEOID))

# use the left_join function to associate the census with the demographics
census = left_join(census, census_demographics, by="GEOID")

# We get an error here, because the shapefile GEOID field is a character, but the CSV
# GEOID is a double (number)
# We can re-read the census CSV and specify that the GEOID should be a character
census_demographics = read_csv(
  "r_gis/hw3-data/triangle_census.csv",
  col_types=c(GEOID="character")
)

census = left_join(census, census_demographics, by="GEOID")

# Take a look at the Census data
View(census[1:20,])

# Now, we can plot the map again, but symbolize by a variable - in this case,
# number of African-American individuals in each block group.
ggplot() +
  geom_sf(data=census, aes(fill=race_black_african_american_alone))

# We can clean that map up a bit. First, we'll get rid of the lines, by setting
# line width to 0 (lwd = 0)
ggplot() +
  geom_sf(data=census, aes(fill=race_black_african_american_alone), lwd=0)

# We can also use a discrete rather than continuous scale
ggplot() +
  geom_sf(data=census, aes(fill=race_black_african_american_alone), lwd=0) +
  scale_fill_fermenter(palette="Blues", n.breaks=5)

# What might be misleading about this map?
# do the block groups have the same total population?

# we can calculate a percent African-American to account for population
census$pct_african_american = census$race_black_african_american_alone / census$total_population * 100

# now map it again
ggplot() +
  geom_sf(data=census, aes(fill=pct_african_american), lwd=0) +
  scale_fill_fermenter(palette="Blues", n.breaks=5)

# Now let's bring in data on libraries in Wake County, from Wake County Open Data
libraries = read_csv("r_gis/data/Libraries.csv")

# Now we convert it to spatial data
# We need to specify the coordinates and the coordinate reference system (projection),
# which is 4326 for latitude/longitude
libraries = st_as_sf(libraries, coords=c("lon", "lat"), crs=4326)

# Lets plot this to make sure it worked
ggplot() +
  geom_sf(data=census) +
  geom_sf(data=libraries, color="red")

# Now let's build a 1km buffer around the libraries. To do this, first we need
# to project the data. Unlike QGIS, we will need to do this for all layers involved
# in our analysis. We need to know the code for the NAD83 / North Carolina projection
# which is easiest to find by looking in QGIS.
census = st_transform(census, 32119)
libraries = st_transform(libraries, 32119)

# now we can buffer the libraries
libraries_buffer = st_buffer(libraries, 1000)

# Map it again to make sure it worked. Exercise: make a map showing the census tracts,
# the buffer, and the libraries
ggplot() +
  geom_sf(data=census) +
  geom_sf(data=libraries_buffer, color="blue") +
  geom_sf(data=libraries, color="red")

# Now we need to identify the Census block groups which are near the libraries. We
# use the st_intersects function to find these.
# and find parcels that intersect
intersects = st_intersects(census, libraries_buffer)

# In QGIS, select by location simply selects all parcels that intersect any feature
# in the buffer layer. R, on the other hand, returns a matrix (table), showing
# which parcels touch which buffers. We can use the apply function to turn this
# into single column. The apply function is used to apply an operation to every row or column
# of a table/matrix. The second argument indicates what to apply the operation to: 1 for rows,
# 2 for columns. There is one row for each of the features in the first layer we gave to
# st_intersects, and one column for each feature in the second. So we want to apply the "any" operation to
# each row to find out if that parcel touched any other parcel. We save that as a new column.
census$library_zone = apply(intersects, 1, any)

# now, make a map showing census block groups in library service areas
ggplot() +
  geom_sf(data=census, aes(fill=library_zone)) +
  geom_sf(data=libraries, color="red")

# and now, we can compare the demographics of the Census tracts around libraries to the
# demographics of the region as a whole, using group_by
group_by(census, library_zone) |>
  summarize(pct_white=sum(race_white_alone) / sum(total_population) * 100)

# Does this compare the demographics of the library service area to all of the Triangle?


# To find the demographics of the whole area, we can use summarize without grouping
# by any variable.
summarize(census, pct_white=sum(race_white_alone) / sum(total_population) * 100)

# The libraries are run by Wake County, so the demographics of Orange and Durham
# counties are outside their jurisdiction. Exercise: filter the data to only include
# Wake County, and repeat the above analyses.
# The COUNTYFP contains Census county FIPS codes. The code for Wake County is 183.
wake = filter(census, COUNTYFP=="183")

group_by(wake, library_zone) |>
  summarize(pct_white=sum(race_white_alone) / sum(total_population) * 100)

summarize(wake, pct_white=sum(race_white_alone) / sum(total_population) * 100)

# Another public service in Wake County is Cardinal Cycle bikeshare. Exercise: download
# the Cardinal Cycle station locations from the Raleigh Open Data Portal:
# https://data-ral.opendata.arcgis.com/datasets/ral::cardinal-cycle-stations/explore?location=35.797935%2C-78.624284%2C9.84
# and perform a similar analysis. You may need to do some data cleaning to avoid
# errors when mapping.
cardinal_cycle = read_csv("r_gis/data/Cardinal_Cycle_Stations.csv")

# There's one row where everything is NA. Remove it to avoid errors when running st_as_sf.
cardinal_cycle = filter(cardinal_cycle, !is.na(X))

cardinal_cycle = st_as_sf(cardinal_cycle, coords=c("X", "Y"), crs=4326)

# Now, we can map the stations, with Census data in the background
ggplot() +
  geom_sf(data=census) +
  geom_sf(data=cardinal_cycle)

# reproject and buffer
cardinal_cycle = st_transform(cardinal_cycle, 32119)
cycle_buffer = st_buffer(cardinal_cycle, 1000)

# Make sure the buffer worked
ggplot() +
  geom_sf(data=census) +
  geom_sf(data=cycle_buffer, color="blue") +
  geom_sf(data=cardinal_cycle, color="red")

# find the block groups near Cardinal Cycle stations
intersects = st_intersects(census, cycle_buffer)
census$cycle_zone = apply(intersects, 1, any)

# plot the selected area
ggplot() +
  geom_sf(data=census, aes(fill=cycle_zone))

# compute demographics
group_by(census, cycle_zone) |>
  summarize(pct_white=sum(race_white_alone) / sum(total_population) * 100)

summarize(census, pct_white=sum(race_white_alone) / sum(total_population) * 100)
