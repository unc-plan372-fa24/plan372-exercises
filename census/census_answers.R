# Often, we will want to retrieve Census data for use in projects. The Census website
# has various download interfaces, but they can be difficult to use. The tidycensus
# package provides an easier-to-use R alternative.

# first, load libraries, as always
# also loading the sf library, as we will be doing some mapping towards the end
library(tidyverse)
library(tidycensus)
library(sf)

# before we start, we need to install our Census API key that we got via email.
# You only need to do this once, and you shouldn't put this function call in a code
# file, for security reasons. Using the console, run
#   census_api_key("YOUR_API_KEY", install=T)
# You may need to restart R for the API key to be available (Session -> Restart R)

# Full usage of tidycensus is described here: https://walker-data.com/tidycensus/articles/basic-usage.html
# tidycensus has two main functions, get_decennial which retrieves decennial census data,
# and get_acs which retrieves one or five year ACS data.
# Both of these functions require several parameters: the area of the country to retrieve,
# the level of spatial aggregation, the year, and the variables. get_acs additionally
# requires specifying 1 or 5-year data (5-year is usually what you want as it has less
# sampling error and more variables).

# First, let's retrieve some information from the 2019 5-year ACS.
# To find our data, we first need to find the variable codes for the variables we want.
# running load_variables(year, dataset) will return a table of all the variables for that
# dataset. acs1 and acs5 correspond to 1 and 5 years ACS; pl, sf1, sf2 represent different
# subsections of the decennial census.
acs_vars = load_variables(2019, "acs5")

# This list of variables is huge, and viewing it in RStudio is a pain. I usually
# save it to a file and view it in Excel.
write_csv(acs_vars, "acsvars.csv")


# Let's retrieve ACS data on means of transportation to work by North Carolina county
commute = get_acs(
  geography="county",  # could be tract, block group, etc.
  variables=c(
    "total_mode"="B08301_001",
    "drove_alone"="B08301_003",
    "transit"="B08301_010",
    "taxi"="B08301_016",
    "motorcycle"="B08301_017",
    "bicycle"="B08301_018",
    "walk"="B08301_019",
    "other"="B08301_020",
    "wfh"="B08301_021",
    "total_ttime"="B08303_001",
    "ttime_60_89"="B08303_012",
    "ttime_90plus"="B08303_013"
  ),
  year=2019,
  state="NC",
  survey="acs5",
  output="wide"
)

View(commute)

# Exercise: also retrieve travel time to work

#######################
# Mapping Census data #
#######################

# The data we retrieved from the Census doesn't have any spatial information. If we
# want to map it or do any spatial analysis, we need to join it with spatial information.
# Spatial information is also available from the Census Bureau, through their TIGER/Line
# site: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
# We will create two maps: one of working from home prevalence, and one of long commutes (over 60 minutes).

# Download 2019 US County shapefile from that site, and load it into R.
counties = read_sf("tl_2019_us_county.shp")

# we want to filter it to just NC counties. We can use the state FIPS (STATEFP) code to do that.
# The code for North Carolina is 37
nc_counties = filter(counties, STATEFP==37)

# Join the datasets together
nc_counties = left_join(nc_counties, commute, by="GEOID")

# create a variable for WFH share
nc_counties$wfh_share = nc_counties$wfhE / nc_counties$total_modeE

# project the data to North Carolina State Plane
nc_counties = st_transform(nc_counties, 32119)

# and map WFH share
ggplot() +
  geom_sf(data=nc_counties, aes(fill=wfh_share)) +
  scale_fill_distiller()

# Exercise: map percent of population with commutes over 60 minutes

nc_counties$hour_commute_share = (nc_counties$ttime_60_89E + nc_counties$ttime_90plusE) / nc_counties$total_ttimeE

ggplot() +
  geom_sf(data=nc_counties, aes(fill=hour_commute_share)) +
  scale_fill_distiller()
