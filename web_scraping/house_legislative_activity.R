# We are going to scrape today's legislative schedule from https://www.house.gov/legislative-activity.
# We want a data frame with the name of the session, the time, the location, and the
# host.
library(tidyverse)
library(rvest)
library(purrr)
library(polite)

# First, create a session object using bow(). You can put a host and path in the bow()
# function, not just the host
session = 

# next, retrieve the page
resp = 

# now, find all elements that contain activity details
elements = 

# and extract the information we're interested in
schedule = map_dfr(elements, function(element) {

})
