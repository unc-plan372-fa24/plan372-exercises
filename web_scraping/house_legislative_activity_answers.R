# We are going to scrape today's legislative schedule from https://www.house.gov/legislative-activity.
# We want a data frame with the name of the session, the time, the location, and the
# host.
library(tidyverse)
library(rvest)
library(purrr)
library(polite)

# First, create a session object using bow(). You can put a host and path in the bow()
# function, not just the host
session = bow("https://www.house.gov/legislative-activity")

# next, retrieve the page
resp = scrape(session)

# now, find all elements that contain activity details
elements = html_elements(resp, ".session-item")

# and extract the information we're interested in
schedule = map_dfr(elements, function(element) {
  title = html_element(element, ".views-field-markup a") %>% html_text2()
  time = html_element(element, ".views-field-date .field-content") %>% html_text2()
  loc = html_element(element, ".views-field-value-2 .field-content") %>% html_text2()
  host = html_element(element, ".views-field-nothing .field-content") %>% html_text2()
  
  return(list(
    "title"=title,
    "time"=time,
    "location"=loc,
    "host"=host
  ))
})
