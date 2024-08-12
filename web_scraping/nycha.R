# Retrieving tabular data using rvest and tidyverse
# If your data is in an HTML table already (<table>, <tr>, <td>, <th>, <thead>, <tfoot> tags),
# it is extremely easy to extract it into a data frame with a single function.
# We will extract NYC Housing Authority projects in Staten Island from this page:
# https://en.wikipedia.org/wiki/List_of_New_York_City_Housing_Authority_properties

# first, as always, we load the libraries we need
# map_dfr is provided by purrr, and we won't be using that function, so no need to load
# that library

library(tidyverse)
library(rvest)
library(polite)

# first, establish a session
session = bow("https://en.wikipedia.org/wiki/List_of_New_York_City_Housing_Authority_properties")

# now, retrieve the page
resp = scrape(session)

# Find the table. There are a lot of ways to do this, but searching by the name
# will make the script somewhat robust in case the order of tables on the page changes
# in the future, though design changes may still cause it to break - web scraping scripts
# are very fragile, and often don't work after a while due to website changes, even if
# the site still looks roughly the same. We then select the table immediately following the
# h3 element containing "Staten Island". We need to put Staten Island in quotes
# because it contains a space; we choose single quotes since the entire selector is
# enclosed in double quotes.
# We pipe the table element into html_table() to turn it into a dataframe
table = html_element(resp, "h3:contains('Staten Island') + table") |> html_table()

View(table)
