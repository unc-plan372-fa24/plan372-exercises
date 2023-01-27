# Missouri dealership sales data
# The State of Missouri releases sales data from car dealerships at
# https://dor.mo.gov/public-reports/dealers_franchise_report.txt
# Download this file, and we'll work through reading it into R. It's in a non-standard
# format intended to be read by humans, but we can use regular expressions to read
# it into a format we can analyze.

library(tidyverse)

# first, we will read the text file into a single string. We are using read_file here
# which will read the file as a single string rather than a table like read_csv
str = read_file("dealers_franchise_report.txt")

# Now, we need to split the file into a list/vector, with one item for each dealership
# we can do this with the str_split function, which takes the string, and a regular
# expression that identifies where we want it to be split. We want to write a regular
# expression that will match the DEALER# ****** delimiter between dealers. Since * is a special
# character in regular expressions, we need to _escape_ it by places two backslashes (\\)
# before it. This tells R to use it to match a *, not to modify the character before it

# str_split returns a list of lists - with one list for the components of each input
# string. Since we only had one input string, we will retrieve the first list. [[ ]] in R
# is like [], but retrieves only a single value (sometimes referred to as a scalar).
dealers = str_split(str, "DEALER# \\**")[[1]]

dealers[1:3]

# The first item in the list of dealerships is the header for the whole file. We can
# discard that.
# By indexing by a negative vector, we remove those records.
dealers = dealers[-c(1)]

# the dealership records are kind of a mess, because all the lines have been smooshed
# together. We can split each dealership record into lines, again with str_split. This
# time, we don't use [[1]], because we want a list of lists - a list whose elements are
# lists of all the lines associated with each dealership.
# \n is the code in R and most other programming languages for a newline (return)
dealers = str_split(dealers, "\n")

# we want to extract the following information for each dealership:
# dealer number
# Dealership name
# dealership phone number
# New, used, and total cars ("units") sold in each year
# The table should contain the columns dealer_id, dealer_name, dealer_phone, year, new_units, used_units

# We will start by parsing the information for a single dealership, as this will be easier to work with.
# Then, we'll move that code into map_dfr, like we did for web scraping, to apply it to each dealership record.

# get the first dealership
dealer = dealers[[1]]
dealer

# first, we want to find the first line, which contains the dealer name, dealer id, and phone number.
dealer_info_line = first(dealer[str_detect(dealer, "^[[:whitespace:]]*D[[:digit:]]+.*PHONE: [[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4}")])

# now, write a regular expression that will extract the dealer number, the dealer name, and the phone
# number from the dealer_info line
dealer_info = str_match(dealer_info_line, "^[[:whitespace:]]*(D[[:digit:]]+)[[:whitespace:]]*(.*)[[:whitespace:]]*PHONE: ([[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4})")

# now, we need to extract the per-year sales from the remaining lines
# We need to write a regular expression that will detect the lines, and then use (a variation of)
# it to extract the lines with years
count_lines = dealer[str_detect(dealer, "^[[:whitespace:]]*UNITS SOLD IN [[:digit:]]{4}[[:whitespace:]]+NEW:[[:whitespace:]]*[[:digit:]]+[[:whitespace:]]+USED:[[:whitespace:]]*[[:digit:]]+[[:whitespace:]]+TOTAL:[[:whitespace:]]*[[:digit:]]+[[:whitespace:]]*$")]

# now, extract the relevant information
counts = str_match(count_lines, "^[[:whitespace:]]*UNITS SOLD IN ([[:digit:]]{4})[[:whitespace:]]+NEW:[[:whitespace:]]*([[:digit:]]+)[[:whitespace:]]+USED:[[:whitespace:]]*([[:digit:]]+)[[:whitespace:]]+TOTAL:[[:whitespace:]]*([[:digit:]]+)[[:whitespace:]]*$")

counts

# give the counts column names
colnames(counts) = c("all", "year", "new", "used", "total")
# convert the counts to a tibble (tabular data)
counts = as_tibble(counts)

# we now have a table (counts) with all of the information about the counts. We just need
# to add the dealer name, number, and phone to it to have our final dataset
counts$dealer_id = dealer_info[1, 2]
counts$dealer_name = dealer_info[1, 3]
counts$dealer_phone = dealer_info[1, 4]

# and we can get rid of the "all" column with the full matched line
counts = select(counts, -all)

View(counts)

# now, just like when we were working with web scraping, we'll use map_dfr to apply the code
# we wrote above to all dealerships

dealer_table = map_dfr(dealers, function (dealer) {
  # first, we want to find the first line, which contains the dealer name, dealer id, and phone number.
  dealer_info_line = first(dealer[str_detect(dealer, "^[[:whitespace:]]*D[[:digit:]]+.*PHONE: [[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4}")])
  
  # now, write a regular expression that will extract the dealer number, the dealer name, and the phone
  # number from the dealer_info line
  dealer_info = str_match(dealer_info_line, "^[[:whitespace:]]*(D[[:digit:]]+)[[:whitespace:]]*(.*)[[:whitespace:]]*PHONE: ([[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4})")
  
  # now, we need to extract the per-year sales from the remaining lines
  # We need to write a regular expression that will detect the lines, and then use (a variation of)
  # it to extract the lines with years
  count_lines = dealer[str_detect(dealer, "^[[:whitespace:]]*UNITS SOLD IN [[:digit:]]{4}[[:whitespace:]]+NEW:[[:whitespace:]]*[[:digit:]]+[[:whitespace:]]+USED:[[:whitespace:]]*[[:digit:]]+[[:whitespace:]]+TOTAL:[[:whitespace:]]*[[:digit:]]+[[:whitespace:]]*$")]
  
  # now, extract the relevant information
  counts = str_match(count_lines, "^[[:whitespace:]]*UNITS SOLD IN ([[:digit:]]{4})[[:whitespace:]]+NEW:[[:whitespace:]]*([[:digit:]]+)[[:whitespace:]]+USED:[[:whitespace:]]*([[:digit:]]+)[[:whitespace:]]+TOTAL:[[:whitespace:]]*([[:digit:]]+)[[:whitespace:]]*$")
  
  counts
  
  # give the counts column names
  colnames(counts) = c("all", "year", "new", "used", "total")
  # convert the counts to a tibble (tabular data)
  counts = as_tibble(counts)
  
  # we now have a table (counts) with all of the information about the counts. We just need
  # to add the dealer name, number, and phone to it to have our final dataset
  counts$dealer_id = dealer_info[1, 2]
  counts$dealer_name = dealer_info[1, 3]
  counts$dealer_phone = dealer_info[1, 4]
  
  counts = select(counts, -all)
  
  return(counts)
})

View(dealer_table)

# Using regular expressions for data cleaning
# The dealer names likely include a lot of whitespace at the end of them (depending on how exactly
# you wrote your regular expression), and some end with INC, ", INC", "LLC", "LLC." or some variation
# of this (with or without comma after name, with or without period). We want to clean up these names
# in order to have the names in our dataset match the names people use to refer to these dealerships.
# We can use the str_replace function to replace a regular expression with a value. Write a regular expression
# to match the whitespace and any LLC/INC at the end of the dealer name. To match either LLC or INC, you can
# use "alternation" - two patterns in parentheses with a pipe symbol | (above the enter key) between them
# will match one or the other of the patterns. Recall also that two backslashes before a special character
# such as . will match that character, rather than whatever the special character would normally match.
# The empty string at the end of str_replace indicates to replace with nothing.
dealer_table = mutate(dealer_table, dealer_name=str_replace(dealer_name, ",? (LLC|INC)\\.?[[:whitespace:]]*$", ""))

# Exercise: plot the number of used, new, and total cars sold over time in the state of Missouri

# first, make the years and counts numeric
dealer_table = mutate(dealer_table, year=as.numeric(year), total=as.numeric(total), new=as.numeric(new), used=as.numeric(used))

# Then, group by year
byyear = group_by(dealer_table, year) %>% summarize(new=sum(new), used=sum(used), total=sum(total))

# then plot
ggplot(byyear, aes(x=year)) +
  geom_line(aes(y=new, color="New")) +
  geom_line(aes(y=used, color="Used")) +
  geom_line(aes(y=total, color="Total"))

# Exercise: what dealership sold the most cars in 2021?
filter(dealer_table, year==2022) %>%
  group_by(dealer_id) %>%
  summarize(total=sum(total), dealer_name=first(dealer_name)) %>%
  arrange(-total)
