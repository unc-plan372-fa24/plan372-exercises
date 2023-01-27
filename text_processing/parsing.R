# We'e seen that we can use regular expressions to determine if text matches a pattern
# However, sometimes we need to extract information from text. In this exercise we have
# a list of courses that can be used as electives in the data science minor at UNC. We
# want to parse out the department and course number for each one to determine the relative
# representation of departments in the minor program.

# Load our libraries
library(tidyverse)

data = read_csv("courses.csv")

View(data)

# The first step is to build a regular expression that will match the information
# we want, and check that it matches.
all(str_detect(data$course, "^[[:alnum:]]+ [[:digit:]]+$"))

# To extract the department and name, we need to learn about a new regular expression
# feature and a new R function. The regular expression feature is capturing groups:
# when you enclose a part of a regular expression in (), R will capture the part of
# the string that matched that part of the regular expression and return it separately.
# str_extract is the R function that is used to return the text that matched and not
# just whether or not the string matched. Run this without saving it to a variable
# to see what the output looks like.
str_match(data$course, "^([[:alnum:]]+) ([[:digit:]]+)$")

# We have three unnamed columns - one with the total match, and one for each of our matches.
# We need to assign the latter two columns to new columns in our data frame. The syntax for this
# is a little confusing: we need to select the two columns we want, and specify both of the 
# column names we want to assign them to.
data[,c("dept", "number")] = str_match(data$course, "^([[:alnum:]]+) ([[:digit:]]+)$")[,2:3]

data

# The course number has been stored as text (<chr>). We can convert it to a number.
data = mutate(data, number=as.numeric(number))

data

# The data also includes the number of units for the course. Extract the number from
# that column.