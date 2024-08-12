# For HW 2, you need to be able to plot a histogram and compute the number of rows
# in each group of a group_by. I switched out some exercises this year, and didn't
# realize that I lost the exercise where I went over how to do this. My apologies, and
# thank you to the students who reached out to me to point it out. This script will
# demonstrate how to create a histogram and how to get the number of observations in
# a group.

# First, load the tidyverse
library(tidyverse)

# for this demonstration, I'm going to use the mtcars dataset that's built in to R
data(mtcars)

# to create a histogram, you use ggplot and map the variable you want to create the
# histogram of to the x axis. You don't map anything to the y axis; the y axis is
# determined by the density of observations. You then add a geom_histogram.
# This shows a histogram of fuel economy for 1974-era new cars (things have gotten
# better since then)
ggplot(mtcars, aes(x=mpg)) +
  geom_histogram()

# To count the number of observations in each group of a groupby, you can use the n()
# function inside summarize. n() returns the number of observations in each group.
# This counts the number of cars with 4, 6, and 8 cylinders in the mtcars dataset
group_by(mtcars, cyl) |>
  summarize(number=n())
