# In this script, we will read and perform some basic analysis of petroleum product
# pricing data from the Energy Information Administration.
# In our previous exercises, I provided the data already in CSV format, and in some
# cases I had done some cleaning of the data before sharing it with you. When you're
# working on your own projects, data will often be much less clean, with various
# idiosyncracies to deal with. While CSV is the de facto standard for data exchange
# in data science, many governments and businesses use Excel for creating and managing
# data.

# In this exercise, we will use US Energy Information Administration data on petroleum
# product prices to look at how prices have changed over time. This data is in Excel format,
# and we will have to write some special code to deal with the format of the data.

# First, we need to get the data. Since this exercise is about finding data, I have not
# provided the data for you. Instead, go to the Energy Information Administration's
# web site (eia.gov) and find the Spot Prices for Petroleum and Other Liquids page by clicking
# Sources and Uses, Petroleum & other liquids, then under the data tab find Prices, then
# spot prices. Download in Excel format by clicking "Download Series History", and save the
# Excel file. You'll need to reference the file from R, so I recommend saving it in the
# oil directory of your plan372-exercises directory. That way you can easily reference it
# using a relative path (relative to your R project) instead of a full path to where
# the file specifically is on your computer.

# Next, we need to load libraries. As always, we'll load the tidyverse library.
# We also need to load the readxl library to read in the Excel formatted file.
# This library is automatically installed when we load tidyverse, but we need to load
# it separately if we want to read Excel files.

library(tidyverse)
library(lubridate)
library(readxl)

# Next, we need to load our data. In the past we've used read_csv to do this, but this
# data is not in CSV format, so we'll use the reas_excel function to do so. Depending on
# where you saved your data file, you may need to adjust the path here.
data = read_excel("oil/PET_PRI_SPT_S1_D.xls")

# Now, look at the data, using View or by double-clicking on it in the variables
# pane.

# That doesn't really look like data. This is fairly common with Excel spreadsheets, as they
# can contain lots of things other than just a single table of data like a CSV. Open
# the file up in Excel to take a look at what's going on.

# The first page of the Excel spreadsheet is a table of contents, with the actual data
# in the other sheets. For this exercise, we'll compare the prices of conventional and
# RBOB gasoline. RBOB gasoline is a special, reformulated blend of gasoline designed to
# reduce smog, which is required in certain areas. First, we need to figure out which sheets
# these data are on, then we can read them using read_excel and specifying a sheet.

# first, read conventional gasoline. Update sheet to be correct.
conv = read_excel("oil/PET_PRI_SPT_S1_D.xls", sheet="Data 2")

# Now take a look at that dataset.

# That still doesn't look right. If we look in Excel, we see there are two rows of metadata
# before the column names in the Excel sheet. This is pretty common in Excel. We can skip those
# with the skip argument to read_excel(). read_csv also has a skip argument, if you ever need it.
conv = read_excel("oil/PET_PRI_SPT_S1_D.xls", sheet="Data 2", skip=2)

# Rename the columns to be easier to work with. We'll call them ny_conventional and gulf_conventional.


# Read in the rbob sheet also. You should double check to see if the format is the same

# Rename the column to la_rbob

# We want to get conventional and RBOB into the same data frame, but there's no guarantee
# they were recorded on the same dates, so a direct merge may not help us. If RBOB was
# recorded on one day, and conventional the next, though, we still want to be able to compare
# them. Instead, we'll create a yearly average, and plot that.

# Compute two new datasets, with yearly averages, based on conv and rbob. Call them
# conv_annual and rbob_annual


# Join the datasets together
# here we are using a full join (called an outer join in most other languages)
# to include all months that are in either the conv_monthly or rbob_monthly datasets.
combined = full_join(conv_annual, rbob_annual, by="year")

# Plot the three on the same plot, with a legend

