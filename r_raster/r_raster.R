# Raster manipulation with R
# We will repeat our Houston development and flooding example using R.
# We will be using the terra package to handle both raster and vector data.

library(tidyverse)
library(terra)

# first, we load our raster land use datasets
houston_2008 = rast("houston_2008.tif")
houston_2019 = rast("houston_2019.tif")

# next, we can map them
# the rasters ship with a color table that associates values with colors, but
# terra plotting/mapping functionality is still kind of buggy and it doesn't
# display it properly. We can just remove the color table for plotting as we
# develop our code. Making pretty maps with rasters in particular is probably
# best left to qgis for the time being.
coltab(houston_2008) <- NULL
coltab(houston_2019) <- NULL

# terra is not compatible with ggplot, so we use the plot() function built into R
plot(houston_2008)
plot(houston_2019)

# Creating a new raster layer through raster algebra just uses the normal operators
# we've seen for scalar values
# So we can create a houston change layer just like we did before
houston_change = (houston_2019 == 22 | houston_2019 == 23 | houston_2019 == 24) & 
  (houston_2008 != 22 & houston_2008 != 23 & houston_2008 != 24)

plot(houston_change)

# now, we can summarize by city.
# first, we need to read the shapefile. This is different from how we read shapefiles
# before in R, because terra has its own mechanism for reading and manipulating vector
# data that is separate from the sf library we've used in the past.
places = vect("texas_census_places.shp")

# we need to project the data to match the raster. This is quite easy in terra -
# by passing the raster itself as the second argument to project, project will
# make the projection match that raster.
places = project(places, houston_change)

# I'm not going to recommend plotting this - it is very slow

# the extract function is the equivalent of zonal statistics
# there is also an unrelated extract function in tidyverse, so we need to specify to use
# the one from terra. We use na.rm so that areas off the map, in the water, etc. are not
# treated as NAs
bycity = terra::extract(houston_change, places, sum, na.rm=T)

# bycity returns a table with two columns - an ID which is the index into the vector
# and a value. We can put this into the places dataset as a new column.
places[bycity$ID, "new_development"] = bycity$NLCD.Land.Cover.Class

# we can convert places into a tibble (table) and sort by new development descending
# to get the city with the most development
as_tibble(places) |> arrange(-new_development) |> select(NAME, new_development)

# now, we can bring in the inundation layer.
# Excercise: read and plot the inundation layer
inundation = 

# as we saw in QGIS, the inundation layer does not line up perfectly with the
# change layer. We can see that by printing them out. Note that the coordinate
# reference system, extent, and resolution differ.
inundation
houston_change

# the equivalent of the QGIS align_rasters operation is the project function. We
# previously used it for vector data, but it can be used for raster data as well.
inundation = project(inundation, houston_change)

# check the coordinate reference system again, confirm they match
inundation
houston_change

# exercise: compute the development inundation areas by city

