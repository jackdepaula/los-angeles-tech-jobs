#####################################################################################################
# Capstone Project          Instructor: William Yu
# By Joao de Paula, UCLA Extension Data Science Intensive
# November. 2018
#####################################################################################################


# Capstone Dot Density Maps using maptools


library(rstudioapi) # load it

# Set current path
current_path <- getActiveDocumentContext()$path 
# The next line sets the working directory
setwd(dirname(current_path ))


# The steps:

# Install R if you haven't already and load necessary packages.
# As usual, load and prepare your data.
# Calculate dot placement.
# Map results.
# Customize to your needs.


# for reference, Mike Bostock laid out the 19-step process
# to download data from American FactFinder.
# 
# The Census Bureau is great, though the user interface of the American FactFinder is somewhat onerous. Here are the nineteen steps to download the data:
#   
#   Go to factfinder2.census.gov.
# Find where it says "American Community Survey" and click "get data ?".
# Click the blue "Geographies" button on the left.
# In the pop-up, select census tract in the "geographic type" menu.
# Select California in the resulting "state" menu.
# Click "All Census Tracts within California".
# Click the "ADD TO YOUR SELECTIONS" button.
# Click "CLOSE" to dismiss the pop-up.
# Click the blue "Topics" button on the left.
# In the pop-up, expand the "People" submenu.
# Expand the "Basic Count/Estimate" submenu.
# Click "Population Total".
# Click "CLOSE" to dismiss the pop-up.
# In the table, click on the most recent ACS 5-year estimate named "TOTAL POPULATION".
# On the next page, click the "Download" link under "Actions".
# In the pop-up, click "OK".
# Wait for it to "build" your file.
# When it's ready, click "DOWNLOAD".
# Finally, expand the downloaded zip file (ACS_14_5YR_B01003.zip).
# Personally, I prefer the Census Bureau's developer API to retrieve data. It uses simple, readable URLs where you can substitute a year (2014), a FIPS code (06 for California), a census variable name (B01003_001E for total population), and other parameters to download the desired data from the command line without fighting a convoluted user interface. Census Reporter also provides a convenient interface to the data, albeit with some limitations such as dataset size.
# 
# The above map shows individual census tracts. For faster download and rendering, and to reduce antialiasing artifacts, you can merge tracts of the same color (population density interval) using topomerge. This is so effective at compressing the data that you can reasonably use block groups instead of tracts for greater resolution. For static maps, consider using geo2svg for even faster rendering.


library(dplyr)
library(maptools)

#
# Draw a dot density map
# 

# load the CSV file that represents population estimates for California Census tracts.
# California population data

acsClasses <- c("character", "character", "character", "numeric", "numeric")
calipop <- read.csv("ACS_12_5YR_B01003_with_ann.csv", stringsAsFactors=FALSE, colClasses=acsClasses)
calipop.sub <- calipop[,c(2,4)] #only use the second and fourth column, which are the Census tract geogr

calitech <- lacounty_data
calitech.17 <- filter(calitech, year == "2017") # subset year 2017
calitech.17 <- calitech.17[,c(2,4)] #only use the second and fourth column, which are the Census tract geographic id and number of tech jobs
str(calipop.sub)
str(calitech.17)
calitech.17$GEOID <- as.character(calitech.17$GEOID)
calitech.17$tech <- as.numeric(calitech.17$tech)
names(calitech.17) <- c("geoid2", "tech")

class(calipop.sub)
class(calitech.17)
cadata <- full_join(calitech.17, capolys@data, by="GEOID")
# cadata <- merge(calipop.sub, calitech.17, by = "geoid2")
# test <- merge(calipop.sub, calitech.17, by.x="geoid2", by.y="GEOID", sort=FALSE)


# The shapefile format is a common way to store data for geographic areas, namely polygons, lines,
# and points. Each tract is represented as a polygon (i.e. some shape), and the maptools package 
# provides the readShapePoly() function to load the data

# California shapefile for Census tracts
# latract.shp <- readShapePoly("./data/la-county-neighborhoods-v6/l.a. county neighborhood (v6).shp", proj4string=CRS("+proj=longlat"))
# latract.shp <- readShapePoly("data/Territory Manager Zip Codes/geo_export_a178def3-b73e-4fde-99b0-83ed418e66fd.shp", proj4string=CRS("+proj=longlat"))
latract.shp <- readShapePoly("tl_2012_06_tract/tl_2012_06_tract.shp", proj4string=CRS("+proj=longlat"))
# catract.shp <- readShapePoly("data/tl_2012_06_tract/tl_2012_06_tract.shp", proj4string=CRS("+proj=longlat"))
# capolys <- SpatialPolygonsDataFrame(latract.shp, data=as(latract.shp, "data.frame"))
capolys <- SpatialPolygonsDataFrame(latract.shp, data=as(latract.shp, "data.frame"))

head(capolys@data)

# You have two data sources. One is population for California Census tracts and the other is a
# bunch of polygons, one for each tract. You need to merge them into one data frame.
# The common variable between the two datasets is geoid2 in calipop.sub and GEOID in capolys@data.

# Notice the sort in merge() is set to FALSE. If you set it to TRUE, the function orders by the 
# first variable alphanumerically, but you want the data to maintain its original order. Then the
# variable of interest is stored as plotvar and divided by 1,000 to make one dot per 1,000 people.


# # chci <- data.frame(read_excel("./data/lacounty_zip_chci.xlsx", sheet="2009"))
# chci <- data.frame(read_excel("lacounty_zip_chci.xlsx", sheet="2012"))



# Merge datasets
# ladata <- merge(capolys@data, calitech.17, by.x="GEOID", by.y="geoid2", sort=FALSE)
cadata <- merge(capolys@data, calipop.sub, by.x="GEOID", by.y="geoid2", sort=FALSE)
# cadata <- merge(income, la17, by.x="GEOID", by.y="GEOID", sort=FALSE)
# calitech.17$GEOID <- as.numeric(calitech.17$GEOID)



head(capolys@data)
str(calitech.17)
# cadata <- as.data.frame(zip.sf.17)
head(cadata)

# plotvar <- cadata$tech07 / 1000		# One dot per 1,000 people
plotvar <- cadata$tech07

# Generate dots based on data
# It seems like the hard part of making dot density maps is figuring out where to put the dots
# and how many of them to draw. However, the dotsInPolys() function from the maptools package 
# does most of the work. The first argument provides the polygons, the second provides the 
# estimate for each polygon, and the last argument says to draw dots randomly within each polygon

# Generate random dots in polygons (e.g. census tracts)
cadots.rand <- dotsInPolys(, as.integer(plotvar), f="random")
# This gives you a data frame with coordinates for all the dots

# we draw county lines for a point of reference (even though we calculated dots based on tract-level 
# data). You could draw the tracts, but there are a lot of them and it gets cluttered pretty quickly.
# So use readShapePoly(), also from the maptools package, to load the county shapefile. Then use 
# SpatialPolygonsDataFrame() to convert the data to a data frame.

# Map boundaries
# cacounties.shp <- readShapePoly("data/california/california_county_shape_file.shp", proj4string=CRS("+proj=longlat"))
cacounties.shp <- readShapePoly("data/Territory Manager Zip Codes/geo_export_a178def3-b73e-4fde-99b0-83ed418e66fd.shp", proj4string=CRS("+proj=longlat"))

cacounties <- SpatialPolygonsDataFrame(cacounties.shp, data=as(cacounties.shp, "data.frame"))

# Set zero margins in plot window, so that map fits
par(mar=c(0,0,0,0))
plot(cacounties, lwd=0.1)

# Remember that you already calculated where all the dots go and stored the coordinates in cadots.rand.
# To plot them, you can use plot() again, but set the add argument to TRUE so that the dots are drawn 
# on top of the existing county map.

# Add dots
plot(cadots.rand, add=T, pch=19, cex=0.1, col="#00880030")