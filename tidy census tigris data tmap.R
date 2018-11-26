#####################################################################################################
# Capstone Project          Instructor: William Yu
# By Joao de Paula, UCLA Extension Data Science Intensive
# November. 2018
#####################################################################################################

# proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
# meuse.utm <- spTransform(meuse, CRS("+proj=utm +zone=32 +datum=WGS84"))

setwd("C:/Users/joao_/Dropbox/Data Science/UCLA/Git/los-angeles-tech-jobs")

library(sf)             # ...................... reading, writing and wangling sf class for map
library(tidyverse)      #....................... R packages for data
library(htmltools)      #....................... converting some data object to HTML and js
library(leaflet)        # .......................  interactive map
library(rmapshaper)     # ........................ resizing the shapefile
library(tidycensus)     # .......................... getting data from Censues
library(tigris)         # .......................... getting the shape file from United States Census
library("tmap") # thematic visualization, with a focus on mapping the spatial distribution of data attributes
library("tmaptools")
library(readxl)
library(dbplyr)
library(data.table) # data.table as an advanced version of data.frame #Fread
library(rgdal)



# install tigris with sf support
# devtools::install_github("walkerke/tigris")
# https://walkerke.github.io/tigris-webinar/#3
# install.packages("tmap")
# install.packages("tmaptools")
# webshot::install_phantomjs() #save static version of map
# install.packages("shinycssloaders")
# install.packages("shinythemes")
# install.packages("tidycensus")
# install.packages("rmapshaper")

# Leaflet for R
# https://rstudio.github.io/leaflet/

# SF
# Tidy spatial data in R: using dplyr, tidyr, and ggplot2 with sf
# http://strimas.com/r/tidy-sf/


###############################################
#
# Set working directory / Load libraries / Load Data
#
###############################################

lacounty_data  <- fread("./data/lacountytech.csv")

masterzip <- data.frame(read_excel("./data/MasterZipCodes.xlsx"))
names(masterzip) <- c("zipcode", "name")

# Dowload shape files for county from LA Data Portal
# https://data.lacounty.gov/Geospatial/ZIP-Codes/65v5-jw9f
zips.shp <- readOGR(("./data/zip-codes/geo_export_1887acad-adf8-4181-8c7d-50a16a3435f2.shp"))
head(zips.shp@data)
masterzip$zipcode <- as.factor(masterzip$zipcode)
str(masterzip)
head(masterzip)
zip.shp <- inner_join(zips.shp@data, masterzip, by = "zipcode")
dim(zip.shp)
class(zip.shp)
head(zip.shp)

cnty.zips <- as.data.frame(zips.shp$zipcode)
names(cnty.zips) <- c("GEOID")
str(lacounty_data)
lacounty_data$GEOID <- as.character(lacounty_data$GEOID) 
str(cnty.zips)
lacounty_data <- inner_join(cnty.zips, lacounty_data, by = "GEOID")

# Subset for 2017
df.17 <- lacounty_data %>% filter(year == 2017)
cnty.zips.17 <- cnty.zips

# 
# is.element(cnty.zips.17$GEOID, df.17$GEOID)

#line up between our data and the shapefile
is.element(cnty.zips.17$GEOID, df.17$GEOID)

df.17 <- rbind(df.17, c(91023, 0, 2017,0,0,0,0,0))
df.17 <- df.17[order(df.17$GEOID),]

# Remove missing data # 191
# cnty.zips.17 <- subset(cnty.zips.17, is.element(cnty.zips.17$GEOID, df.17$GEOID))
is.element(cnty.zips.17$GEOID, df.17$GEOID)





# lacounty_data  <- fread("lacountytech.csv")

## geting the state boundry file from package tigris
# If you have issues with API, download from Tiger's Website
# download.file("https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html", desfile = 'states.zip")
# unzip("states.zip")
# state <- read_shape("cb_2015_us_state_20m.shp", as.sf = TRUE, stringsAsFactors = FALSE)

# tigris_use_cache = TRUE
# state<-states(cb=TRUE,class="sf")
# head(state)
# summary(state)
# class(state)
# # 
# # ## geting the County boundry file from package tigris
# tigris_use_cache = TRUE
# county<-counties(cb=TRUE,class="sf")
# head(county)
# summary(county)
# class(county)
# 
# ## geting the zip boundry file from package tigris
# zip<-zctas(cb=TRUE,class="sf")
# 
# ## geting the zip boundry file from package tigris Only from LA
# tigris_use_cache = TRUE
# zip<-zctas(cb=TRUE, starts_with = c("90", "91", "92", "93"),class="sf")
# 
# head(zip)
# summary(zip)
# class(zip)


## joining data with geometry data to map the data
str(zips.shp@data)
str(df.17)

zips.shp$zipcode <-as.numeric(zips.shp$zipcode)
names(zips.shp)
# state$GEOID <-as.numeric(state$GEOID)
# state.sf<-inner_join(state,state.chci,by="GEOID")
df.17$GEOID<-as.numeric(df.17$GEOID)
# county.sf<-inner_join(county,county.chci,by="GEOID")

# str(county.chci)
# str(zip)
# str(lacounty_data)
# dim(lacounty_data)
# dim(zip)
# 
# head(lacounty_data)


## for crearing a dataset for mapping the state with CHCI data point, we join two data frame zip.chci & zip
# zip$ZCTA5CE10<-as.numeric(zip$ZCTA5CE10)
# zip.sf<-inner_join(lacounty_data, zip,by=c("GEOID"="ZCTA5CE10"))

y <- merge(df.17, zips.shp, by.x = "GEOID", by.y = "zipcode")

# df.17.shp <-left_join(df.17, zips.shp,by=c("GEOID"="zipcode"))

# zip.sf<-left_join(lacounty_data, zip,by=c("GEOID"="ZCTA5CE10"))

# dt <- zip.sf
# 
# my_spatial_polys <- SpatialPolygons(dt, proj4string = CRS("+proj=longlat +datum=WGS84") )

# Merge sf object and data frame
# st_geometry(zip.sf) <- zip.sf$geometry
# 
# # Subset zip.sf for 2017
# zip.sf.17 <- zip.sf %>% filter(year == 2017)
# 
# # # Subset zip.sf for 2015
# zip.sf.05 <- zip.sf %>% filter(year == 2005)

# #####################################################################################################
# #
# # Dot Density Maps using maptools
# #
# #####################################################################################################

library(maptools)

# plotvar <- cadata$totalpop / 1000		# One dot per 1,000 people
# plotvar <- df.17$tech
plotvar <- df.17$tech/10
cadots.rand <- dotsInPolys(zips.shp, as.integer(plotvar), f="random")

# Map boundaries
cacounties.shp <- zips.shp
cacounties <- SpatialPolygonsDataFrame(cacounties.shp, data=as(cacounties.shp, "data.frame"))

# Set zero margins in plot window, so that map fits
# par(mar=c(0,0,0,0))
# plot(cacounties, lwd=0.1)
par(mar=c(0,0,0,0))
plot(zips.shp, lwd=0.1)

# Remember that you already calculated where all the dots go and stored the coordinates in cadots.rand.
# To plot them, you can use plot() again, but set the add argument to TRUE so that the dots are drawn 
# on top of the existing county map.

# Add dots
# plot(cadots.rand, add=T, pch=19, cex=0.1, col="#00880030")
# dotCols <- c("#da6678", "#647eee", "#c29219", "#09900d")

# Plot a specific region
plot(zips.shp, lwd=0.1, xlim=c(-118.510000,-117.820000), ylim=c(33.710000,34.860000))
# plot(cadots.rand, add=T, pch=19, cex=0.1, col="#008800")
plot(cadots.rand, add=T, pch=19, cex=0.1, col="#09900d")



##########
names(zip.shp) <- c("objectid","shape_area","shape_len","GEOID","name")
df <- inner_join(zips.shp, lacounty_data, by = "GEOID")
class(df.17)

names(lacounty_data)

tm_shape(zip.sf.17) + tm_fill()

tm_shape(zip.sf.17) + tm_borders()

map_la = tm_shape(zip.sf.17) +
  tm_polygons() +
  tm_fill(col = zip.sf.17$tech)
map_la
##########


# new
my_int_17 <- tm_shape(y) +
  tm_polygons("per", id = "GEOID", n = 15, palette = "BuGn")
tmap_mode('view')
my_int_17 <- tmap_leaflet(my_int_17)
my_int_17 %>% 
  setView(lng = -118.2437, lat = 34.0522, zoom = 9) %>% 
  addProviderTiles("CartoDB.Positron")




# map 2017
my_int_17 <- tm_shape(zip.sf.17) +
  tm_polygons("per", id = "GEOID", n = 15, palette = "BuGn")
tmap_mode('view')
my_int_17 <- tmap_leaflet(my_int_17)
my_int_17 %>% 
  setView(lng = -118.2437, lat = 34.0522, zoom = 9) %>% 
  addProviderTiles("CartoDB.Positron")

# map
map <- tm_shape(zip.sf.17) +
  tm_polygons("per", id = "GEOID")
tmap_mode('view')
map <- tmap_leaflet(map)
map %>% 
  setView(lng = -118.2437, lat = 34.0522, zoom = 9) %>% 
  addProviderTiles("CartoDB.Positron")

breaks = c(0, 25, 40, 60, 75, 100)
# map
map <- tm_shape(zip.sf.17) +
  tm_polygons("per", id = "GEOID", breaks = breaks)
tmap_mode('view')
map <- tmap_leaflet(map)
map %>% 
  setView(lng = -118.2437, lat = 34.0522, zoom = 9) %>% 
  addProviderTiles("CartoDB.Positron")

# map
map <- tm_shape(lacounty) +
  tm_polygons()
tmap_mode('view')
map <- tmap_leaflet(map)
map %>% 
  setView(lng = -118.2437, lat = 34.0522, zoom = 9) %>% 
  addProviderTiles("CartoDB.Positron")

# map
map <- tm_shape(zips.shp) +
  tm_polygons()
tmap_mode('view')
map <- tmap_leaflet(map)
map %>% 
  setView(lng = -118.2437, lat = 34.0522, zoom = 9) %>% 
  addProviderTiles("CartoDB.Positron")




# # Map color scale
# breaks <- c(0, 2, 4, 6, 8, 10, 12, 14, 100)
# pal <- colorRampPalette(c("#b9d7ef", "#102d44"))
# col <- pal(length(breaks)-1)
