#####################################################################################################
# Capstone Project          Instructor: William Yu
# By Joao de Paula, UCLA Extension Data Science Intensive
# November. 2018
#####################################################################################################

# proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
# meuse.utm <- spTransform(meuse, CRS("+proj=utm +zone=32 +datum=WGS84"))

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

# lacounty_data  <- fread("./data/lacountytech.csv")

lacounty_data  <- fread("lacountytech.csv")

## geting the state boundry file from package tigris
# If you have issues with API, download from Tiger's Website
# download.file("https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html", desfile = 'states.zip")
# unzip("states.zip")
# state <- read_shape("cb_2015_us_state_20m.shp", as.sf = TRUE, stringsAsFactors = FALSE)

tigris_use_cache = TRUE
state<-states(cb=TRUE,class="sf")
head(state)
summary(state)
class(state)
# 
# ## geting the County boundry file from package tigris
tigris_use_cache = TRUE
county<-counties(cb=TRUE,class="sf")
head(county)
summary(county)
class(county)

## geting the zip boundry file from package tigris
zip<-zctas(cb=TRUE,class="sf")

## geting the zip boundry file from package tigris Only from LA
tigris_use_cache = TRUE
zip<-zctas(cb=TRUE, starts_with = c("90", "91", "92", "93"),class="sf")

head(zip)
summary(zip)
class(zip)



#####################################################################################################
#
## joining the chci data with sf geometry data to map the data.
#
#####################################################################################################

##  for crearing a dataset for mapping the state with CHCI data point, we join two data frame state.chci & state
# library(tidyverse)


state$GEOID<-as.numeric(state$GEOID)
# state.sf<-inner_join(state,state.chci,by="GEOID")

# ## for crearing a dataset for mapping the state with CHCI data point, we join two data frame county.chci & county
county$GEOID<-as.numeric(county$GEOID)
# county.sf<-inner_join(county,county.chci,by="GEOID")

# str(county.chci)
str(zip)
str(lacounty_data)
dim(lacounty_data)
dim(zip)
## for crearing a dataset for mapping the state with CHCI data point, we join two data frame zip.chci & zip
zip$ZCTA5CE10<-as.numeric(zip$ZCTA5CE10)
# zip.sf<-inner_join(lacounty_data, zip,by=c("GEOID"="ZCTA5CE10"))
zip.sf<-left_join(lacounty_data, zip,by=c("GEOID"="ZCTA5CE10"))

# Merge sf object and data frame
st_geometry(zip.sf) <- zip.sf$geometry

# Subset zip.sf for 2017
zip.sf.17 <- zip.sf %>% filter(year == 2017)

# tmap 2017
my_int_17 <- tm_shape(zip.sf.17) +
  tm_polygons("per", id = "GEOID")
tmap_mode('view')

my_int_17 <- tmap_leaflet(my_int_17)

my_int_17 %>% 
  setView(lng = -118.2437, lat = 34.0522, zoom = 9)

my_int_17 %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -118.2437, lat = 34.0522, zoom = 9)

# # Map color scale
# breaks <- c(0, 2, 4, 6, 8, 10, 12, 14, 100)
# pal <- colorRampPalette(c("#b9d7ef", "#102d44"))
# col <- pal(length(breaks)-1)
