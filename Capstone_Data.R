#####################################################################################################
# Capstone Project          Instructor: William Yu
# By Joao de Paula, UCLA Extension Data Science Intensive
# December. 2018
#####################################################################################################
# 
# library(shinydashboard) #.................... creating shiny dashboard
# library(shiny)          #.................... creating shiny app
# library(shinycssloaders)# ....................loading css file with shiny app
# library(shinythemes)    # ..................... using the boostrap them for shiny
library(sf)             # ...................... reading, writing and wangling sf class for map
library(tidyverse)      #....................... R packages for data
# library(htmltools)      #....................... converting some data object to HTML and js
# library(leaflet)        # .......................  interactive map
# library(rmapshaper)     # ........................ resizing the shapefile
library(tidycensus)     # .......................... getting data from Censues
library(tigris)         # .......................... getting the shape file from United States Census
# 
# # install tigris with sf support
# # devtools::install_github("walkerke/tigris")
# # https://walkerke.github.io/tigris-webinar/#3


library(readxl)
library(dbplyr)
library(data.table) # data.table as an advanced version of data.frame #Fread
library(rgdal)

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

#####################################################################################################
#
# PROBLEM B - Calculate the Density of Tech Job by Zip Code in Los Angeles
#
#####################################################################################################


###############################################
#
# Set working directory / Load libraries
#
###############################################

setwd("C:/Users/joao_/Dropbox/Data Science/UCLA/Git/los-angeles-tech-jobs/data")

# # obesity - getting obesity stats
# obesity = chron.dis %>%
#   select(-one_of(c("YearEnd"))) %>%
#   filter( str_detect(Question, "Overweight or obesity")) %>%
#   filter( !str_detect(DataValueType, "Crude Prevalence")) %>%
#   filter(str_detect(Stratification1, "Overall" )) %>%
#   filter(LocationDesc %in% filtered.states.full) %>%
#   filter( YearStart %in% (filtered.six.year))

 
###############################################
#
# Download the data (P01_LA zipcode payroll.xlsx) into your computer.
#
###############################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2017"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

#  Subset Tech
##
# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)
##
# Subset for the Information sector jobs
laz17ei=subset(laz2017,NAICS==51)
##
# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)
##

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2017)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty17 <- laz17tech
lacounty17 <- lacounty17[c(1,5,6,3,4,7,2)]
head(lacounty17)

#######################################################################################

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2016"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total16", "info16", "prof16")
# laz17tech$tech16 = (laz17tech$info16 + laz17tech$prof16)
# laz17tech$per16 = (laz17tech$tech16 / laz17tech$total16)
# lacounty16 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2016)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty16 <- laz17tech
lacounty16 <- lacounty16[c(1,5,6,3,4,7,2)]
head(lacounty16)
#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2015"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total15", "info15", "prof15")
# laz17tech$tech15 = (laz17tech$info15 + laz17tech$prof15)
# laz17tech$per15 = (laz17tech$tech15 / laz17tech$total15)
# lacounty15 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2015)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty15 <- laz17tech
lacounty15 <- lacounty15[c(1,5,6,3,4,7,2)]
head(lacounty15)
#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2014"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total14", "info14", "prof14")
# laz17tech$tech14 = (laz17tech$info14 + laz17tech$prof14)
# laz17tech$per14 = (laz17tech$tech14 / laz17tech$total14)
# lacounty14 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2014)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty14 <- laz17tech
lacounty14 <- lacounty14[c(1,5,6,3,4,7,2)]
head(lacounty14)

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2013"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total13", "info13", "prof13")
# laz17tech$tech13 = (laz17tech$info13 + laz17tech$prof13)
# laz17tech$per13 = (laz17tech$tech13 / laz17tech$total13)
# lacounty13 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2013)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty13 <- laz17tech
lacounty13 <- lacounty13[c(1,5,6,3,4,7,2)]
head(lacounty13)

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2012"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total12", "info12", "prof12")
# laz17tech$tech12 = (laz17tech$info12 + laz17tech$prof12)
# laz17tech$per12 = (laz17tech$tech12 / laz17tech$total12)
# lacounty12 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2012)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty12 <- laz17tech
lacounty12 <- lacounty12[c(1,5,6,3,4,7,2)]
head(lacounty12)

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2011"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total11", "info11", "prof11")
# laz17tech$tech11 = (laz17tech$info11 + laz17tech$prof11)
# laz17tech$per11 = (laz17tech$tech11 / laz17tech$total11)
# lacounty11 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2011)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty11 <- laz17tech
lacounty11 <- lacounty11[c(1,5,6,3,4,7,2)]
head(lacounty11)

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2010"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total10", "info10", "prof10")
# laz17tech$tech10 = (laz17tech$info10 + laz17tech$prof10)
# laz17tech$per10 = (laz17tech$tech10 / laz17tech$total10)
# lacounty10 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2010)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty10 <- laz17tech
lacounty10 <- lacounty10[c(1,5,6,3,4,7,2)]
head(lacounty10)

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2009"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total09", "info09", "prof09")
# laz17tech$tech09 = (laz17tech$info09 + laz17tech$prof09)
# laz17tech$per09 = (laz17tech$tech09 / laz17tech$total09)
# lacounty09 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2009)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty09 <- laz17tech
lacounty09 <- lacounty09[c(1,5,6,3,4,7,2)]
head(lacounty09)

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2008"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total08", "info08", "prof08")
# laz17tech$tech08 = (laz17tech$info08 + laz17tech$prof08)
# laz17tech$per08 = (laz17tech$tech08 / laz17tech$total08)
# lacounty08 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2008)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty08 <- laz17tech
lacounty08 <- lacounty08[c(1,5,6,3,4,7,2)]
head(lacounty08)

#######################################################################################

laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2007"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total07", "info07", "prof07")
# laz17tech$tech07 = (laz17tech$info07 + laz17tech$prof07)
# laz17tech$per07 = (laz17tech$tech07 / laz17tech$total07)
# lacounty07 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2007)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty07 <- laz17tech
lacounty07 <- lacounty07[c(1,5,6,3,4,7,2)]
head(lacounty07)

#######################################################################################


laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2006"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total06", "info06", "prof06")
# laz17tech$tech06 = (laz17tech$info06 + laz17tech$prof06)
# laz17tech$per06 = (laz17tech$tech06 / laz17tech$total06)
# lacounty06 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2006)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty06 <- laz17tech
lacounty06 <- lacounty06[c(1,5,6,3,4,7,2)]
head(lacounty06)

#######################################################################################


laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2005"))

# Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
laz2017[is.na(laz2017)]=100

# Remove "Total" in Zip.Code
laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)

# Replace "*****" with 0
laz2017[laz2017=="*****"]=0

# Convert Column 5 and 6 from Character to Numeric

lapply(laz2017, class)
laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)

# Subset for the total sector job
laz17et=subset(laz2017,NAICS==100)

# Subset for the Information sector job
laz17ei=subset(laz2017,NAICS==51)

# Subset for the Professional Scientific Technical Services sector job in South Bay
laz17ebp=subset(laz2017,NAICS==54)


# laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
# 
# colnames(laz17tech) = c("zip", "total05", "info05", "prof05")
# laz17tech$tech05 = (laz17tech$info05 + laz17tech$prof05)
# laz17tech$per05 = (laz17tech$tech05 / laz17tech$total05)
# lacounty05 <- laz17tech

laz17tech.a=merge(laz17et[,c(1,5)], laz17ei[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5)], by="Zip.Code", all=TRUE)
laz17tech$year = c(2005)
laz17tech[is.na(laz17tech)] <- 0
colnames(laz17tech) = c("zip", "total", "info", "prof", "year")
laz17tech$tech = (laz17tech$info + laz17tech$prof)
laz17tech$per = round(((laz17tech$tech / laz17tech$total)*100), 2)
lacounty05 <- laz17tech
lacounty05 <- lacounty05[c(1,5,6,3,4,7,2)]
head(lacounty15)

#######################################################################################

# # Then, merge
# 
# lacountytech = merge(lacounty05,   lacounty06)
# lacountytech = merge(lacountytech, lacounty07)
# lacountytech = merge(lacountytech, lacounty08)
# lacountytech = merge(lacountytech, lacounty09)
# lacountytech = merge(lacountytech, lacounty10)
# lacountytech = merge(lacountytech, lacounty11)
# lacountytech = merge(lacountytech, lacounty12)
# lacountytech = merge(lacountytech, lacounty13)
# lacountytech = merge(lacountytech, lacounty14)
# lacountytech = merge(lacountytech, lacounty15)
# lacountytech = merge(lacountytech, lacounty16)
# lacountytech = merge(lacountytech, lacounty17)

# Then, bind

lacountytech=rbind(lacounty05, lacounty06, lacounty07,
                   lacounty08, lacounty09, lacounty10,
                   lacounty11, lacounty12, lacounty13,
                   lacounty14, lacounty15, lacounty16, lacounty17)


colnames(lacountytech)[1] <- "GEOID"
colnames(lacountytech)

# #we no longer need these variables
rm(lacounty05)
rm(lacounty06)
rm(lacounty07)
rm(lacounty08)
rm(lacounty09)
rm(lacounty10)
rm(lacounty11)
rm(lacounty12)
rm(lacounty13)
rm(lacounty14)
rm(lacounty15)
rm(lacounty16)
rm(lacounty17)
rm(laz17ebp)
rm(laz17ei)
rm(laz17et)
rm(laz2017)
rm(laz17tech.a)
rm(laz17tech)

# ###############################################
# #
# # Subset Tech Total Wages by zipcode for 20017 <- income17
# # ##### Data missing correct numbers for wages #### 
# # Decided to download data from ACS
# #
# ###############################################
# 
# laz2017 <- data.frame(read_excel("la_zip_payroll.xlsx", sheet="2017"))
# 
# # Repalceing NA with the value of 100 because I want to use it to subset/filter the industry
# laz2017[is.na(laz2017)]=100
# 
# # Remove "Total" in Zip.Code
# laz2017$Zip.Code=gsub("Total","",laz2017$Zip.Code)
# 
# # Replace "*****" with 0
# laz2017[laz2017=="*****"]=0
# 
# # Convert Column 5 and 6 from Character to Numeric
# 
# lapply(laz2017, class)
# laz2017[,c(1,5:6)]=sapply(laz2017[c(1,5:6)], as.numeric)
# 
# #  Subset Tech
# ##
# # Subset for the total sector job
# laz17et=subset(laz2017,NAICS==100)
# ##
# # Subset for the Information sector jobs
# laz17ei=subset(laz2017,NAICS==51)
# ##
# # Subset for the Professional Scientific Technical Services sector job in South Bay
# laz17ebp=subset(laz2017,NAICS==54)
# ##
# 
# laz17tech.a=merge(laz17et[,c(1,5,6)], laz17ei[,c(1,5,6)], by="Zip.Code", all=TRUE)
# laz17tech=merge(laz17tech.a, laz17ebp[,c(1,5,6)], by="Zip.Code", all=TRUE)
# laz17tech$year = c(2017)
# laz17tech[is.na(laz17tech)] <- 0
# colnames(laz17tech) = c("zip", "total_jobs", "total_wages", "info","info_wages", "prof", "prof_wages", "year")
# laz17tech$income = round((laz17tech$total_wages / laz17tech$total_jobs), 2)
# income17 <- laz17tech
# rm(laz17ebp)
# rm(laz17ei)
# rm(laz17et)
# rm(laz2017)
# rm(laz17tech.a)
# rm(laz17tech)


# Save as csv
write.csv(lacountytech,"lacountytech.csv")




