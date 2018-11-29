#####################################################################################################
# Capstone Project          Instructor: William Yu
# By Joao de Paula, UCLA Extension Data Science Intensive
# November. 2018
#####################################################################################################

#####################################################################################################
#
# Dynamic Visualization of tech jobs Los Angeles city & LA coutny - GoogleViz
#
#####################################################################################################

# Set working directory in R

#install.packages("rstudioapi") # run this if it's your first time using it to install
library(rstudioapi) # load it
current_path <- getActiveDocumentContext()$path # set current path
setwd(dirname(current_path ))# set working directory
getwd()

# Load libraries
library(googleVis)
# library(readxl)

# Load data
lacounty_data <- read.csv("./data/lacounty_data.csv")

# GoogleViz
J = gvisMotionChart(lacounty_data, idvar="location", timevar="year", xvar = "all_jobs", yvar="tech_jobs",
                    options=list(width=700, height=600))

plot(J)

print(J, "chart")

# It will open browser to show visulization, the site needs Flash 
# to work & # internet connection is required.
# 
# If a website isn't working, you might need to change your settings to allow Flash.
# 
# To enable Flash on Chrome:
# To the left of the web address, click (i) (next to url) or Info View site information.
# At the bottom, click Site Settings.
# In the new tab, to the right of "Flash," click the Down and then Allow. Close tab.
# Go back to the site and reload the page.


#####################################################################################################
#
# Best Visualizations
#
#####################################################################################################

# 1 ## Bubbles
# Tech Jobs x Time
# Color = Percentage
# Size = Tech Jobs
# Select = ALL + Trails
#-- shows drop after 2007 recession
#-- shows 2014 LA best tech % 10.34

# 2 ## Bubbles
# Per_Tech x Time
# Color = Percentage
# Size = Tech Jobs
# Select = ALL + Trails
# --- shows % fluctuates between 9/10% 

# 3 ## Bubbles
# Time x Growth
# Color = Percentage
# Size = Tech Jobs
# Select = ALL + Trails


#4 ## Bar Charts
# Tech Jobs x Tech Jobs

##5 Line Charts
# Tech Jobs x Tech Jobs



