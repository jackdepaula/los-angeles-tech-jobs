#####################################################################################################
# Capstone Project          Instructor: William Yu
# By Joao de Paula, UCLA Extension Data Science Intensive
# December. 2018
#####################################################################################################

#####################################################################################################
#
# Dynamic Visualization of tech jobs Grownth rate city x coutny
#
#####################################################################################################

# Load libraries
library(googleVis)

# Load LA Zip Data
la_city_zip <- data.frame(read_excel("lacity_zip_chci.xlsx"))

city_zip <- as.data.frame(la_city_zip$zip)
names(city_zip) <- c("GEOID")
city_zip <- unique(city_zip)
citytech <- inner_join(city_zip, lacountytech, by = "GEOID")


#####################################################################################################
#
# Calculate growth rates  
#
#####################################################################################################


# totals for city for 2005

# city_tech05 <- lacountytech %>% 
#   filter(year == 2005) %>% 
#   select("GEOID", "year", "tech", "total")

# totals for County

county_tech_total <- lacountytech %>%
  group_by(year) %>% 
  summarize(total_tech_cnty = sum(tech), total_jobs_cnty = sum(total)) %>% 
  mutate(per_cnty = total_jobs_cnty/total_tech_cnty, cnty_tech_gr = c(NA,exp(diff(log(per_cnty)))-1))


# total city 

city_tech_total <- citytech %>%
  group_by(year) %>%
  summarize(total_tech_la = sum(tech), total_jobs_la = sum(total)) %>% 
  mutate(per_la = total_jobs_la/total_tech_la, city_tech_gr = c(NA,exp(diff(log(per_la)))-1))


city_tech_total$loc <- "city"

county_tech_total$loc <- "county"

city_tech_total <- city_tech_total[c(6,1,2,3,4,5)]
county_tech_total <- county_tech_total[c(6,1,2,3,4,5)]

names(city_tech_total) <- c("location", "year", "tech_jobs", "all_jobs", "percentage", "growth")
names(county_tech_total) <- c("location", "year", "tech_jobs", "all_jobs", "percentage", "growth")

# city_tech_total$GEOID <- "06"
# county_tech_total$GEOID <- "07"

lacounty_data=rbind(city_tech_total, county_tech_total)

write.csv(lacounty_data,"lacounty_data.csv")



# Year Totals - City & County 

# J1 = gvisGeoChart(filter(lacounty_data, year %in% 2005),
#                   locationvar = "location", 
#                   colorvar = "tech_jobs",
#                   options=list(region="US", 
#                                displayMode="regions", 
#                                resolution="provinces",
#                                colorAxis="{colors:[\'#87CEEB\', \'#BE2625\']}",
#                                width=800, height=600))

# J2 = gvisGeoChart(filter(lacounty_data, year %in% 2017),
#                   locationvar = "location", 
#                   colorvar = "tech_jobs",
#                   options=list(region="US", 
#                                displayMode="regions", 
#                                resolution="provinces",
#                                colorAxis="{colors:[\'#87CEEB\', \'#BE2625\']}",
#                                width=800, height=600))

J = gvisMotionChart(lacounty_data, idvar="location", timevar="year", xvar = "all_jobs", yvar="tech_jobs",
                    options=list(width=700, height=600))

plot(J)

J1 = gvisMotionChart(lacounty_data, idvar="location", timevar="year", xvar = "all_jobs", yvar="tech_jobs",
                     options=list(width=700, height=600))


#####################################################################################################
#
# GoogleViz best visualizations
#
#####################################################################################################

# Bar Chart - Grownth x Grownth , color = percentage
# Same - Line Chart
# TBar Chart - Tech Jobs x Tech Jobs , color = percentage

plot(J1)


# Year Totals - All zip codes + City & County

# total city 

zip_tech_total <- citytech %>%
  group_by(GEOID, year)%>%
  summarize(total_tech_la = sum(tech), total_jobs_la = sum(total)) %>% 
  mutate(per_la = total_jobs_la/total_tech_la, city_tech_gr = c(NA,exp(diff(log(per_la)))-1))


# city_tech_total$loc <- "city"

# county_tech_total$loc <- "county"

# city_tech_total <- city_tech_total[c(6,1,2,3,4,5)]
# county_tech_total <- county_tech_total[c(6,1,2,3,4,5)]

names(zip_tech_total) <- c("location", "year", "tech_jobs", "all_jobs", "percentage", "growth")

write.csv(zip_tech_total,"zip_tech_total.csv")

zip_tech_total <- data.frame(read_excel("zip_tech_total_1.xlsx"))

# zip_tech_total <- as.data.frame(zip_tech_total)

# city_tech_total$GEOID <- "06"
# county_tech_total$GEOID <- "07"
# str(zip_tech_total)
# str(lacounty_data)
# 
# class(zip_tech_total)
# class(lacounty_data)
# identical(sapply(zip_tech_total, class), sapply(lacounty_data, class))

# zip_tech_total$location <- as.character(zip_tech_total$location)

# lacounty_all <- bind_rows(lacounty_data, zip_tech_total)

# names(zip_tech_total)

K = gvisMotionChart(zip_tech_total, idvar="location", timevar="year", xvar = "all_jobs", yvar="tech_jobs",
                    options=list(width=1200, height=600))


plot(K)

str(zip_tech_total)
str(lacounty_data)

write.csv(zip_tech_total, file = "zip_tech_total.csv")
zip_tech_total <- read.csv("zip_tech_total.csv")

str(zip_tech_total)
zip_tech_total$X <- NULL
zip_tech_total$location <- as.character(zip_tech_total$location)
zip_tech_total$year <- as.numeric(zip_tech_total$year)
zip_tech_total$tech_jobs <- as.numeric(zip_tech_total$tech_jobs)
zip_tech_total$all_jobs <- as.numeric(zip_tech_total$all_jobs)
str(zip_tech_total)
