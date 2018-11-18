library(readxl)
library(tidycensus)
library(tidyverse)

# chci <- data.frame(read_excel("./data/lacounty_zip_chci.xlsx", sheet="2009"))
chci <- data.frame(read_excel("lacounty_zip_chci.xlsx", sheet="2012"))

# Getting data from the 2012-2016 5-year ACS
income <- get_acs(geography = "zcta", 
                  variables = c(medincome = "B19013_001"))


cor <- merge(chci, income, by.x = "zip", by.y = "GEOID")

# Create the scatter plot with regression line

p7 <- ggplot(cor, aes(x=chci, y=estimate)) +
  geom_point(colour = "blue", size=4) + 
  labs(title = "Correlation Between the CHCI and the\n
       Median House Income of Los Angeles County Zip Codes",
       x = "City Human Capital Index", y = "Median Household Income") +
  scale_x_continuous(limits = c(85, 175)) +
    geom_smooth(colour = "red", method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
p7

# lm.cor <- lm(estimate ~ chci, data = cor)
# summary(lm.cor)
