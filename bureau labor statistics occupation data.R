#####################################################################################################
#
# Bureau of Statistics - https://www.bls.gov/emp/tables/emp-by-major-occupational-group.htm
#
#####################################################################################################

install.packages("tidyverse") # contains many packages that allow you to organize, summarize, and plot data.
install.packages("scales")
# install.packages("stringr")
# install.packages("Hmisc") 
# install.packages("forcats") 
# install.packages("ggthemes") 

remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

library("tidyverse") # This needs to be executed every time you load R

library("scales") # This needs to be executed every time you load R
library("stringr") # This needs to be executed every time you load R
library("Hmisc") # This needs to be executed every time you load R
library("forcats") # This needs to be executed every time you load R
library("ggthemes") # This needs to be executed every time you load R

library("ggplot2")

# We use the scales library to customize the scales of our axes.
# 
# The stringr package allows us to manipulate strings, which we use to manipulate string labels.
# 
# The Hmisc package provides mathematical and statistical functions to use with our plots.
# 
# The forcats package provides tools for manipulating categorical variables.
# 
# The ggthemes package provides multiple themes, which are combinations of parameters to change a plots look and feel

library("readxl")
# Reads URLs directly
# x <- fread("https://www.bls.gov/emp/tables/emp-by-major-occupational-group.html")

occupation <- read_excel("./data/occupation.xlsx", sheet = "Table 1.1", skip = 2)
occupation <- occupation[1:23,]
colnames(occupation) <- c("name","code", "2016", "2026", "change2026", "perchange206","median_wages17")

# Creating the Bar Plot
ggplot(data=occupation, mapping=aes(x=median_wages17, y=name))


# Possible improvement - use API
# Package ‘blsAPI’ -- https://cran.r-project.org/web/packages/blsAPI/blsAPI.pdf
# Allows users to request data for one or multiple series through the
# U.S. Bureau of Labor Statistics API. Users provide parameters as specified in
# <https://www.bls.gov/developers/api_signature.htm> and the function returns a JSON
# string.


