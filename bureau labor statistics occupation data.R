#####################################################################################################
#
# Bureau of Statistics - https://www.bls.gov/emp/tables/emp-by-major-occupational-group.htm
#
#####################################################################################################

# Inspired by https://murraylax.org/rtutorials/barplots.html

# install.packages("tidyverse") # contains many packages that allow you to organize, summarize, and plot data.
# install.packages("scales")
# install.packages("stringr")
# install.packages("Hmisc") 
# install.packages("forcats") 
# install.packages("ggthemes") 

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
colnames(occupation) <- c("industry","code", "2016", "2026", "change2026", "perchange206","median_wages17")


# Creating the Bar Plot
ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17))

# Add Statistics and Geometry Layers
ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17)) + 
  stat_summary(fun.data=mean_sdl, geom="bar")

# Fix Labels - Text Wrap
occupation$industry <- as.factor(occupation$industry)
levels(occupation$industry)
levels(occupation$industry) <- str_wrap( levels(occupation$industry), width=12 )

ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17)) + 
  stat_summary(fun.data=mean_sdl, geom="bar")

# Labels and Titles

ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=dollar)

# add a descriptive title for plot and we remove the titles for the horizontal 
# and vertical axes since those will be obvious from the title of the plot and the labels on the scales

ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=dollar) + 
  labs(title="2017 Median Wages by Industry", x="", y="")

# Reordering Bars by Mean

# It is often useful for visual communication if the bars are ordered by height (mean). This allows the reader to 
# determine at a glance which industries earn the highest and lowest wages and where a particular industry falls
# in the distribution.

levels(occupation$industry)

# The function takes four parameters: (1) the factor variable to order levels, (2) the numerical variable to compute the mean (or other statistic) for each level (3) the summary statistic function we wish to use for ordering - we will pass along mean, but you can potentially pass along another function like median or sum, and (4) any parameters that you need to pass to the summary statistic function. In our case, we will pass the parameter, na.rm=TRUE, so that the mean() function ignores missing values. The function returns a new factor variable whose levels are ordered appropriately, but it is otherwise equal to the original factor.

# In the code below, we call fct_reorder() and let the output overwrite our current industry variable in data frame df.

occupation$industry <- fct_reorder(occupation$industry, occupation$median_wages17, mean, na.rm=TRUE)

ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=dollar) + 
  labs(title="Usual Hourly Earnings by Industry", x="", y="")


# Flip Coordinates

ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + 
  scale_y_continuous(labels=dollar) + 
  labs(title="Usual Hourly Earnings by Industry", x="", y="") +
  coord_flip()

# Using Color to Highlight One Category

# it is best practice to keep all of the bars the same color unless a change in color communicates something meaningful

# Color is visual attribute that can be mapped to a variable in the aesthetics layer. Let us first create a dummy variable called manu 
# that is equal to 1 if the industry variable is equal to "Computer and\nmathematical\noccupations" and 0 otherwise.

occupation$compu <- (occupation$industry == "Computer and\nmathematical\noccupations")

# The expression (df$industry == "Computer and\nmathematical\noccupations") will compare every observation in the industry variable and return a value Boolean TRUE if the expression is true and FALSE if the value is false.
# Next, we build our plot again from the beginning, this time including another mapping in the aesthetics layer.

ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17, fill=compu))

# build our plot again from the beginning, this time including another mapping in the aesthetics layer.

ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17, fill=compu)) +
  stat_summary(fun.y=mean, geom="bar") +
  scale_y_continuous(labels=dollar) +
  labs(title="2017 Median Wages by Industry", x="", y="") + 
  coord_flip() + 
  theme(legend.position="none")

# Choose colors - http://sape.inf.usi.ch/quick-reference/ggplot2/colour

mycols <- c("deepskyblue4", "seagreen")
ggplot(data=occupation, mapping=aes(x=industry, y=median_wages17, fill=compu)) +
  stat_summary(fun.y=mean, geom="bar") +
  scale_y_continuous(labels=dollar) +
  labs(title="2017 Median Wages by Industry", x="", y="") + 
  coord_flip() + 
  theme(legend.position="none") + 
  scale_fill_manual(values=mycols)



# Possible improvement - use API
# Package ‘blsAPI’ -- https://cran.r-project.org/web/packages/blsAPI/blsAPI.pdf
# Allows users to request data for one or multiple series through the
# U.S. Bureau of Labor Statistics API. Users provide parameters as specified in
# <https://www.bls.gov/developers/api_signature.htm> and the function returns a JSON
# string.


