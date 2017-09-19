# Including libraries
library(tidyverse)
library(rworldmap)

# Loading and viewing data
data <- read_csv("API_ILO_country_YU.csv")
View(data)

# Notice that there are rows that are not countries. Rather, it is a classification (based on income or continent)

# First question: how does youth unemployment change over years?
averageYears <- tribble (~year, ~average,
                         "2010", mean(data$"2010"),
                         "2011", mean(data$"2011"),
                         "2012", mean(data$"2012"),
                         "2013", mean(data$"2013"),
                         "2014", mean(data$"2014"))
ggplot(data = averageYears) + geom_histogram(mapping = aes(x = year, y = average, fill = year),
                                             stat = "identity") # Plot shows that average unemployment doesn't change much over the year.

# Displaying histogram to see how average unemployment changes over years
data <- data %>% mutate(averageChange = (`2014` - `2010`)/ `2014`)
ggplot(data) + geom_histogram(aes(x = `Country Name`, y = `averageChange`), stat = "identity") # Most generally showcase small changes, should investigate outliers

# Make a horizontal bar graph that displays % change in unemployment from 2010 to 2014 in BRICS
brics <- c("Brazil", "Russian Federation", "China", "India",
                     "South Africa")
pcUnemBrics <- data %>% filter(`Country Name` %in% brics) %>% 
  mutate(averageChange = (`2014` - `2010`)/`2014`)
ggplot(pcUnemBrics) + geom_bar(mapping = aes(x = `Country Name`, y = `averageChange`,
                                      fill = `Country Name`), stat = "identity") + coord_flip()
# Conclusion: unemployment in Brazil and Russia fell. India barely changed

# Horizontal bar graph of G7
g7 <- c("Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States", "European Union")
pcUnemG7 <- data %>% filter(`Country Name` %in% g7) %>% 
  mutate(averageChange = (`2014` - `2010`)/`2014`)
arrange(pcUnemG7, averageChange)
ggplot(pcUnemG7) + geom_bar(mapping = aes(x = `Country Name`, y = `averageChange`,
                                             fill = `Country Name`), stat = "identity") + coord_flip()
# Conclusion: most G7 countries were able to have positive employment gains, but Italy (proabably due to bankruptcy) had unemployment skyrocket

# Horizontal bar graph of countries that almost bankrupted?!?

# Map unemployment in countries: http://blog.kaggle.com/2016/11/30/seventeen-ways-to-map-data-in-kaggle-kernels/
mapped_data <- joinCountryData2Map(data, joinCode = "ISO3", 
                                   nameJoinColumn = "Country Code")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data, nameColumnToPlot = "averageChange")

# Comparing youth unemployment in European regions
centralEurope <- data[30, 3:7]
centralEurope <- tribble (~year, ~rate,
                         "2010", centralEurope$`2010`,
                         "2011", centralEurope$`2011`,
                         "2012", centralEurope$`2012`,
                         "2013", centralEurope$`2013`,
                         "2014", centralEurope$`2014`)
ggplot(data = centralEurope) + geom_histogram(mapping = aes(x = year, y = rate, fill = year),
                                             stat = "identity") #2014 had a drastic drop
