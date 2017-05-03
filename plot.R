# The code is almost the same as the artcle
#  "Is happiness correlated with open data?" by Megan Risdal
#  https://www.kaggle.com/mrisdal/happiness-and-open-data

library(dplyr)

# Read in data files from `open-data` and `world-happiness` datasets
#open_data <- read.csv("~/Documents/kaggle/WorldHappinessReport2016/2015_GlobalOpenDataIndex/countries.csv", stringsAsFactors=F)
# MyComment:
# Russia is "Russian Federation" in the original countries.csv, 
# then I changed "Russia" in countries2.csv in order to match "Russia" in 2015.csv.
open_data <- read.csv("~/Documents/kaggle/WorldHappinessReport2016/2015_GlobalOpenDataIndex/countries2.csv", stringsAsFactors=F)
happiness <- read.csv("~/Documents/kaggle/WorldHappinessReport2016/2015.csv", stringsAsFactors=F)

# Rename from "Country Name" to just "Country" so it's easier to join
colnames(open_data)[2] <- "Country"

# Join the two dataset files on "Country"
open_data_happiness <- open_data %>%
    left_join(happiness, by = "Country")

# MyComment:
# Set ronames to plot the country names.
odh <- open_data_happiness
rownames(odh) <- odh$Country

library(ggplot2)
library(ggthemes)

# For all the countries.
ggplot(odh,
    aes(x = X2015.Score, 
        y = Happiness.Score, label=rownames(odh))) +
    geom_point() +
    geom_text(hjust=0,vjust=0,size=3,aes(fontface=2)) +
    geom_smooth(method="lm") +
    labs(x = "Openness Score",
         y = "Happiness Score",
         title = "Are open data friendly countries happy countries?",
         subtitle = "Data openness and happiness by country in 2015") +
    theme_hc()

odh %>% filter(!is.na(X2015.Score) & !is.na(Happiness.Score)) %>%
  dplyr::summarise(cor(X2015.Score,Happiness.Score))
##   cor(X2015.Score, Happiness.Score)
## 1                         0.4910137

# For just G8 countries.
g8 <- c("Canada","France","Germany","Italy","Japan","United Kingdom","United States","Russia")
ggplot(odh,
    aes(x = X2015.Score, 
        y = Happiness.Score)) + 
    geom_point() +
    geom_point(data=odh[g8,],color="red",size=5) +
    geom_smooth(method="lm") +
    labs(x = "Openness Score",
         y = "Happiness Score",
         title = "G8 in Red: Are open data friendly countries happy countries?",
         subtitle = "Data openness and happiness by country in 2015") +
    theme_hc()

# For just G8 countries.
odh %>%
  filter(Country=="Canada"|Country=="France"|Country=="Germany"|Country=="Italy"|Country=="Japan"|Country=="United Kingdom"|Country=="United States"|Country=="Russia") %>%
  ggplot(aes(x = X2015.Score, y = Happiness.Score, label=g8)) + 
    geom_point() +
    geom_text(size=6,aes(fontface=2)) +
    geom_smooth(method="lm") +
    labs(x = "Openness Score",
         y = "Happiness Score",
         title = "G8: Are open data friendly countries happy countries?",
         subtitle = "Data openness and happiness by country in 2015") +
    theme_hc()

odh %>%
  filter(Country=="Canada"|Country=="France"|Country=="Germany"|Country=="Italy"|Country=="Japan"|Country=="United Kingdom"|Country=="United States"|Country=="Russia") %>%
  dplyr::summarise(cor(X2015.Score,Happiness.Score))
##   cor(X2015.Score, Happiness.Score)
## 1                         0.6234862

