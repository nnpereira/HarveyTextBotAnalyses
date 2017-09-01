library(ggplot2)
library(lubridate)
library(janitor)
library(dplyr)
library(knitr)
library(ggmap)
library(readr)
library(zipcode)
library(stringr)

#GOALS:usage patterns, unmet needs, areas of confusion

setwd("~/Harvey_Textbot_Data_Stuff")
harvey_texts <- read.csv("harvey_texts.csv", stringsAsFactors = FALSE)

##'Get only text bodies with zipcodes mentioned from Texas
harvey_texts<- harvey_texts %>% filter(Status == "received")
pattern <- "(\\b([7][0-9]{4})\\b)"
texas_zipcodes <- harvey_texts[grepl(pattern, harvey_texts$Body),]
texas_zipcodes$zip <- str_extract(texas_zipcodes$Body, pattern)

#'Geocode zipcodes
data("zipcode")

texas_zipcodes <- texas_zipcodes %>% left_join(zipcode, by = "zip")

#'map the locations: country wide
#usa <- map_data("usa")
# ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
#   coord_fixed(1.3) +  geom_point(data = zipcodes, aes(x = longitude, y = latitude), color = "yellow", size = 1)

counties <- map_data("county")
tx_county <- subset(counties, region == "texas")

texas_df <- map_data("state") %>% subset(., region == "texas")
ggplot() + geom_polygon(data = texas_df, aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3) +  geom_point(data = subset(texas_zipcodes, zip %in% c(74000:78000)), aes(x = longitude, y = latitude), color = "yellow", size = 1) 

texas_base <- ggplot(data = texas_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

texas_counties <- texas_base + theme_nothing() +
  geom_polygon(data = tx_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)+
  geom_point(data = subset(texas_zipcodes, zip %in% c(74000:78000)), aes(x = longitude, y = latitude, group = NULL), color = "red", size = .2) +
  geom_polygon(data = subset(tx_county, subregion == "harris"), fill = "NA", color = "blue", size = 1)




