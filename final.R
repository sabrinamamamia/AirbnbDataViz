library(tidyverse)
library(ggplot2)
library(gridExtra)

#load dc data
dc_listings <- read.csv("DC_listings.csv")
dc_neighborhoods <- read.csv("DC_neighbourhoods.csv")
dc_reviews <- read.csv("DC_reviews.csv")

#load ny data
ny_listings <- read.csv("NY_listings.csv")
ny_neighborhoods <- read.csv("NY_neighbourhoods.csv")
ny_reviews <- read.csv("NY_reviews.csv")

#load london data
lon_listings <- read.csv("LON_listings.csv")
lon_neighborhoods <- read.csv("LON_neighbourhoods.csv")
lon_reviews <- read.csv("LON_reviews.csv")

#load san francisco data
sf_listings <- read.csv("SF_listings.csv")
sf_neighborhoods <- read.csv("SF_neighbourhoods.csv")
sf_reviews <- read.csv("SF_reviews.csv")


names(dc_listings)
#pie charts for question 1. listings by room type
g_dc <- ggplot(data=dc_listings,aes(x=factor(1),fill=room_type, y=..count../sum(..count..))) + geom_bar(width=1, position="fill", alpha=.7, color="black") + coord_polar(theta="y") +
  labs(title="DC Listings By Room Type", x="", y="")
g_dc <- g_dc + scale_fill_brewer(name="Room Type", palette="Set2")

g_ny <- ggplot(data=ny_listings,aes(x=factor(1),fill=room_type, y=..count../sum(..count..))) + geom_bar(width=1, position="fill", alpha=.7, color="black") + coord_polar(theta="y") +
  labs(title="New York Listings By Room Type", x="", y="")
g_ny <- g_ny + scale_fill_brewer(name="Room Type", palette="Set2")

g_lon <- ggplot(data=lon_listings,aes(x=factor(1),fill=room_type, y=..count../sum(..count..))) + geom_bar(width=1, position="fill", alpha=.7, color="black") + coord_polar(theta="y") +
  labs(title="London Listings By Room Type", x="", y="")
g_lon <- g_lon + scale_fill_brewer(name="Room Type", palette="Set2")

g_sf <- ggplot(data=sf_listings,aes(x=factor(1),fill=room_type, y=..count../sum(..count..))) + geom_bar(width=1, position="fill", alpha=.7, color="black") + coord_polar(theta="y") +
  labs(title="San Francisco Listings By Room Type", x="", y="")
g_sf <- g_sf + scale_fill_brewer(name="Room Type", palette="Set2")

grid.arrange(g_dc,g_ny,g_lon,g_sf)

#nightingale charts for question 1. listings by room type
# g <- ggplot(data=dc_listings) + geom_bar(aes(x=room_type, fill=room_type,color=room_type, y=..count../sum(..count..)),width=1, alpha=.7, color="black") + 
#   coord_polar()+scale_y_sqrt() + labs(title="DC Listings By Room Type", x="Room Type", y="Proportion")
# g <- g + scale_fill_brewer(name="Room Type", palette="Set2")
# g
# 
# g <- ggplot(data=ny_listings) + geom_bar(aes(x=room_type, fill=room_type,color=room_type, y=..count../sum(..count..)),width=1, alpha=.7, color="black") + 
#   coord_polar()+scale_y_sqrt() + labs(title="DC Listings By Room Type", x="Room Type", y="Proportion")
# g <- g + scale_fill_brewer(name="Room_Type", palette="Set2")
# g
# 
# g <- ggplot(data=lon_listings) + geom_bar(aes(x=room_type, fill=room_type,color=room_type, y=..count../sum(..count..)),width=1, alpha=.7, color="black") + 
#   coord_polar()+scale_y_sqrt() + labs(title="DC Listings By Room Type", x="Room Type", y="Proportion")
# g <- g + scale_fill_brewer(name="Room Type", palette="Set2")
# g
# 
# g <- ggplot(data=sf_listings) + geom_bar(aes(x=room_type, fill=room_type,color=room_type, y=..count../sum(..count..)),width=1, alpha=.7, color="black") + 
#   coord_polar()+scale_y_sqrt() + labs(title="DC Listings By Room Type", x="Room Type", y="Proportion")
# g <- g + scale_fill_brewer(name="Room Type", palette="Set2")
# g

#stacked bar chart
#first add city variable to each dataset before joining
dc_listings$city <- "Washington D.C."
ny_listings$city <- "New York City"
lon_listings$city <- "London"
sf_listings$city <- "San Francisco"

#join all cities
all_listings <- rbind(dc_listings, ny_listings, sf_listings, lon_listings)
all_listings_temp <- all_listings[all_listings$calculated_host_listings_count <= 10,]

#stacked bar chart by room type
g <- ggplot(all_listings) + geom_bar(aes(x=city, fill=room_type, y=..count../sum(..count..)), position="fill", alpha=.7, color="black")
g <- g + labs(title="City Listings By Room Type", x="", y="Relative frequency")
g <- g + scale_fill_brewer(name="Room Type", palette="Set2")
g

#--------------------------------------------------------------
#faceted density plots by city
g <- ggplot(all_listings_temp, aes(x=calculated_host_listings_count, colour=city)) + 
  geom_density() + scale_x_continuous(breaks = seq(1,10))+
  labs(title="Listings per Host by City", x="Number of Listings Per Host") + facet_grid(city~.)
g

#-----------------------------------------------------------------
#side by side bar chart looking at room types listed by host type
all_listings$host_type[all_listings$calculated_host_listings_count <= 5] <- "1-5"
all_listings$host_type[all_listings$calculated_host_listings_count > 5 & all_listings$calculated_host_listings_count < 11] <- "6-10"
all_listings$host_type[all_listings$calculated_host_listings_count > 10] <- "10+"

g <- ggplot(all_listings) + geom_bar(aes(x=host_type, y=..count../sum(..count..), fill=room_type), position="dodge", alpha=.7, color="black")
g <- g + labs(title="Types of Rooms Rented by Host Type", x="Listings Per Host", y="Relative Frequency")
g <- g + scale_fill_brewer(name="", palette="Set2" )
g

#---------------------------------------
#scatter plot
all_listings <- all_listings[all_listings$minimum_nights < 1000,]
g <- ggplot(all_listings) + geom_point(aes(x=availability_365, y=minimum_nights, size=calculated_host_listings_count, colour=room_type), alpha=.7)
g <- g + labs(title="Availability vs. Minimum Nights for Rental Scatterplot", x="Availability", y="Minimum Nights")
g <- g + scale_fill_brewer(name="", palette="Set2" )
g



#_----------------------------------




#---------------------------------
library(lubridate)

DC_li <- read.csv("DC_listings.csv")
DC_rev <- read.csv("DC_reviews.csv")

#Change listing_id attribute to id so inner join can be made 
names(DC_rev)[names(DC_rev)=="listing_id"] <- "id"

#Join listing and review tables on listing id
DC_rev_and_list <- inner_join(DC_li,DC_rev,by="id")

#Convert date to YYY-MM-DD to YYY-Month format
DC_rev_and_list$date <- as.Date(DC_rev_and_list$date)

DC_rev_and_list$month <- month(DC_rev_and_list$date,label=F,abbr=T)
DC_rev_and_list$year <- year(DC_rev_and_list$date)
DC_rev_and_list$date <- paste(DC_rev_and_list$year,DC_rev_and_list$month,sep="-")
DC_rev_monthly <- summarize(group_by(DC_rev_and_list, date, neighbourhood), number_of_reviews=n())

DC_rev_monthly <- filter(DC_rev_monthly,date>=2013 & date<2014)
DC_rev_monthly$date <- month(as.yearmon(DC_rev_monthly$date))

#Initial plot draft - too many lines, select subset of neighborhoods
ggplot(DC_rev_monthly, aes(x=date, y=number_of_reviews))+
  geom_line(aes(group=neighbourhood))+scale_x_continuous(breaks=seq(1,12))
