library(lubridate)
library(zoo)
library(shiny)
library(rgdal)
library(rgeos)
library(tidyverse)
library(ggmap)
library(plotly)

DC_li <- read.csv("listings/DC_listings.csv")
DC_rev <- read.csv("reviews/DC_reviews.csv")
DC_map <- readOGR("maps/DC_neighbourhoods.geojson", "OGRGeoJSON")

names(DC_rev)[names(DC_rev)=="listing_id"] <- "id"
DC_rev_and_list <- inner_join(DC_li,DC_rev,by="id")
remove(DC_li)
remove(DC_rev)

DC_rev_and_list$date <- as.Date(DC_rev_and_list$date)
DC_rev_and_list$month <- month(DC_rev_and_list$date,label=T,abbr=T)
DC_rev_and_list$year <- year(DC_rev_and_list$date)

DC_price <- summarise(group_by(DC_rev_and_list,neighbourhood,year),price=median(price))

DC_rev_year_sum <- summarize(group_by(DC_rev_and_list, neighbourhood, year, month), number_of_reviews=n())

DC_map_centers <- gCentroid(DC_map,byid=T)
DC_map_label <- cbind(as.data.frame(DC_map_centers),DC_map@data$neighbourhood)
remove(DC_map_centers)
names(DC_map_label)[3]<-"name"

DC_map_cart <- fortify(DC_map)


#########################
AMS_li <- read.csv("listings/AMS_listings.csv")
AMS_rev <- read.csv("reviews/AMS_reviews.csv")
AMS_map <- readOGR("maps/AMS_neighbourhoods.geojson", "OGRGeoJSON")

names(AMS_rev)[names(AMS_rev)=="listing_id"] <- "id"
AMS_rev_and_list <- inner_join(AMS_li,AMS_rev,by="id")
remove(AMS_li)
remove(AMS_rev)

AMS_rev_and_list$date <- as.Date(AMS_rev_and_list$date)
AMS_rev_and_list$month <- month(AMS_rev_and_list$date,label=T,abbr=T)
AMS_rev_and_list$year <- year(AMS_rev_and_list$date)

AMS_price <- summarise(group_by(AMS_rev_and_list,neighbourhood,year),price=median(price))

AMS_rev_year_sum <- summarize(group_by(AMS_rev_and_list, neighbourhood, year, month), number_of_reviews=n())

AMS_map_centers <- gCentroid(AMS_map,byid=T)
AMS_map_label <- cbind(as.data.frame(AMS_map_centers),AMS_map@data$neighbourhood)
remove(AMS_map_centers)
names(AMS_map_label)[3]<-"name"

AMS_map_cart <- fortify(AMS_map)

#################################
LON_li <- read.csv("listings/LON_listings.csv")
LON_rev <- read.csv("reviews/LON_reviews.csv")
LON_map <- readOGR("maps/LON_neighbourhoods.geojson", "OGRGeoJSON")

names(LON_rev)[names(LON_rev)=="listing_id"] <- "id"
LON_rev_and_list <- inner_join(LON_li,LON_rev,by="id")
remove(LON_li)
remove(LON_rev)

LON_rev_and_list$date <- as.Date(LON_rev_and_list$date)
LON_rev_and_list$month <- month(LON_rev_and_list$date,label=T,abbr=T)
LON_rev_and_list$year <- year(LON_rev_and_list$date)

LON_price <- summarise(group_by(LON_rev_and_list,neighbourhood,year),price=median(price))

LON_rev_year_sum <- summarize(group_by(LON_rev_and_list, neighbourhood, year, month), number_of_reviews=n())

LON_map_centers <- gCentroid(LON_map,byid=T)
LON_map_label <- cbind(as.data.frame(LON_map_centers),LON_map@data$neighbourhood)
remove(LON_map_centers)
names(LON_map_label)[3]<-"name"

LON_map_cart <- fortify(LON_map)
#############################################
NY_li <- read.csv("listings/NY_listings.csv")
NY_rev <- read.csv("reviews/NY_reviews.csv")
NY_map <- readOGR("maps/NY_neighbourhoods.geojson", "OGRGeoJSON")

names(NY_rev)[names(NY_rev)=="listing_id"] <- "id"
NY_rev_and_list <- inner_join(NY_li,NY_rev,by="id")
remove(NY_li)
remove(NY_rev)

NY_rev_and_list$date <- as.Date(NY_rev_and_list$date)
NY_rev_and_list$month <- month(NY_rev_and_list$date,label=T,abbr=T)
NY_rev_and_list$year <- year(NY_rev_and_list$date)

NY_price <- summarise(group_by(NY_rev_and_list,neighbourhood,year),price=median(price))

NY_rev_year_sum <- summarize(group_by(NY_rev_and_list, neighbourhood, year, month), number_of_reviews=n())

NY_map_centers <- gCentroid(NY_map,byid=T)
NY_map_label <- cbind(as.data.frame(NY_map_centers),NY_map@data$neighbourhood)
remove(NY_map_centers)
names(NY_map_label)[3]<-"name"

NY_map_cart <- fortify(NY_map)
###################################################
SF_li <- read.csv("listings/SF_listings.csv")
SF_rev <- read.csv("reviews/SF_reviews.csv")
SF_map <- readOGR("maps/SF_neighbourhoods.geojson", "OGRGeoJSON")

names(SF_rev)[names(SF_rev)=="listing_id"] <- "id"
SF_rev_and_list <- inner_join(SF_li,SF_rev,by="id")
remove(SF_li)
remove(SF_rev)

SF_rev_and_list$date <- as.Date(SF_rev_and_list$date)
SF_rev_and_list$month <- month(SF_rev_and_list$date,label=T,abbr=T)
SF_rev_and_list$year <- year(SF_rev_and_list$date)

SF_price <- summarise(group_by(SF_rev_and_list,neighbourhood,year),price=median(price))

SF_rev_year_sum <- summarize(group_by(SF_rev_and_list, neighbourhood, year, month), number_of_reviews=n())

SF_map_centers <- gCentroid(SF_map,byid=T)
SF_map_label <- cbind(as.data.frame(SF_map_centers),SF_map@data$neighbourhood)
remove(SF_map_centers)
names(SF_map_label)[3]<-"name"

SF_map_cart <- fortify(SF_map)
##################################################

DC_sum <- summarise(group_by(DC_rev_and_list,year,month,neighbourhood),
                    availability=median(availability_365),
                    price=median(availability_365),
                    number_of_reviews=median(number_of_reviews),
                    minimum=median(minimum_nights),
                    listings=median(calculated_host_listings_count))

AMS_sum <- summarise(group_by(AMS_rev_and_list,year,month,neighbourhood),
                    availability=median(availability_365),
                    price=median(availability_365),
                    number_of_reviews=median(number_of_reviews),
                    minimum=median(minimum_nights),
                    listings=median(calculated_host_listings_count))

LON_sum <- summarise(group_by(LON_rev_and_list,year,month,neighbourhood),
                    availability=median(availability_365),
                    price=median(availability_365),
                    number_of_reviews=median(number_of_reviews),
                    minimum=median(minimum_nights),
                    listings=median(calculated_host_listings_count))

NY_sum <- summarise(group_by(NY_rev_and_list,year,month,neighbourhood),
                    availability=median(availability_365),
                    price=median(availability_365),
                    number_of_reviews=median(number_of_reviews),
                    minimum=median(minimum_nights),
                    listings=median(calculated_host_listings_count))

SF_sum <- summarise(group_by(SF_rev_and_list,year,month,neighbourhood),
                    availability=median(availability_365),
                    price=median(availability_365),
                    number_of_reviews=median(number_of_reviews),
                    minimum=median(minimum_nights),
                    listings=median(calculated_host_listings_count))

DC_sum <- as.data.frame(DC_sum)
AMS_sum <- as.data.frame(AMS_sum)
LON_sum <- as.data.frame(LON_sum)
NY_sum <- as.data.frame(NY_sum)
SF_sum <- as.data.frame(SF_sum)

# head(DC_sum)
# names(DC_rev_and_list)
# DC_ward_map <- readOGR(dsn="Ward__2002",layer="Ward__2002")

# DC_map <- readOGR("maps/DC_neighbourhoods.geojson", "OGRGeoJSON")

# names(DC_ward_map)
# names(DC_map)
# DC_map
# ggplot(DC_map)+geom_polygon(DC_map)


# #convert listing coordinates in SpatialPoints object -- note that we get matching projections by using the proj4string value from DC_neighborhoods_map
# DC_li_spatial <- SpatialPoints(data.frame(long=DC_li$longitude, lat=DC_li$latitude),proj4string = DC_ward_map@proj4string)
# 
# # use the over() function to overlay the SpatialPoints for listings with the polygons defined in DC_neighborhoods_map
# DC_li_overlay <- over(DC_li_spatial,DC_ward_map)
# 
# DC_li <- cbind(DC_li_overlay,DC_li)
# DC_li <- filter(DC_li,!is.na(DC_li$WARD_ID))
# 
# names(DC_li)[4]<-"ward"

#Change listing_id attribute to id so inner join can be made 
# names(DC_rev)[names(DC_rev)=="listing_id"] <- "id"

#Join listing and review tables on listing id
# DC_rev_and_list <- inner_join(DC_li,DC_rev,by="id")
# remove(DC_li)
# remove(DC_rev)

#Convert date to YYY-MM-DD to YYY-Month format
# DC_rev_and_list$date <- as.Date(DC_rev_and_list$date)
# 
# DC_rev_and_list$month <- month(DC_rev_and_list$date,label=F,abbr=T)
# DC_rev_and_list$year <- year(DC_rev_and_list$date)
# names(DC_rev_and_list)
# head(DC_rev_and_list$neighbourhood_group)
# DC_price <- summarise(group_by(DC_rev_and_list,neighbourhood,year),price=median(price))
# DC_rev_and_list$date <- paste(DC_rev_and_list$year,DC_rev_and_list$month,sep="-")

# DC_rev_year_sum <- summarize(group_by(DC_rev_and_list, neighbourhood, year, month), number_of_reviews=n())
# head(DC_rev_year_sum)
# names(DC_rev_and_list)
# DC_rev_and_list_all_year <- DC_rev_and_list[c(2,23,24,26,35)]
#----------------------------------------------------
# DC_rev_and_list
# DC_map_centers <- gCentroid(DC_map,byid=T)
# DC_map_label <- cbind(as.data.frame(DC_map_centers),DC_map@data$neighbourhood)
# remove(DC_map_centers)
# names(DC_map_label)[3]<-"name"
# 
# DC_map_cart <- fortify(DC_map)
# DC_map_cart
# DC_map_df <- DC_map 
# DC_map_df@data$id <- rownames(DC_map_df@data)
# DC_map_df.points <- fortify(DC_map_df, region="id")
# DC_map.cart <- inner_join(DC_map_df.points, DC_map_df@data, by="id")
# remove(DC_map_df)
# remove(DC_map.points)

# DC_ward_map_df <- DC_ward_map
# 
# DC_ward_map_df@data$id <- rownames(DC_ward_map_df@data)
# DC_ward_map_df.points <- fortify(DC_ward_map_df, region="id")
# ward.df.cart <- inner_join(DC_ward_map_df.points, DC_ward_map_df@data, by="id")
# remove(DC_ward_map_df)
# #processing for labelling, explained in meeting
# ward_centers <- gCentroid(DC_ward_map,byid=TRUE)
# ward_label_data <- cbind(as.data.frame(ward_centers), DC_ward_map@data$NAME)
# names(ward_label_data)[3] <- "name"
# 
# ward.df.cart <- ward.df.cart[c(1,2,7)]

# names(DC_rev_and_list)

# price_sum <- summarise(group_by(DC_rev_and_list,ward,year),price=median(price))
