# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  ranges <- reactiveValues(x=NULL, y=NULL)
  output$reviewPlot <- renderPlotly({
    
    
    if(!is.null(input$reviewCity))
    {
      if(input$reviewCity=="Washington DC")
      {
        DC_rev_year <- filter(DC_rev_year_sum,year==input$reviewYear)
        
        #Initial plot draft - too many lines, select subset of neighborhoods
        g<-ggplot(DC_rev_year, aes(x=month, y=number_of_reviews, color=neighbourhood))
      }
      else if(input$reviewCity=="Amsterdam")
      {
        AMS_rev_year <- filter(AMS_rev_year_sum,year==input$reviewYear)
        
        #Initial plot draft - too many lines, select subset of neighborhoods
        g<-ggplot(AMS_rev_year, aes(x=month, y=number_of_reviews, color=neighbourhood))
      }
      else if(input$reviewCity=="London")
      {
        LON_rev_year <- filter(LON_rev_year_sum,year==input$reviewYear)
        
        #Initial plot draft - too many lines, select subset of neighborhoods
        g<-ggplot(LON_rev_year, aes(x=month, y=number_of_reviews, color=neighbourhood))
      }
      else if(input$reviewCity=="New York")
      {
        NY_rev_year <- filter(NY_rev_year_sum,year==input$reviewYear)
        
        #Initial plot draft - too many lines, select subset of neighborhoods
        g<-ggplot(NY_rev_year, aes(x=month, y=number_of_reviews, color=neighbourhood))
      }
      else if(input$reviewCity=="San Francisco")
      {
        SF_rev_year <- filter(SF_rev_year_sum,year==input$reviewYear)
        
        #Initial plot draft - too many lines, select subset of neighborhoods
        g<-ggplot(SF_rev_year, aes(x=month, y=number_of_reviews, color=neighbourhood))
      }
      g <- g+geom_line(aes(group=neighbourhood))+theme_minimal()+
        scale_x_discrete(month(seq(1,12),label=T))+
        labs(y="Number of Reviews")+theme(axis.title.x = element_blank())
      ggplotly(g)
    }
      
  })
  
  output$listingPlot <- renderPlot({
    if(!is.null(input$listingCity))
    {
      if(input$mapType=="googleMaps")
      {
        if(input$listingCity=="Washington DC")
        {
          DC_list_year <- filter(DC_rev_and_list,year==input$listingYear)
          locationRequest <- paste(input$listingNeighborhoods, "DC", sep=" ")
          DC <- get_map(locationRequest, zoom=input$listingZoom, maptype="roadmap")
          ggmap(DC) +
            geom_point(data=DC_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void()
        }
        else if(input$listingCity=="Amsterdam")
        {
          AMS_list_year <- filter(AMS_rev_and_list,year==input$listingYear)
          locationRequest <- paste(input$listingNeighborhoods, "Amsterdam", sep=" ")
          AMS <- get_map(locationRequest, zoom=input$listingZoom, maptype="roadmap")
          ggmap(AMS) + 
            geom_point(data=AMS_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void() 
        }
        else if(input$listingCity=="London")
        {
          LON_list_year <- filter(LON_rev_and_list,year==input$listingYear)
          locationRequest <- paste(input$listingNeighborhoods, "London", sep=" ")
          LON <- get_map(locationRequest, zoom=input$listingZoom, maptype="roadmap")
          ggmap(LON) + 
            geom_point(data=LON_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void()
        }
        else if(input$listingCity=="New York")
        {
          NY_list_year <- filter(NY_rev_and_list,year==input$listingYear)
          locationRequest <- paste(input$listingNeighborhoods, "New York", sep=" ")
          NY <- get_map(locationRequest, zoom=input$listingZoom, maptype="roadmap")
          ggmap(NY) + 
            geom_point(data=NY_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void()
        }
        else if(input$listingCity=="San Francisco")
        {
          SF_list_year <- filter(SF_rev_and_list,year==input$listingYear)
          locationRequest <- paste(input$listingNeighborhoods, "San Francisco", sep=" ")
          SF <- get_map(locationRequest, zoom=input$listingZoom, maptype="roadmap")
          ggmap(SF) + 
            geom_point(data=SF_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void() 
        }
      }
      else
      {
        if(input$listingCity=="Washington DC")
        {
          DC_list_year <- filter(DC_rev_and_list,year==input$listingYear)
          ggplot(DC_map_cart) +
            geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
            geom_text(data=DC_map_label, aes(x=x,y=y,label=name),size=2) +
            geom_point(data=DC_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void()
        }
        else if(input$listingCity=="Amsterdam")
        {
          AMS_list_year <- filter(AMS_rev_and_list,year==input$listingYear)
          ggplot(AMS_map_cart) + 
            geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
            geom_text(data=AMS_map_label, aes(x=x,y=y,label=name),size=2) +
            geom_point(data=AMS_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void() 
        }
        else if(input$listingCity=="London")
        {
          LON_list_year <- filter(LON_rev_and_list,year==input$listingYear)
          ggplot(LON_map_cart) + 
            geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
            geom_text(data=LON_map_label, aes(x=x,y=y,label=name),size=2) +
            geom_point(data=LON_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void()
        }
        else if(input$listingCity=="New York")
        {
          NY_list_year <- filter(NY_rev_and_list,year==input$listingYear)
          
          ggplot(NY_map_cart) + 
            geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
            geom_text(data=NY_map_label, aes(x=x,y=y,label=name),size=2) +
            geom_point(data=NY_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void()
        }
        else if(input$listingCity=="San Francisco")
        {
          SF_list_year <- filter(SF_rev_and_list,year==input$listingYear)
          ggplot(SF_map_cart) + 
            geom_polygon(aes(long,lat, group=group),color="grey50",fill="white") +
            geom_text(data=SF_map_label, aes(x=x,y=y,label=name),size=2) +
            geom_point(data=SF_list_year,aes(longitude,latitude,color=neighbourhood,size=price,alpha=0.5))+
            scale_alpha_continuous(guide=F)+
            coord_map() +
            scale_size_area(max_size=11) +
            theme_void() 
        }
      }
    }
  })
  
  output$pricePlot <- renderPlotly({
    if(!is.null(input$priceCity))
    {
      if(input$priceCity=="Washington DC")
      {
        g<-ggplot(DC_price)+geom_line(aes(x=year,y=price,color=neighbourhood))
      }
      else if(input$priceCity=="Amsterdam")
      {
        g<-ggplot(AMS_price)+geom_line(aes(x=year,y=price,color=neighbourhood))
      }
      else if(input$priceCity=="London")
      {
        g<-ggplot(LON_price)+geom_line(aes(x=year,y=price,color=neighbourhood))
      }
      else if(input$priceCity=="New York")
      {
        g<-ggplot(NY_price)+geom_line(aes(x=year,y=price,color=neighbourhood))
      }
      else if(input$priceCity=="San Francisco")
      {
        g<-ggplot(SF_price)+geom_line(aes(x=year,y=price,color=neighbourhood))
      }
      ggplotly(g)
    }
  })
  
  output$plotOptions <- renderUI({
    if(!is.null(input$listingCity))
    {
      if(input$mapType=="googleMaps")
      {
        if(input$listingCity=="Washington DC")
        {
          DC_list_year <- filter(DC_rev_and_list,year==input$listingYear)
          tagList(selectInput("listingNeighborhoods", "Neighborhood", DC_list_year$neighbourhood))
        }
        else if(input$listingCity=="Amsterdam") 
        {
          AMS_list_year <- filter(AMS_rev_and_list,year==input$listingYear)
          tagList(selectInput("listingNeighborhoods", "Neighborhood", AMS_list_year$neighbourhood))
        }
        else if(input$listingCity=="London") 
        {
          LON_list_year <- filter(LON_rev_and_list,year==input$listingYear)
          tagList(selectInput("listingNeighborhoods", "Neighborhood", LON_list_year$neighbourhood))
        }
        else if(input$listingCity=="New York") 
        {
          NY_list_year <- filter(NY_rev_and_list,year==input$listingYear)
          tagList(selectInput("listingNeighborhoods", "Neighborhood", NY_list_year$neighbourhood))
        }
        else if(input$listingCity=="San Francisco") 
        {
          SF_list_year <- filter(SF_rev_and_list,year==input$listingYear)
          tagList(selectInput("listingNeighborhoods", "Neighborhood", SF_list_year$neighbourhood))
        }
      }
    }
  }) 
  
  output$scatterPlot <- renderPlot({
    if(!is.null(input$correlationCity))
    {
      if(input$correlationCity=="Washington DC")
      {
        g<-ggplot(DC_sum)
      }
      else if(input$correlationCity=="Amsterdam") 
      {
        g<-ggplot(AMS_sum)
      }
      else if(input$correlationCity=="London") 
      {
        g<-ggplot(LON_sum)
      }
      else if(input$correlationCity=="New York") 
      {
        g<-ggplot(NY_sum)
      }
      else if(input$correlationCity=="San Francisco") 
      {
        g<-ggplot(SF_sum)
      }
      
      if(input$varX=="Price")
      {
        if(input$varY=="Price")
        {
          g<-g+geom_point(aes(x=price,y=price))+geom_smooth(aes(x=price,y=price),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Minimum Nights")
        {
          g<-g+geom_point(aes(x=price,y=minimum))+geom_smooth(aes(x=price,y=minimum),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Number of Reviews")
        {
          g<-g+geom_point(aes(x=price,y=number_of_reviews))+geom_smooth(aes(x=price,y=number_of_reviews),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Calculated Host Listings")
        {
          g<-g+geom_point(aes(x=price,y=listings))+geom_smooth(aes(x=price,y=listings),method="lm",se=T,level=.95)
        }
      }
      else if(input$varX=="Minimum Nights")
      {
        if(input$varY=="Price")
        {
          g<-g+geom_point(aes(x=minimum,y=price))+geom_smooth(aes(x=minimum,y=price),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Minimum Nights")
        {
          g<-g+geom_point(aes(x=minimum,y=minimum))+geom_smooth(aes(x=minimum,y=minimum),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Number of Reviews")
        {
          g<-g+geom_point(aes(x=minimum,y=number_of_reviews))+geom_smooth(aes(x=minimum,y=number_of_reviews),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Calculated Host Listings")
        {
          g<-g+geom_point(aes(x=minimum,y=listings))+geom_smooth(aes(x=minimum,y=listings),method="lm",se=T,level=.95)
        }
      }
      else if(input$varX=="Number of Reviews")
      {
        if(input$varY=="Price")
        {
          g<-g+geom_point(aes(x=number_of_reviews,y=price))+geom_smooth(aes(x=number_of_reviews,y=price),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Minimum Nights")
        {
          g<-g+geom_point(aes(x=number_of_reviews,y=minimum))+geom_smooth(aes(x=number_of_reviews,y=minimum),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Number of Reviews")
        {
          g<-g+geom_point(aes(x=number_of_reviews,y=number_of_reviews))+geom_smooth(aes(x=number_of_reviews,y=number_of_reviews),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Calculated Host Listings")
        {
          g<-g+geom_point(aes(x=number_of_reviews,y=listings))+geom_smooth(aes(x=number_of_reviews,y=listings),method="lm",se=T,level=.95)
        }
      }
      else if(input$varX=="Calculated Host Listings")
      {
        if(input$varY=="Price")
        {
          g<-g+geom_point(aes(x=listings,y=price))+geom_smooth(aes(x=listings,y=price),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Minimum Nights")
        {
          g<-g+geom_point(aes(x=listings,y=minimum))+geom_smooth(aes(x=listings,y=minimum),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Number of Reviews")
        {
          g<-g+geom_point(aes(x=listings,y=number_of_reviews))+geom_smooth(aes(x=listings,y=number_of_reviews),method="lm",se=T,level=.95)
        }
        else if(input$varY=="Calculated Host Listings")
        {
          g<-g+geom_point(aes(x=listings,y=listings))+geom_smooth(aes(x=listings,y=listings),method="lm",se=T,level=.95)
        }
      }
      g
    }
  })
  
})
