navbarPage("Airbnb Plots",
  tabPanel("Reviews over Time",
    titlePanel("Number of Reviews by Neighborhood over Time"),
      absolutePanel(top = "17%", left = 5, right = 5, bottom = "auto",
        width = "100%", height = "100%", draggable = F, fixed=F, 
        plotlyOutput("reviewPlot", height="600px")
      ),
      absolutePanel(fixed = F,
        draggable = TRUE, top = "80%", left = 20, right = 20, bottom = 20,
        width = 330, height = "auto",
        selectInput("reviewCity","City",c("Washington DC","Amsterdam","London","New York","San Francisco")),
        sliderInput("reviewYear",
        "Year",
        min = 2009,
        max = 2015,
        value = 2012,
        animate = animationOptions(interval=4000, loop=F)
      )
    )
  ),
  
  tabPanel("Listings by Region",
           titlePanel("Airbnb Listings by Region"),
           absolutePanel(top = "17%", left = 5, right = 5, bottom = "auto",
                         width = "100%", height = "100%", draggable = F, fixed=F,  
                         plotOutput("listingPlot", height="800px")
           ),
           fluidPage(
           absolutePanel(fixed = F,
                         draggable = TRUE, top="10%", left="50%", right = 20, bottom = "auto",
                         width = 1000, height = "auto",
                         
                         fluidRow(
                           
                           column(2,
                                    selectInput("listingCity","City",c("Washington DC","Amsterdam","London","New York","San Francisco"))
                           ),
                           column(2, 
                                  radioButtons("mapType", "Map Type", c("Google Maps"="googleMaps", "Shape File"="shape"))
                           ),
                           column(3,
                                  sliderInput("listingYear",
                                              "Year",
                                              min = 2009,
                                              max = 2015,
                                              value = 2012,
                                              animate = animationOptions(interval=5000, loop=F))
                           )
                           
                        ),
                        
                        fluidRow(
                          column(3,
                                 sliderInput("listingZoom",
                                             "Zoom",
                                             min=5,
                                             max = 18,
                                             value=13) 
                          ),
                          column(2,
                                 uiOutput("plotOptions"))
                        )
           )
      )
  ),
  
  tabPanel("Listing Prices Over Time",
             titlePanel("Listing Prices By Neighborhood Over Time"),
             absolutePanel(top = "17%", left = 5, right = 5, bottom = "auto",
                           width = "100%", height = "100%", draggable = F, fixed=F,  
                           plotlyOutput("pricePlot", height="600px")
                          ),
             absolutePanel(fixed = F,
                             draggable = TRUE, top = "85%", left = 20, right = 20, bottom = 20,
                             width = "auto", height = "auto",
                             sidebarPanel(
                               selectInput("priceCity","City",c("Washington DC","Amsterdam","London","New York","San Francisco"))
                              )
            )        
          ),
  
  tabPanel("Correlation",
   sidebarLayout(

    mainPanel(
     plotOutput("scatterPlot")
    ),

    sidebarPanel(
      selectInput("correlationCity","City",c("Washington DC","Amsterdam","London","New York","San Francisco")),
      selectInput("varX","Variable X",c("Price","Minimum Nights","Number of Reviews","Calculated Host Listings")),
      selectInput("varY","Variable Y",c("Price","Minimum Nights","Number of Reviews","Calculated Host Listings"))
    ),

    position = "right"
  ))
)