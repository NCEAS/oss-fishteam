#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(rgeos)
library(leaflet)
library(gstat)
library(DT)
# sea_level_2000<-read.csv("sea_lev_2000.csv")
# sea_level_2000 <- sea_level_2000[!is.na(sea_level_2000$elev_mm),]
# grd<- expand.grid(x= seq(-99,-80,by =0.1),y=seq(24,32,by = 0.1))
# coordinates(grd)<- ~x+y
# gridded(grd)<-TRUE
oilspill<-read.csv("GoM_oilspill.csv")
datetxt <- as.Date((oilspill$open_date),format = "%m/%d/%Y")
oilspillbydate <- data.frame(lat= oilspill$lat, 
                             lon=oilspill$lon,
                             maxgal=oilspill$max_ptl_release_gallons,
                             year = as.numeric(format(datetxt, format = "%Y")),
                             month = as.numeric(format(datetxt, format = "%m")),
                             day = as.numeric(format(datetxt, format = "%d")))
wodsum<-read.csv("env_summer.csv")

cpuesum<-read.csv("summerCPUE_for_shapefile.csv")
cpuesum<-cpuesum[!is.na(cpuesum$CPUE),]

#Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Gulf of Mexico study"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Accident year:",
                     min = 1982,
                     max = 2017,
                     value = 2001,
                     sep = ""),
         selectInput("ocean","Oceanographic factors",
                     c("Oxygen","temp","sal"))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("distPlot")
      )
   )
)

# Define server logic required to draw a leaflet
server <- function(input, output) {
   
   output$distPlot <- renderLeaflet({
    
     #  idw_pow1 <- idw(formula = elev_mm ~ 1, 
     #                 locations = sea_level_2000, 
     #                 newdata = grd,
     #                 idp =input$power)  
     # plot(idw_pow1)
     t<-input$ocean
     testchol<-wodsum[!is.na(wodsum$t),]
     ttt<-length(testchol$Cast)
     title2<-paste0("WOD ", input$ocean)
     pal <- colorNumeric(
       palette = "YlOrRd",
       domain = oilspillbydate$month
     )
     color2<-length(testchol[testchol$year==input$year,]$year)
     color3<-length(cpuesum[cpuesum$year==input$year,]$year)
     color2f<-colorRampPalette(c("yellow","red"))
     color3f<-colorRampPalette(c("purple","blue"))
     leaflet() %>%
       addTiles() %>% 
       addCircleMarkers(data=oilspillbydate[oilspillbydate$year==input$year,],
                  lng= ~lon,
                  lat= ~lat,
                  weight= 1,
                  color="Green",
                  #fillColor= ~colorQuantile("BuGn", month)(month),
                  #fillOpacity = 0.5,
                  radius = 4,
                  popup = ~paste(sep = "<br/>", 
                                paste0("Gallons released: ",  as.character(maxgal)), 
                                paste0("month: ",as.character(month))),
                  group = "oil spill")%>%
       addCircleMarkers(data=testchol[testchol$year==input$year,],
                        lng=~lon,
                        lat=~lat,
                        weight =1,
                       # fillColor = ~terrain.colors(color2,alpha=0.5),
                       fillColor = color2f(color2), 
                       fillOpacity = 0.5,
                        radius=5,
                        popup=~paste(sep = "<br/>", 
                                     paste0("Factor value: ", as.character(input$ocean)), 
                                     paste0("month: ",as.character(month))),
                        group =title2)%>%
       addCircleMarkers(data=cpuesum[cpuesum$year==input$year,],
                  lng= ~DECSLON,
                  lat= ~DECSLAT,
                  weight= 1,
                  color =  "Yellow",
                  fillColor = ~color3f(color3),
                  #fillColor= ~rainbow(color3,alpha= 1),
                  fillOpacity = 0.5,
                  radius = ~ CPUE* 1e1,
                  popup = ~ paste(sep = "<br/>", 
                                  paste0("CPUE: ", as.character(CPUE)), 
                                  paste0("month: ",as.character(month))),
                  group = "CPUE")%>%
       addLayersControl(
         overlayGroups = c("oil spill",title2,"CPUE"),
         options = layersControlOptions(collapsed = TRUE)
       )  
        
         
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

