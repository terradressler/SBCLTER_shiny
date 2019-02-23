#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(sf)

SitePoints <- read_csv("Site_GPS.csv")
fish <- read_csv("All_Fish_All_Years_20171201.csv")

SitePoints_edit<- SitePoints %>% 
  rename(site = SiteName)

FishSummary <- fish %>% 
  select(year, month, site, transect, SIZE, COUNT, AREA, COMMON_NAME) %>%
  filter(COMMON_NAME != "-99999", COMMON_NAME != "no fish present") %>% 
  group_by(site, year, COMMON_NAME) %>% 
  summarize(SUM =sum(COUNT)) %>% 
  mutate(density = SUM/80) %>% 
  mutate(size = density*10)

fulldata = merge(FishSummary, SitePoints_edit)

fulldata_sf <- st_as_sf(fulldata, coords = c("LONG", "LAT"), crs = 4326)
  
map <- leaflet(fulldata_sf) %>% 
  addTiles() %>% 
  addCircleMarkers(color = "red", radius = fulldata_sf$size)

ui <- fluidPage(
   
   
   titlePanel("Fish Counts and Temperature in Santa Barbara Kelp Forests"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Year",
                     min = 2000,
                     max = 2017,
                     value = 18),
                     
         selectInput("COMMON_NAME",
                     "Fish Species",
                     list("Kelp Bass", "Senorita", "Painted Greenling", "Rock Wrasse", "Giant Kelpfish", "California Sheephead", "Black Surfperch", "Opaleye", "White Surfperch")
                     )
      ),
      
      
      mainPanel(
        tabsetPanel(
          type = "tab",
          tabPanel("Map",
                  leafletOutput("map1", width = 2000, height = 1000)),
          tabPanel("Plot",
                   plotOutput("timeseries"))
                    )
      )
   )
)

# Define server logic 
server <- function(input, output) {
   
   output$map1 <- renderLeaflet({
    map1<- leaflet(fulldata_sf) %>%
       addTiles() %>% 
       addCircleMarkers(color = "red", radius = fulldata_sf$density*10)
   })
   output$plot <- renderPlot({
     ggplot(FishSummary, aes(x = year, y = density))+
       geom_scatter()
   })
}

shinyApp(ui, server)

