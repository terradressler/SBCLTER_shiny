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
temp <- read_csv("tempdata_shortened.csv")

SitePoints_edit<- SitePoints %>% 
  rename(site = SiteName)

FishSummary <- fish %>% 
  select(year, month, site, transect, SIZE, COUNT, AREA, COMMON_NAME) %>%
  filter(COMMON_NAME != "-99999", COMMON_NAME != "no fish present", COUNT != -99999) %>% 
  group_by(site, year, COMMON_NAME) %>% 
  summarize(SUM =sum(COUNT)) %>% 
  mutate(density = SUM/80) %>% 
  mutate(size = density*10) 

fulldata_fish = merge(FishSummary, SitePoints_edit)

fulldata <- merge(fulldata, temp)

fulldata_sf <- st_as_sf(fulldata_sf, coords = c("LONG", "LAT"), crs = 4326)
  


ui <- fluidPage( theme = shinytheme("superhero"),
   
   
   titlePanel("Fish Counts and Temperature in Santa Barbara Kelp Forests"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("slider",
                     "Year",
                     min = 2000,
                     max = 2017,
                     value = 2000,
                     step = 1),
                    
                
                     
         selectInput("select",
                     "Fish Species",
                     choices = fulldata_sf$COMMON_NAME
                     ),
        
          radioButtons("buttons",
                       "Site Code",
                       choices = c("ABUR", "AQUE", "BULL", "MOWK", "IVEE", "CARP", "NAPL"),
                       selected =  "ABUR"
                       )
      ),
      
      
      mainPanel(
        tabsetPanel(
          type = "tab",
          tabPanel("Map",
                  leafletOutput("map", width = 2000, height = 1000)),
          tabPanel("Plot",
                   plotOutput("plot")),
          tabPanel("Data Source",
                   textOutput("text"))
                    )
      )
   )
)

# Define server logic 
server <- function(input, output) {
 
  filtered_map <- reactive({
    fulldata_sf [fulldata_sf$year <= input$slider & fulldata_sf$COMMON_NAME <= input$select]
  })
    
  output$map <- renderLeaflet({
    
    
    leaflet(filtered_map()) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-119.1, 34.1, zoom= 10) %>% 
      addCircleMarkers(radius = 10, fillOpacity = fulldata_sf$mean_temp, color = "orange") %>% 
      addCircleMarkers(color = "darkmagenta", radius = fulldata_sf$density)
  })
  
  
  
 filtered_plot <- reactive({
   fulldata %>% 
     filter(site == input$buttons, COMMON_NAME == input$select)
 })
   output$plot <- renderPlot({
     filtered_plot() %>% 
     ggplot()+
       geom_col(aes(x = year, y = density), fill = "darkmagenta")+
       geom_line(aes(x = year, y = mean_temp/10), col = "orange")+
       theme_classic()+
       labs(y = "Observed Fish Density (number of fish per square meter)", x = "Year")+
       scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Mean Yearly Temperature (Celcius)"))
   })
   
   output$text <- renderText({
     "These data were collected by the Santa Barbara Coastal Long Term Ecological Research program from 7 sites the year 2000-2017. Temperature data were recorded continuously from 7m depth by loggers installed at at each site. Fish counts were obtained from the same 40X2 transects each year during the fall months.

Observed fish density (number of fish per square meter) is used as a proxy for abundance in this app. Average yearly temperature is used to visualize long term temperature patterns.
     
    
 Citations: 1. Reed, D. . 2017. SBC LTER: Reef: Kelp Forest Community Dynamics: Fish abundance. Santa Barbara Coastal Long Term Ecological Research Project. doi:10.6073/pasta/6134f68762279edc58acbc0416501d3e
2. Reed, D. . 2017. SBC LTER: Reef: Bottom Temperature: Continuous water temperature, ongoing since 2000. Santa Barbara Coastal Long Term Ecological Research Project. doi:10.6073/pasta/704ae1187eb55f3052c8ba46ec26e76f
     "
   })
   
}

shinyApp(ui= ui, server= server)

