

library(shiny)
library(leaflet)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("map"),
  p()
  )

server <- function(input, output, session) 
  
points <- eventReactive(input$SitePoints)
    
  
  output$map <- renderLeaflet
    leaflet() 

shinyApp(ui, server)
