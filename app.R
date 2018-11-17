library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(magrittr)
library(leaflet)
library(data.table)


ui <- dashboardPage(
  dashboardHeader(title = "helpR"),
  dashboardSidebar(
    radioButtons(inputId = "type_of_transport", label = "Chose mode of transportation", choices = c("Foot", "Bike", "Public Transport", "Car"))
  ),
  dashboardBody(
    
    leafletOutput("basicMap")
  ),
  
  tags$script(' $(document).ready(function () { navigator.geolocation.getCurrentPosition(onSuccess, onError);
                function onError (err) { Shiny.onInputChange("geolocation", false); }
                function onSuccess (position) { 
              setTimeout(function () { var coords = position.coords; console.log(coords.latitude + ",
              " + coords.longitude); Shiny.onInputChange("geolocation", true); Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude); }, 1100) } }); ')
  
)

server <- function(input, output) {
    
  my_location <- reactive({
    
    
  })
  
   
  downloadedData <- reactive({
    
    app_id <- "va6Psz5oPSUztoDEu1NV"
    app_code <- "xeULHwPc_ab9QHZ2PMZmBA"
    
    data_tmp <- httr::GET("https://geocoder.api.here.com/6.2/geocode.xml",
                           query = list(app_id = app_id,
                                        app_code = app_code))
  })
    
   output$basicMap <- renderLeaflet({
     leaflet() %>%
       addProviderTiles(providers$Stamen.TonerLite,
                        options = providerTileOptions(noWrap = TRUE)
       ) %>%
       addMarkers(data = points())
   })
}

shinyApp(ui = ui, server = server)